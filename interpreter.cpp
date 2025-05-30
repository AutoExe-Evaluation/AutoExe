#include "interpreter.h"

#include "AST.h"

// EvalResult output
std::ostream& operator<<(std::ostream& out, const EvalResult& result)
{
    using Inner = std::variant<z3::expr, ExceptionType>;
    std::vector<std::pair<Inner, z3::expr>> values;
    for (const auto& [value, cond] : result)
        values.emplace_back(std::visit(overload{
            [](const std::any& a) -> Inner { return std::any_cast<z3::expr>(a); },
            [](const auto& a) -> Inner { return a; }
        }, value), std::any_cast<z3::expr>(cond));
    out << values;
    return out;
}

/**
 * Get the subtree denoted by the path
 *
 * @param root Root of the AST
 * @param path Current path
 * @return     The denoted subtree
 */
const Node* get_subtree(const Node* root, const Path& path)
{
    if (path.empty()) return root;
    Path new_path(path.begin() + 1, path.end());
    root = unwrap(root);
    if (path[0] >= root->size() || path[0] < 0) return nullptr;
    return get_subtree(root->begin()[path[0]].get(), new_path);
}

/**
 * Return the next path in the AST
 *
 * @param root          Root of the AST
 * @param path          Current path
 * @param allow_descend Whether to allow descending into the subtree
 * @return              Next sibling path if it exists
 */
std::optional<Path> next_path(const Node* root, const Path& path, bool allow_descend)
{
    if (path.empty()) return std::nullopt;
    if (auto subtree = get_subtree(root, path); allow_descend && !unwrap(subtree)->empty())
    {
        auto new_path = path;
        new_path.push_back(0);
        return new_path;
    }

    auto last = path.back();
    if (last < 0) return std::nullopt;
    Path new_path(path.begin(), std::prev(path.end()));
    if (auto subtree = get_subtree(root, new_path); subtree && last + 1 < unwrap(subtree)->size())
    {
        new_path.push_back(last + 1);
        return new_path;
    }
    return next_path(root, new_path);
}

static std::optional<NodeList> match_ast_aux(
    const Node* root, const std::vector<Kind>& symbols,
    const Path& path, NodeList& result
)
{
    auto next_p = next_path(root, path);
    if (symbols.empty()) return result;

    auto subtree = get_subtree(root, path);
    if (!subtree) return std::nullopt;

    auto detail = unwrap(subtree);
    if (subtree->kind == symbols[0] || detail->kind == symbols[0])
    {
        if (subtree->kind != symbols[0]) subtree = detail;
        result.push_back(subtree);
        if (symbols.size() == 1) return result;
        next_p = next_path(root, path, true);
        if (!next_p) return std::nullopt;
        std::vector new_symbols(symbols.begin() + 1, symbols.end());
        if (auto inner = match_ast_aux(root, new_symbols, *next_p, result))
            return inner;
        result.pop_back();
    }
    if (subtree->empty() && detail->empty())
    {
        if (!result.empty() || !next_p) return std::nullopt;
        return match_ast_aux(root, symbols, *next_p, result);
    }

    auto new_path = path;
    new_path.push_back(0);
    return match_ast_aux(root, symbols, new_path, result);
}

/**
 * Match AST elements to a list of non-terminals
 *
 * @param symbols A list of non-terminals to be matched
 * @param root    Root of the AST
 * @return        Disengaged if the tree does not match the expected structure, otherwise a list of matched nodes
 */
std::optional<NodeList> match_ast(const Node* root, const std::vector<Kind>& symbols)
{
    NodeList result;
    return match_ast_aux(root, symbols, {}, result);
}

/**
 * Substitute with a given mapping in an expression
 *
 * @param expr      The expression to be processed
 * @param ctx       Z3 context to be used
 * @param subst_map Substitution mapping
 * @return          Substituted expression
 */
z3::expr subst(const z3::expr& expr, z3::context& ctx, const std::vector<std::pair<z3::expr, z3::expr>>& subst_map)
{
    z3::expr_vector lhs_vec(ctx);
    z3::expr_vector rhs_vec(ctx);
    for (const auto& [lhs, rhs] : subst_map)
    {
        lhs_vec.push_back(lhs);
        rhs_vec.push_back(rhs);
        if (!z3::eq(lhs.get_sort(), rhs.get_sort()))
            log_error_sep("",
                          "LHS sort don't equal RHS sort!\nLHS: ", lhs, " (", lhs.get_sort(),
                          ")\nRHS: ", rhs, " (", rhs.get_sort(), ")"
            );
    }
    auto new_expr = expr;
    return new_expr.substitute(lhs_vec, rhs_vec);
}

template<TypeIndex index>
static const auto& get_value(const ProgramValue& value)
{
    return std::get<static_cast<int>(index)>(value);
}

/**
 * Make a Z3 sequence from a list of values
 *
 * @param values List of values provided
 * @param ctx    Z3 context to be used
 * @return       An expr referencing the value sequence
 */
z3::expr list_to_seq(const std::vector<ValueType>& values, z3::context& ctx)
{
    // Defaults to an integer sequence if empty
    auto elem_sort = ctx.int_sort();
    if (values.empty()) return z3::empty(ctx.seq_sort(elem_sort));
    if (values.size() == 1) return z3::to_expr(
        ctx, Z3_mk_seq_unit(ctx, to_z3_expr(values[0].get_value(), ctx))
    );

    z3::expr_vector vec{ctx};
    for (const auto& elem : values)
        vec.push_back(z3::to_expr(ctx, Z3_mk_seq_unit(ctx, to_z3_expr(elem.get_value(), ctx))));
    return z3::concat(vec);
}

z3::expr to_z3_expr(const ProgramValue& value, z3::context& ctx)
{
    switch (static_cast<TypeIndex>(value.index()))
    {
        case TypeIndex::BOOL: return ctx.bool_val(get_value<TypeIndex::BOOL>(value));
        case TypeIndex::INT: return ctx.int_val(get_value<TypeIndex::INT>(value));
        case TypeIndex::DOUBLE: return ctx.fpa_val(get_value<TypeIndex::DOUBLE>(value));
        case TypeIndex::CHAR: return ctx.int_val(get_value<TypeIndex::CHAR>(value));
        case TypeIndex::STRING: return ctx.string_val(get_value<TypeIndex::STRING>(value));
        case TypeIndex::POINTER: return ctx.int_val(reinterpret_cast<std::uint64_t>(get_value<TypeIndex::POINTER>(value))); // TODO: proper pointer type
        case TypeIndex::LIST: return list_to_seq(get_value<TypeIndex::LIST>(value), ctx);
        default: error("Invalid type index: %d\n", value.index());
    }
}

bool to_bool(const ProgramValue& value)
{
    switch (static_cast<TypeIndex>(value.index()))
    {
        case TypeIndex::BOOL: return get_value<TypeIndex::BOOL>(value);
        case TypeIndex::INT: return get_value<TypeIndex::INT>(value);
        case TypeIndex::POINTER: return get_value<TypeIndex::POINTER>(value);
        default: error("Invalid bool-like expression type: %d\n", value.index());
    }
}

[[nodiscard]] static std::vector<z3::expr> split_app(const z3::expr& expr, Z3_decl_kind kind)
{
    if (!expr.is_app() || expr.decl().decl_kind() != kind)
        return {expr};
    std::vector<z3::expr> result;
    for (const auto& child : expr.args())
        for (const auto& elem : split_app(child, kind))
            result.push_back(elem);
    return result;
}

ProgramValue from_z3_expr(const z3::expr& expr)
{
    if (expr.is_bool())
    {
        if (expr.is_true()) return true;
        if (expr.is_false()) return false;
        log_error("Unknown bool value:", expr);
    }
    if (expr.is_int()) return expr.as_int64();
    if (expr.is_fpa()) return expr.as_double();
    if (expr.is_string_value() || expr.get_sort().to_string() == "String") return expr.get_string();
    if (expr.is_seq())
    {
        std::vector<ValueType> result;
        if (expr.decl().decl_kind() == Z3_OP_SEQ_EMPTY) return result;
        auto unit = split_app(expr, Z3_OP_SEQ_CONCAT);
        for (const auto& elem : unit)
        {
            if (elem.decl().decl_kind() != Z3_OP_SEQ_UNIT)
                log_error("Wrong decl kind for subexpression:", elem);
            result.emplace_back(from_z3_expr(elem.arg(0)));
        }
        return result;
    }
    // TODO: proper handling for char and pointer
    log_error("Unknown expression type:", expr);
}