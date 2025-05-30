/**
 * Symbolic Executor
 * Trying to generate an execution trace based on concolic execution on the universal AST
 */

#ifndef SYMBOLIC_EXECUTOR_H
#define SYMBOLIC_EXECUTOR_H

#include <exception>
#include <optional>
#include <string_view>
#include <vector>

#include "AST.h"
#include "AutoBug.h"
#include "constraint-solver.h"
#include "coverage_map.h"
#include "interpreter.h"
#include "parser.h"

/**
 * Symbolic store: A set of assignments of program variables to symbolic expressions over program variables
 * Note that the design of symbolic stores is immutable
 */
class SymbolicStore : public Store<z3::expr>
{
public:
    using Store::Store;

    // Disable mutating overloads
    [[nodiscard]] auto begin() const noexcept { return env.begin(); }
    [[nodiscard]] auto end() const noexcept { return env.end(); }
    [[nodiscard]] const z3::expr& operator[](std::string_view name) const
    {
        if (auto maybe = get_variable(name))
            return env.find(*maybe)->second;
        log_error("Undefined variable:", name);
    }
    [[nodiscard]] const z3::expr& operator[](const Variable& var) const
    {
        if (auto it = env.find(var); it != env.end())
            return it->second;
        log_error("Undefined variable:", var.to_string());
    }

    // A setter to return a new store with an additional assignment
    [[nodiscard]] SymbolicStore set(const Variable& var, const z3::expr& value, z3::context& ctx) const
    {
        auto new_env = env;
        new_env.insert_or_assign(var, apply_to(value, ctx));
        return SymbolicStore{new_env};
    }

    [[nodiscard]] z3::expr apply_to(const z3::expr& onto, z3::context& ctx) const
    {
        std::vector<std::pair<z3::expr, z3::expr>> subst_map;
        for (const auto& [var, var_env] : env)
            subst_map.emplace_back(var.to_z3(ctx), var_env);
        return subst(onto, ctx, subst_map).simplify();
    }

    // Concretize: Replace symbolic variables with concrete values
    [[nodiscard]] ValueStore concretize(const ValueStore& base_store, z3::context& ctx) const
    {
        std::vector<std::pair<z3::expr, z3::expr>> subst_map;
        for (const auto& [var, var_env] : base_store)
            subst_map.emplace_back(var.to_z3(ctx), to_z3_expr(var_env.get_value(), ctx));

        std::unordered_map<Variable, ValueType> new_env;
        for (const auto& [var, expr] : env)
            new_env.insert({var, ValueType{from_z3_expr(subst(expr, ctx, subst_map).simplify())}});
        for (const auto& [var, var_env] : base_store)
            if (env.find(var) == env.end())
                new_env.insert_or_assign(var, ValueType{var_env.get_value()});
        return ValueStore{new_env};
    }
};

using SymbolicFunctionType = std::pair<EvalResult, std::vector<Trace>> (z3::context&, const std::vector<z3::expr>&, const Trace&);

/**
 * Symbolic environment: A store and a repository of registered functions.
 * Additionally, it comprises the set of path constraints
 * Note that the design of symbolic environments is immutable
 */
class SymbolicEnvironment : public Environment<SymbolicStore, SymbolicFunctionType>
{
private:
    std::vector<z3::expr> path_constraints;
    ExceptionType exception = {};

public:
    // We don't want add_default_function() from parent, roll our own.
    [[nodiscard]] SymbolicEnvironment() = default;
    [[nodiscard]] explicit SymbolicEnvironment(
        const SymbolicStore& store,
        const std::vector<z3::expr>& path_constraints = {},
        ExceptionType exception = {},
        const FunctionRepo& functions = {}
    ) : Environment(store, functions), path_constraints(path_constraints), exception(MOVE(exception))
    {
        for (auto& elem : this->path_constraints)
        {
            if (!elem.is_bool()) log_error("Path constraint", elem, "is not an boolean expression!");
            elem = elem.simplify();
        }
        add_default_functions();
    }

    void add_default_functions() override
    {
        functions["add"] = Function{
            TypeIndex::INT,
            {Variable{"a", TypeIndex::INT}, Variable{"b", TypeIndex::INT}},
            [](z3::context& ctx, const std::vector<z3::expr>& args, const Trace& current_trace) -> std::pair<EvalResult, std::vector<Trace>> {
                return {{{args[0] + args[1], ctx.bool_val(true)}}, {current_trace}};
            }
        };
        functions["free"] = Function{
            TypeIndex::VOID,
            {Variable{"p", TypeIndex::POINTER}},
            [](z3::context& ctx, const std::vector<z3::expr>& args, const Trace& current_trace) -> std::pair<EvalResult, std::vector<Trace>> {
                return {{{ctx.int_val(0), ctx.bool_val(true)}}, {current_trace}};
            }
        };
    }

    [[nodiscard]] const std::vector<z3::expr>& get_constraints() const { return path_constraints; }
    [[nodiscard]] ExceptionType get_exception() const { return exception; }

    // A setter to return a new environment with an additional assignment
    [[nodiscard]] SymbolicEnvironment set(const Variable& var, const z3::expr& value, z3::context& ctx) const
    {
        return SymbolicEnvironment{store.set(var, value, ctx), path_constraints, exception, functions};
    }
    [[nodiscard]] SymbolicEnvironment set_exception(ExceptionType new_exception) const
    {
        return SymbolicEnvironment{store, path_constraints, MOVE(new_exception), functions};
    }

    [[nodiscard]] SymbolicEnvironment add_constraint(const z3::expr& constraint, z3::context& ctx) const
    {
        if (!constraint.is_bool())
            log_error("Path constraint", constraint, "is not an boolean expression!");
        auto inner = constraint.simplify();
        std::vector<z3::expr> new_constraints;
        if (inner.is_true()) return *this;
        if (inner.is_false()) new_constraints.push_back(ctx.bool_val(false));
        else
        {
            bool added = false;
            for (const auto& elem : path_constraints)
                if (!z3::implies(constraint, elem))
                {
                    new_constraints.push_back(elem);
                    if (z3::eq(constraint, elem)) added = true;
                }
            if (!added) new_constraints.push_back(constraint);
        }
        return SymbolicEnvironment{store, new_constraints, exception, functions};
    }
    [[nodiscard]] SymbolicEnvironment add_constraints(const std::vector<z3::expr>& constraints, z3::context& ctx) const
    {
        auto new_env = *this;
        for (const auto& elem : constraints)
            new_env = new_env.add_constraint(elem, ctx);
        return new_env;
    }
    [[nodiscard]] SymbolicEnvironment add_function(const std::string& name, const Function& function) const
    {
        auto new_env = *this;
        new_env.functions[name] = function;
        return new_env;
    }

    [[nodiscard]] z3::expr constraints_expr(z3::context& ctx) const
    {
        if (path_constraints.empty()) return ctx.bool_val(true);
        if (path_constraints.size() == 1) return path_constraints[0];
        z3::expr_vector vec{ctx};
        for (const auto& elem : path_constraints)
            vec.push_back(elem);
        return z3::mk_and(vec);
    }

    [[nodiscard]] bool unsatisfiable(z3::context& ctx, const Node& node) const
    {
        if (constraints_expr(ctx).is_false()) return true;
        return false;
#if 0
        const File& file = File::get(node.id);
        auto lineno = file.lineno(node.lo);
        log_info("Generating trace on file", file.path, "at line", lineno);
        addIndex(file.path);
        TRACE T;
        T.push_back({file.path, lineno, 1, Event::EXE});
        bool prev = option_quiet; option_quiet = true;
        summarizeTrace(default_config(), T, get_file_buf_handle(), nullptr);
        option_quiet = prev;
        log_info(get_file_buf());
        return is_unsat(constraints_expr(ctx), ctx, get_file_buf());
#endif
    }

    [[nodiscard]] std::string to_string() const
    {
        auto parent = Environment::to_string();
        parent = parent.substr(0, parent.size() - 1) +\
            ", constraints: [" + ::to_string(path_constraints) + "]";
        if (exception) parent += " (with exception)";
        return parent + "}";
    }

    friend std::ostream& operator<<(std::ostream& out, const SymbolicEnvironment& env)
    {
        out << env.to_string();
        return out;
    }

    // Concretize Function: Returns empty if the path constraints are not satisfied; otherwise execute one PC
    [[nodiscard]] std::optional<ValueStore> concretize(const ValueStore& base_store, z3::context& ctx) const
    {
        std::vector<std::pair<z3::expr, z3::expr>> subst_map;
        for (const auto& [var, var_env] : base_store)
            subst_map.emplace_back(var.to_z3(ctx), to_z3_expr(var_env.get_value(), ctx));

        if (auto concrete_cond = subst(constraints_expr(ctx), ctx, subst_map);
            !is_unsat_z3(!concrete_cond, ctx)) return std::nullopt;
        return store.concretize(base_store, ctx);
    }
};

/**
 * Full concur function: Execute the program with the given constraint
 *
 * @param ses   The current symbolic environment
 * @param pc    The current program counter
 * @param sigma The current value store
 * @param ctx   Z3 context to be used
 * @return      The new value store if the path constraints are satisfiable; otherwise, an empty optional
 */
[[nodiscard]] std::optional<ValueStore> concur(
    const SymbolicEnvironment& ses, const Node* pc,
    const ValueStore& sigma, z3::context& ctx
);

/**
 * Symbolic Execution Tree (SET) Node: Environment + Next PC
 */
class SETNode
{
public:
    enum class AbruptCompletion { NONE = 0, BREAK, CONTINUE, RETURN };

private:
    EvalResult result;
    Trace trace;
    SymbolicEnvironment env;
    const Node* next_pc = nullptr;
    std::vector<std::shared_ptr<SETNode>> children;
    AbruptCompletion abrupt_completion = AbruptCompletion::NONE;

public:
    [[nodiscard]] SETNode() = default;
    [[nodiscard]] explicit SETNode(
        const EvalResult& result,
        const Trace& trace,
        const SymbolicEnvironment& env = {},
        const Node* next_pc = nullptr,
        AbruptCompletion completion = AbruptCompletion::NONE
    ) : result(result), trace(trace), env(env), next_pc(next_pc), abrupt_completion(completion)
    {}

    [[nodiscard]] const EvalResult& get_result() const { return result; }
    void set_result(const EvalResult& new_result)
    {
        result = new_result;
    }
    [[nodiscard]] const Trace& get_trace() const { return trace; }
    [[nodiscard]] const SymbolicEnvironment& get_env() const { return env; }
    [[nodiscard]] SymbolicEnvironment& get_env() { return env; }
    [[nodiscard]] const Node* get_next_pc() const { return next_pc; }
    [[nodiscard]] std::vector<std::shared_ptr<SETNode>>& get_children() { return children; }
    void set_children(std::vector<std::shared_ptr<SETNode>> new_children)
    {
        children = MOVE(new_children);
    }
    [[nodiscard]] AbruptCompletion get_abrupt_completion() const { return abrupt_completion; }
    void set_abrupt_completion(AbruptCompletion new_completion)
    {
        abrupt_completion = new_completion;
    }

    // iterator & accessor
    [[nodiscard]] auto begin() { return children.begin(); }
    [[nodiscard]] auto begin() const { return children.begin(); }
    [[nodiscard]] auto end() { return children.end(); }
    [[nodiscard]] auto end() const { return children.end(); }
    [[nodiscard]] auto size() const { return children.size(); }
    [[nodiscard]] bool empty() const { return children.empty(); }
    [[nodiscard]] SETNode* operator[](std::size_t index)
    {
        if (index >= size()) error("Index %d out of range! (size = %d)\n", index, size());
        return children[index].get();
    }
    [[nodiscard]] const SETNode* operator[](std::size_t index) const
    {
        if (index >= size()) error("Index %d out of range! (size = %d)\n", index, size());
        return children[index].get();
    }
    void set(std::size_t index, SETNode node)
    {
        if (index >= size()) error("Index %d out of range! (size = %d)\n", index, size());
        children[index] = std::make_shared<SETNode>(MOVE(node));
    }

    [[nodiscard]] std::string to_string() const
    {
        return "{result: " + ::to_string(result) + ", " + env.to_string() +\
            ", next_pc: " + (next_pc ? next_pc->str() : "null") + "} " + ::to_string(trace);
    }

    friend std::ostream& operator<<(std::ostream& out, const SETNode& node)
    {
        out << node.to_string();
        return out;
    }
};

/**
 * Display a SET
 *
 * @param root  The root of the tree to display
 * @param ctx   Z3 context to be used
 * @param level Current indentation level
 */
void display_set(const SETNode* root, z3::context& ctx, int level = 0);

// Symbolic Interpreter
// First, an exception type for zero division/modulo
// TODO: No other exception is currently handled
class ZeroDivisionException : public std::exception
{
public:
    enum class Type { DIV, MOD };
    Type type;

    [[nodiscard]] explicit ZeroDivisionException(Type type) : type(type) {}

    [[nodiscard]] const char* what() const noexcept override
    {
        static const std::unordered_map<Type, std::string> reasons = {
            {Type::DIV, "Division by zero"},
            {Type::MOD, "Modulo by zero"}
        };
        auto it = reasons.find(type);
        if (it == reasons.end()) return "Unknown reason";
        return it->second.c_str();
    }
};
[[nodiscard]] inline ExceptionType DivByZeroException()
{
    static ZeroDivisionException exception{ZeroDivisionException::Type::DIV};
    return &exception;
}
[[nodiscard]] inline ExceptionType ModByZeroException()
{
    static ZeroDivisionException exception{ZeroDivisionException::Type::MOD};
    return &exception;
}

template<EvalResultType index>
auto get_value(const std::variant<std::any, ExceptionType>& value)
{
    if constexpr (index == EvalResultType::VALUE)
        return std::any_cast<z3::expr>(std::get<static_cast<int>(index)>(value));
    else return std::get<static_cast<int>(index)>(value);
}

[[nodiscard]] static std::vector<SETNode*> get_leaves(SETNode& root)
{
    if (root.empty()) return {&root};

    std::vector<SETNode*> leaves;
    for (const auto& child : root)
        for (auto leaf : get_leaves(*child))
            leaves.push_back(leaf);
    return leaves;
}

// Tree operations
[[nodiscard]] static bool tree_contains(const SETNode& tree, const SETNode& node)
{
    if (&tree == &node) return true;
    for (const auto& child : tree)
        if (tree_contains(*child, node))
            return true;
    return false;
}

static void tree_path_iterator_aux(
    SETNode* root, std::vector<std::pair<Path, SETNode*>>& path, Path current_path
)
{
    path.emplace_back(current_path, root);
    ssize_t cnt = 0;
    for (const auto& child : *root)
    {
        current_path.push_back(cnt); ++cnt;
        tree_path_iterator_aux(child.get(), path, current_path);
        current_path.pop_back();
    }
}

[[nodiscard]] static std::vector<std::pair<Path, SETNode*>> tree_path_iterator(SETNode& root)
{
    std::vector<std::pair<Path, SETNode*>> path;
    tree_path_iterator_aux(&root, path, {});
    return path;
}

[[nodiscard]] static SETNode tree_replace(SETNode* tree, SETNode* before, SETNode after)
{
    for (const auto& [path, child] : tree_path_iterator(*tree))
        if (child == before)
        {
            // In-place replacement
            if (path.empty()) return after;
            SETNode* cur_parent = tree;
            auto last_index = path[0];
            SETNode* cur_tree = (*cur_parent)[last_index];
            Path cur_path(path.begin() + 1, path.end());
            while (!cur_path.empty())
            {
                last_index = cur_path[0];
                cur_path.erase(cur_path.begin());
                cur_parent = cur_tree;
                cur_tree = (*cur_tree)[last_index];
            }
            cur_parent->set(last_index, after);
            return *tree;
        }
    log_error("Node", before, "not found in tree");
}

/**
 * Handle unary operation results.
 *
 * @param op    The unary operation to handle
 * @param inner Inner operand
 * @param ctx   Z3 context to be used
 * @return      Evaluation result
 */
[[nodiscard]] EvalResult handle_unary_op(std::string_view op, const EvalResult& inner, z3::context& ctx);

/**
 * Handle binary operation results.
 *
 * @param op    The binary operation to handle
 * @param left  Left operand
 * @param right Right operand
 * @param ctx   Z3 context to be used
 * @return      Evaluation result
 */
[[nodiscard]] EvalResult handle_binary_op(std::string_view op, const EvalResult& left, const EvalResult& right, z3::context& ctx);

/**
 * Handle ternary operation results.
 *
 * @param cond_node The condition part of the ternary operator
 * @param then_node Then operand
 * @param else_node Else operand
 * @param ctx       Z3 context to be used
 * @return          Evaluation result
 */
[[nodiscard]] EvalResult handle_ternary_op(const EvalResult& cond_node, const EvalResult& then_node, const EvalResult& else_node, z3::context& ctx);

class SymbolicStmtVisitor;
class SymbolicExprVisitor;
class SymbolicInterpreter;

/**
 * Handle assignment to variables.
 *
 * @param parent        Parent interpreter
 * @param env           Current symbolic environment
 * @param ctx           Z3 context to be used
 * @param stmt          Assignment/declaration statement
 * @param left_node     Variable assigned to
 * @param right_ptr     Value to be assigned (can be Empty)
 * @param current_trace Current trace
 * @param is_element    True if the variable refers to an element
 * @return              Evaluation result
 */
[[nodiscard]] SETNode handle_assign(SymbolicInterpreter& parent, SymbolicEnvironment& env, z3::context& ctx, const Node& stmt, const Chunk* left_node, const Node* right_ptr, const Trace& current_trace, bool is_element = false);

/**
 * Symbolic interpreter for the universal AST
 */
class SymbolicInterpreter
{
private:
    std::unique_ptr<SymbolicStmtVisitor> stmt_visitor;
    std::unique_ptr<SymbolicExprVisitor> expr_visitor;

public:
    [[nodiscard]] explicit SymbolicInterpreter(z3::context& ctx, const SymbolicEnvironment& env = {});
    [[nodiscard]] SymbolicInterpreter(const SymbolicInterpreter& other);
    [[nodiscard]] SymbolicInterpreter& operator=(const SymbolicInterpreter& other) &;
    [[nodiscard]] SymbolicInterpreter(SymbolicInterpreter&& other) noexcept;
    [[nodiscard]] SymbolicInterpreter& operator=(SymbolicInterpreter&& other) & noexcept;

    // execute = statement, evaluate = expression
    SETNode execute(const Node* stmt, const Trace& current_trace) const;
    SETNode evaluate(const Node* expr, const Trace& current_trace) const;
    [[nodiscard]] const SymbolicEnvironment& get_environment() const;
    void set_environment(const SymbolicEnvironment& env) const;
};

// Symbolic Visitor (std::any is always SETNode)
class SymbolicStmtVisitor : public NodeVisitor<std::any>
{
private:
    z3::context* ctx = nullptr;
    SymbolicEnvironment env;
    SymbolicInterpreter& parent;

    [[nodiscard]] SETNode handle_loop(const Node& loop_stmt, const Trace& current_trace) const
    {
        auto cond_expr = loop_stmt.getCond()->get();
        auto body = loop_stmt.getBody()->get();
        auto eval_result = parent.evaluate(cond_expr, current_trace);
#ifdef PYTHON_PARSER
        if (eval_result.get_result().empty() && dynamic_cast<const For*>(&loop_stmt) != nullptr)
        {
            eval_result.set_result({{ctx->bool_const("__for_range"), ctx->bool_val(true)}});
        }
#endif
        SETNode node{eval_result.get_result(), current_trace, env, &loop_stmt};
        node.set_children(MOVE(eval_result.get_children()));

        for (const auto& [value_wrap, cond_wrap] : eval_result.get_result())
        {
            auto cond = std::any_cast<z3::expr>(cond_wrap);
            auto new_env = env.add_constraint(cond, *ctx);
            if (new_env.unsatisfiable(*ctx, loop_stmt)) continue;

            if (EvalResultType{value_wrap.index()} == EvalResultType::EXCEPTION)
            {
                node.get_children().push_back(std::make_shared<SETNode>(
                    EvalResult{}, current_trace, new_env.set_exception(get_value<EvalResultType::EXCEPTION>(value_wrap))
                ));
                continue;
            }

            auto value = get_value<EvalResultType::VALUE>(value_wrap);
            auto then_env = new_env.add_constraint(value.simplify(), *ctx);
            if (then_env.unsatisfiable(*ctx, *body))
                log_error("Unable to determine suitable loop continuation for", value);

            Trace new_trace = current_trace;
            if (body->kind != BLOCK) new_trace.addTrace(*body);
            SymbolicInterpreter then_int{*ctx, then_env};
            auto body_result = then_int.execute(body, new_trace);
            node.get_children().push_back(std::make_shared<SETNode>(body_result));
            node.get_children().push_back(std::make_shared<SETNode>(EvalResult{}, current_trace, env));
        }
        return node;
    }

public:
    [[nodiscard]] explicit SymbolicStmtVisitor(SymbolicInterpreter& parent) : parent(parent) {}
    [[nodiscard]] const SymbolicEnvironment& get_environment() const { return env; }
    void set_environment(const SymbolicEnvironment& env) { this->env = env; }
    [[nodiscard]] z3::context* get_context() const { return ctx; }
    void set_context(z3::context& ctx) { this->ctx = &ctx; }

    std::any default_value(const Trace& current_trace) override
    {
        return SETNode{{}, current_trace, env};
    }

    std::any apply(const IfThenElse& if_stmt, const Trace& current_trace) override
    {
        // TODO: handling Elif
        auto cond_expr = if_stmt.getCond()->get();
        auto body = if_stmt.child[1].get();
        auto else_body = if_stmt.child.back().get();
        auto eval_result = parent.evaluate(cond_expr, current_trace);
        SETNode node{eval_result.get_result(), current_trace, env, &if_stmt};
        node.set_children(MOVE(eval_result.get_children()));

        for (const auto& [value_wrap, cond_wrap] : eval_result.get_result())
        {
            auto cond = std::any_cast<z3::expr>(cond_wrap);
            auto new_env = env.add_constraint(cond, *ctx);
            if (new_env.unsatisfiable(*ctx, if_stmt)) continue;

            if (EvalResultType{value_wrap.index()} == EvalResultType::EXCEPTION)
            {
                node.get_children().push_back(std::make_shared<SETNode>(
                    EvalResult{}, current_trace, new_env.set_exception(get_value<EvalResultType::EXCEPTION>(value_wrap))
                ));
                continue;
            }

            auto value = get_value<EvalResultType::VALUE>(value_wrap);
            auto then_env = new_env.add_constraint(value.simplify(), *ctx);
            auto else_env = new_env.add_constraint((!value).simplify(), *ctx);
            auto then_unsat = then_env.unsatisfiable(*ctx, *body);
            auto else_unsat = else_env.unsatisfiable(*ctx, *else_body);
            if (then_unsat && else_unsat)
                log_error("Unable to determine suitable continuation for", value);
            if (!then_unsat)
            {
                Trace new_trace = current_trace;
                if (body->kind != BLOCK && body->kind != EMPTY) new_trace.addTrace(*body);
                SymbolicInterpreter then_int{*ctx, then_env};
                node.get_children().push_back(std::make_shared<SETNode>(then_int.execute(body, new_trace)));
            }
            if (!else_unsat)
            {
                Trace new_trace = current_trace;
                if (else_body->kind != BLOCK && else_body->kind != EMPTY) new_trace.addTrace(*else_body);
                SymbolicInterpreter else_int{*ctx, else_env};
                node.get_children().push_back(std::make_shared<SETNode>(else_int.execute(else_body, new_trace)));
            }
        }
        return node;
    }

    std::any apply(const While& while_stmt, const Trace& current_trace) override
    {
        return handle_loop(while_stmt, current_trace);
    }

    std::any apply(const For& for_stmt, const Trace& current_trace) override
    {
        return handle_loop(for_stmt, current_trace);
    }

    std::any apply(const Break& break_stmt, const Trace& current_trace) override
    {
        return SETNode{{}, current_trace, env, nullptr, SETNode::AbruptCompletion::BREAK};
    }

    std::any apply(const Continue& continue_stmt, const Trace& current_trace) override
    {
        return SETNode{{}, current_trace, env, nullptr, SETNode::AbruptCompletion::CONTINUE};
    }

    std::any apply(const Block& block, const Trace& current_trace) override
    {
        std::vector<const Node*> stmts;
        for (const auto& stmt : block)
        {
            if (stmt->kind == COMMENT) continue;
#ifdef PYTHON_PARSER
            if (auto for_loop = dynamic_cast<const For*>(stmt.get()))
            {
                stmts.push_back(for_loop->child[0].get());
                stmts.push_back(for_loop);
                continue;
            }
#endif
            stmts.push_back(stmt.get());
        }
        if (stmts.empty()) return default_value(current_trace);

        std::optional<SETNode> result;
        std::vector<SETNode*> children;

        for (auto stmt : stmts)
        {
            if (!result)
            {
                Trace new_trace = current_trace;
                new_trace.addTrace(*stmt);
                result = parent.execute(stmt, new_trace);
                children = get_leaves(*result);
                continue;
            }

            auto completion = SETNode::AbruptCompletion::NONE;
            for (auto child : children)
            {
                if (child->get_env().get_exception()) // || child->get_env().unsatisfiable(*ctx, *stmt))
                    continue;
                SymbolicInterpreter child_int{*ctx, child->get_env()};
                Trace new_trace = child->get_trace();
                new_trace.addTrace(*stmt);
                auto subtree = child_int.execute(stmt, new_trace);
                result = tree_replace(&*result, child, subtree);
                completion = std::max(completion, subtree.get_abrupt_completion());
                if (completion != SETNode::AbruptCompletion::NONE) break;
            }
            if (completion == SETNode::AbruptCompletion::BREAK || completion == SETNode::AbruptCompletion::RETURN)
                break;
            if (completion == SETNode::AbruptCompletion::CONTINUE)
                continue;
            children = get_leaves(*result);
        }
        if (!result) log_error("Invalid block:", block.str());
        return *result;
    }

    std::any apply(const Expr& expr, const Trace& current_trace) override
    {
        if (!expr.detail) return default_value(current_trace);
        auto inner = parent.evaluate(expr.detail.get(), current_trace);
        inner.set_result({});
        return inner;
    }

    std::any apply(const Decl& decl, const Trace& current_trace) override
    {
        if (decl.empty()) return default_value(current_trace);
        auto current_env = env;
        for (const auto& [var_chunk, index] : decl.vars)
        {
            auto right = decl.child[index].get();
            if (right->kind == EMPTY) continue;
            auto result = handle_assign(parent, current_env, *ctx, decl, &var_chunk, right, current_trace, decl.is_python_for_range);
            current_env = result.get_children().back()->get_env();
        }
        return SETNode{{}, current_trace, current_env};
    }

    std::any apply(const Return& return_stmt, const Trace& current_trace) override
    {
        auto expr = parent.evaluate(return_stmt.begin()->get(), current_trace);
        auto new_trace = expr.get_trace();
        new_trace.addTrace(return_stmt, Event::RET);
        return SETNode{expr.get_result(), new_trace, env, nullptr, SETNode::AbruptCompletion::RETURN};
    }

    std::any apply(const Func& func, const Trace& current_trace) override
    {
        // FIXME: for now, all params and return type are assumed to be int
        auto func_name = func.name.str();
        std::vector<Variable> params;
        std::size_t cnt = 0;
        for (const auto& param : func)
        {
            if (cnt + 1 == func.size()) break;
            ++cnt;
            auto full_name = param->str();
            full_name = full_name.substr(full_name.rfind(' ') + 1);
            auto type = TypeIndex::INT;
            if (full_name[0] == '*')
            {
                type = TypeIndex::POINTER;
                full_name = full_name.substr(full_name.rfind('*') + 1);
            }
            params.emplace_back(full_name, type);
        }

        return SETNode{{}, current_trace, env.add_function(
            func_name, {
                TypeIndex::INT, params,
                [this, params = MOVE(params), &func, func_name](z3::context& ctx, const std::vector<z3::expr>& args, const Trace& current_trace) mutable -> std::pair<EvalResult, std::vector<Trace>> {
                    if (params.size() != args.size())
                        log_error("Function", func_name, "expects", params.size(), "arguments, but got", args.size());
                    for (std::size_t i = 0; i < params.size(); ++i)
                        env = env.set(params[i], args[i], ctx);
                    parent.set_environment(env);

                    auto new_trace = current_trace;
                    new_trace.addTrace(func, Event::CLL);
                    auto block_result = parent.execute(func.getBody()->get(), new_trace);
                    EvalResult result;
                    std::vector<Trace> traces;
                    for (auto node : get_leaves(block_result))
                    {
                        if (node->get_abrupt_completion() != SETNode::AbruptCompletion::RETURN)
                            log_error("No return statement found in function", func_name);
                        auto constraint = node->get_env().constraints_expr(ctx);
                        for (const auto& [value_wrap, cond_wrap] : node->get_result())
                        {
                            result.emplace_back(value_wrap, (constraint && std::any_cast<z3::expr>(cond_wrap)).simplify());
                            traces.push_back(node->get_trace());
                        }
                    }
                    return {result, traces};
                }
            }
        )};
    }
};

class SymbolicExprVisitor : public NodeVisitor<std::any>
{
private:
    z3::context* ctx = nullptr;
    SymbolicEnvironment env;
    SymbolicInterpreter& parent;

    template<typename F>
    [[nodiscard]] SETNode handle_call(const Node& expr, const Trace& current_trace, std::string_view func_name, const F& func, std::size_t arg_num)
    {
        std::vector<std::shared_ptr<SETNode>> children;
        std::vector<EvalResult> args_result;
        for (const auto& child : expr)
        {
            auto eval_result = parent.evaluate(child.get(), current_trace);
            args_result.push_back(eval_result.get_result());
            for (auto& inner_child : eval_result.get_children())
                children.push_back(MOVE(inner_child));
        }
        SETNode node{{}, current_trace, env};
        node.set_children(MOVE(children));
        auto leaves = get_leaves(node);

        // Try to calculate all possible combinations; exit early on exception
        EvalResult result;
        auto combinations = cartesian_product(args_result);
        for (const auto& combination : combinations)
        {
            std::vector<z3::expr> values;
            auto path_constraints = ctx->bool_val(true);
            for (const auto& [value_wrap, cond_wrap] : combination)
            {
                path_constraints = (path_constraints && std::any_cast<z3::expr>(cond_wrap)).simplify();
                if (EvalResultType{value_wrap.index()} == EvalResultType::EXCEPTION)
                {
                    result.emplace_back(get_value<EvalResultType::EXCEPTION>(value_wrap), path_constraints);
                    break;
                }

                auto value = get_value<EvalResultType::VALUE>(value_wrap);
                values.push_back(value);
            }

            if (values.size() != combination.size())
                continue;
            if (values.size() != arg_num)
                log_error_sep("", "Mismatch # of params for function ", func_name,
                              ": Expected ", arg_num, " arguments, but ",
                              values.size(), " found in expression ", expr.str());

            for (auto leaf : leaves)
            {
                const auto& [func_result, traces] = func(*ctx, values, leaf->get_trace());
                for (int i = 0; i < func_result.size(); ++i)
                {
                    const auto& [elem_value, elem_cond] = func_result[i];
                    result.emplace_back(elem_value, (std::any_cast<z3::expr>(elem_cond) && path_constraints).simplify());
                    EvalResult single{{elem_value, (std::any_cast<z3::expr>(elem_cond) && path_constraints).simplify()}};
                    node = tree_replace(&node, leaf, SETNode{single, traces[i], env});
                    children.push_back(std::make_shared<SETNode>(single, traces[i], env));
                }
            }
        }
        node.set_result(result);
        return node;
    }

public:
    [[nodiscard]] explicit SymbolicExprVisitor(SymbolicInterpreter& parent) : parent(parent) {}
    [[nodiscard]] const SymbolicEnvironment& get_environment() const { return env; }
    void set_environment(const SymbolicEnvironment& env) { this->env = env; }
    [[nodiscard]] z3::context* get_context() const { return ctx; }
    void set_context(z3::context& ctx) { this->ctx = &ctx; }

    std::any default_value(const Trace& current_trace) override
    {
        return SETNode{{}, current_trace, env};
    }

    std::any apply(const Identifier& identifier, const Trace& current_trace) override
    {
        return SETNode{{{env.get_store()[identifier.str()], ctx->bool_val(true)}}, current_trace, env};
    }

    std::any apply(const NumberLiteral& number, const Trace& current_trace) override
    {
        auto result = parse_number(number.str());
        if (result.index() == 0)
            return SETNode{{{ctx->int_val(std::get<0>(result)), ctx->bool_val(true)}}, current_trace, env};
        return SETNode{{{ctx->fpa_val(std::get<1>(result)), ctx->bool_val(true)}}, current_trace, env};
    }

    std::any apply(const BooleanLiteral& boolean, const Trace& current_trace) override
    {
        bool value;
        auto str = boolean.str();
#ifdef PYTHON_PARSER
        if (str == "True") value = true;
        else if (str == "False") value = false;
#else
        if (str == "true") value = true;
        else if (str == "false") value = false;
#endif
        else log_error("Invalid boolean literal:", str);
        return SETNode{{{ctx->bool_val(value), ctx->bool_val(true)}}, current_trace, env};
    }

    std::any apply(const StringLiteral& str, const Trace& current_trace) override
    {
        auto inner = str.str();
        if (inner.size() < 2 || ((inner[0] != '\"' || inner.back() != '\"')
#ifdef PYTHON_PARSER
                             && (inner[0] != '\'' || inner.back() != '\'')
#endif
        ))
            log_error("Invalid string literal:", inner);
        inner = unescape(inner.substr(1, inner.size() - 2));
        return SETNode{{{ctx->string_val(inner), ctx->bool_val(true)}}, current_trace, env};
    }

    std::any apply(const NullLiteral& null, const Trace& current_trace) override
    {
#ifdef PYTHON_PARSER
        if (null.str() != "None")
#elif defined(JAVA_PARSER)
        if (null.str() != "null")
#else
        if (null.str() != "NULL")
#endif
            log_error("Invalid null literal:", null.str());
        return SETNode{{{ctx->int_val(0), ctx->bool_val(true)}}, current_trace, env};
    }

    std::any apply(const CharLiteral& ch, const Trace& current_trace) override
    {
        auto inner = ch.str();
        if (inner.size() < 2 || inner[0] != '\'' || inner.back() != '\'')
            log_error("Invalid char literal:", inner);
        inner = unescape(inner.substr(1, inner.size() - 2), true);
        if (inner.size() > 2 || (inner.size() == 2 && inner[0] != '\\'))
            log_error("Multi-character char literal:", inner);
        return SETNode{{{ctx->int_val(inner[0]), ctx->bool_val(true)}}, current_trace, env};
    }

#ifdef PYTHON_PARSER
    std::any apply(const ListLiteral& list, const Trace& current_trace) override
    {
        auto inner = list.str();
        if (inner.size() < 2 || inner[0] != '[' || inner.back() != ']')
            log_error("Invalid list literal:", inner);

        if (list.empty())
        {
            std::vector<z3::sort> sorts;
            z3::func_decl_vector projs(*ctx);
            // TODO: handle this better instead of assuming int
            auto inner_sort = ctx->string_sort();
            return SETNode{{{z3::empty(ctx->seq_sort(inner_sort)), ctx->bool_val(true)}}, current_trace, env};
        }

        return handle_call(list, current_trace, "__list", [](z3::context& ctx, const std::vector<z3::expr>& exprs, const Trace& trace) -> std::pair<EvalResult, std::vector<Trace>> {
            z3::expr_vector args(ctx);
            for (int i = 0; i < exprs.size(); ++i) args.push_back(exprs[i].unit());
            return {{{concat(args), ctx.bool_val(true)}}, {trace}};
        }, list.size());
    }

    std::any apply(const SliceLiteral& slice, const Trace& current_trace) override
    {
        log_error("Bare slice literal:", slice.str());
    }
#endif

    std::any apply(const Assignment& assign, const Trace& current_trace) override
    {
        return handle_assign(parent, env, *ctx, assign, assign.child[0].get(), assign.child[1].get(), current_trace);
    }

    std::any apply(const Unary& unary, const Trace& current_trace) override
    {
        auto inner = parent.evaluate(unary.child[0].get(), current_trace);
        if (inner.get_result().empty())
            log_error("Unary operand", inner, "is empty!");
        auto result = handle_unary_op(unary.op, inner.get_result(), *ctx);
        SETNode node{result, current_trace, env};
        node.set_children(MOVE(inner.get_children()));
        return node;
    }

    std::any apply(const Binary& binary, const Trace& current_trace) override
    {
        auto left = parent.evaluate(binary.child[0].get(), current_trace);
        auto right = parent.evaluate(binary.child[1].get(), current_trace);
        if (left.get_result().empty())
            log_error("Binary left operand", left, "is empty!");
        if (right.get_result().empty())
            log_error("Binary right operand", right, "is empty!");
        auto result = handle_binary_op(binary.op, left.get_result(), right.get_result(), *ctx);
        auto children = MOVE(left.get_children());
        for (auto& elem : right.get_children())
            children.push_back(MOVE(elem));
        SETNode node{result, current_trace, env};
        node.set_children(MOVE(children));
        return node;
    }

    std::any apply(const Ternary& ternary, const Trace& current_trace) override
    {
        auto cond_node = parent.evaluate(ternary.getCond()->get(), current_trace);
        auto then_node = parent.evaluate(ternary.child[1].get(), current_trace);
        auto else_node = parent.evaluate(ternary.child[2].get(), current_trace);
        if (cond_node.get_result().empty())
            log_error("Ternary cond operand", cond_node, "is empty!");
        if (then_node.get_result().empty())
            log_error("Ternary then operand", then_node, "is empty!");
        if (else_node.get_result().empty())
            log_error("Ternary else operand", else_node, "is empty!");
        auto result = handle_ternary_op(cond_node.get_result(), then_node.get_result(), else_node.get_result(), *ctx);
        auto children = MOVE(cond_node.get_children());
        for (auto& elem : then_node.get_children())
            children.push_back(MOVE(elem));
        for (auto& elem : else_node.get_children())
            children.push_back(MOVE(elem));
        SETNode node{result, current_trace, env};
        node.set_children(MOVE(children));
        return node;
    }

    std::any apply(const Subscript& subscript, const Trace& current_trace) override
    {
        auto arg = parent.evaluate(subscript.child[0].get(), current_trace);
        auto indexChild = subscript.child[1].get();
#ifdef PYTHON_PARSER
        SETNode index;
        // TODO: handle slices better
        if (auto slice = dynamic_cast<const SliceLiteral*>(indexChild))
            index = arg;
        else index = parent.evaluate(indexChild, current_trace);
#else
        auto index = parent.evaluate(indexChild, current_trace);
#endif
        if (arg.get_result().empty())
            log_error("Subscript argument operand", arg, "is empty!");
        if (index.get_result().empty())
            log_error("Subscript index operand", index, "is empty!");
        auto result = handle_binary_op("[]", arg.get_result(), index.get_result(), *ctx);
        auto children = MOVE(arg.get_children());
        for (auto& elem : index.get_children())
            children.push_back(MOVE(elem));
        SETNode node{result, current_trace, env};
        node.set_children(MOVE(children));
        return node;
    }

    std::any apply(const Expr& expr, const Trace& current_trace) override
    {
        if (!expr.detail) return default_value(current_trace);
        return parent.evaluate(expr.detail.get(), current_trace);
    }

    std::any apply(const Call& expr, const Trace& current_trace) override
    {
        std::vector<std::shared_ptr<SETNode>> children;
        auto func_name = expr.func.str();
        if (!env.contains(func_name)) return default_value(current_trace);
        auto func = env[func_name];
        return handle_call(expr, current_trace, func_name, func.function, func.arguments.size());
    }
};

#endif //SYMBOLIC_EXECUTOR_H
