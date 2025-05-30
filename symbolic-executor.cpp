#include <map>

#include "value_interpreter.h"
#include "symbolic-executor.h"

/**
 * Full concur function: Execute the program with the given constraint
 *
 * @param ses   The current symbolic environment
 * @param pc    The current program counter
 * @param sigma The current value store
 * @param ctx   Z3 context to be used
 * @return      The new value store if the path constraints are satisfiable; otherwise, an empty optional
 */
std::optional<ValueStore> concur(
    const SymbolicEnvironment& ses, const Node* pc,
    const ValueStore& sigma, z3::context& ctx
)
{
    auto sigma_1 = ses.concretize(sigma, ctx);
    if (!sigma_1) return std::nullopt;

    ValueEnvironment env{*sigma_1};
    ValueInterpreter interpreter{env};
    interpreter.execute(pc);
    return env.get_store();
}

/**
 * Display a SET
 *
 * @param root  The root of the tree to display
 * @param ctx   Z3 context to be used
 * @param level Current indentation level
 */
void display_set(const SETNode* root, z3::context& ctx, int level)
{
    if (!root) return;
    std::string indent;
    for (int i = 0; i < level; ++i) indent += "  ";
    info("%s", indent.c_str());
    std::string result = "<" + ::to_string(root->get_result()) + "> ";
    bool first = true;
    std::map<std::string, std::pair<const Variable*, const z3::expr*>> store;
    for (const auto& [var, value]: root->get_env().get_store())
        store.emplace(var.get_name(), std::pair{&var, &value});
    for (const auto& [name, var_pair]: store)
    {
        const auto& [var, value] = var_pair;
        if (first) first = false;
        else result += ", ";
        result += std::string{var->get_name()} + " = " + value->simplify().to_string();
    }
    result += " [" + ::to_string(root->get_env().constraints_expr(ctx).simplify()) + "] ";
    if (root->get_env().get_exception()) result += "(with exception) ";
    result += ::to_string(root->get_trace()) + " ";
    result += root->get_next_pc() ? root->get_next_pc()->str() : "null";
    if (root->get_next_pc() == nullptr) COVERAGE_MAP.insert(root->get_trace());
    info("%s\n", result.c_str());

    int cnt = 0;
    for (const auto& child : *root)
    {
        info("%s|-[Child %d]\n", indent.c_str(), cnt);
        ++cnt;
        if (child == nullptr)
        {
            info("%s  <NULL>\n", indent.c_str());
            continue;
        }
        display_set(child.get(), ctx, level + 1);
    }
}

/**
 * Handle unary operation results.
 *
 * @param op    The unary operation to handle
 * @param inner Inner operand
 * @param ctx   Z3 context to be used
 * @return      Evaluation result
 */
EvalResult handle_unary_op(std::string_view op, const EvalResult& inner, z3::context& ctx)
{
    EvalResult result;

    for (const auto& [inner_val, inner_cond_wrap] : inner)
    {
        auto inner_cond = std::any_cast<z3::expr>(inner_cond_wrap);
        if (EvalResultType{inner_val.index()} == EvalResultType::EXCEPTION)
        {
            result.emplace_back(inner_val, inner_cond);
            continue;
        }
        const z3::expr& inner_value = get_value<EvalResultType::VALUE>(inner_val);

        // Logical operators
        if (op == "!" || op == "not")
            result.emplace_back(!inner_value, inner_cond);

        // Arithmetic operators
        if (op == "+") { result.emplace_back(inner_value, inner_cond); continue; }
        if (op == "-") { result.emplace_back(-inner_value, inner_cond); continue; }

        // Bitwise operators
        if (op == "~") { result.emplace_back(~inner_value, inner_cond); continue; }
    }

    return result;
}

/**
 * Handle binary operation results.
 *
 * @param op    The binary operation to handle
 * @param left  Left operand
 * @param right Right operand
 * @param ctx   Z3 context to be used
 * @return      Evaluation result
 */
EvalResult handle_binary_op(std::string_view op, const EvalResult& left, const EvalResult& right, z3::context& ctx)
{
    EvalResult result;

    for (const auto& [left_val, left_cond_wrap] : left)
    {
        auto left_cond = std::any_cast<z3::expr>(left_cond_wrap);
        if (EvalResultType{left_val.index()} == EvalResultType::EXCEPTION)
        {
            result.emplace_back(left_val, left_cond);
            continue;
        }
        const z3::expr& left_value = get_value<EvalResultType::VALUE>(left_val);

        // Logical operators
        if (op == "&&" || op == "and")
            result.emplace_back(ctx.bool_val(false), (left_cond && !left_value).simplify());
        if (op == "||" || op == "or")
            result.emplace_back(ctx.bool_val(true), (left_cond && left_value).simplify());

        for (const auto& [right_val, right_cond_wrap] : right)
        {
            auto right_cond = std::any_cast<z3::expr>(right_cond_wrap);
            auto cond_and = (left_cond && right_cond).simplify();
            if (cond_and.is_false()) continue;

            if (EvalResultType{right_val.index()} == EvalResultType::EXCEPTION)
            {
                // Short-circuit for logic operators
                if (op == "&&" || op == "and") cond_and = cond_and && !left_value;
                else if (op == "||" || op == "or") cond_and = cond_and && left_value;
                cond_and = cond_and.simplify();
                if (!cond_and.is_false())
                    result.emplace_back(right_val, cond_and);
                continue;
            }
            const z3::expr& right_value = get_value<EvalResultType::VALUE>(right_val);

            // Logical operators, part 2
            if (op == "&&" || op == "and")
            {
                cond_and = (cond_and && left_value).simplify();
                if (!cond_and.is_false())
                    result.emplace_back(right_val, cond_and);
                continue;
            }
            if (op == "||" || op == "or")
            {
                cond_and = (cond_and && !left_value).simplify();
                if (!cond_and.is_false())
                    result.emplace_back(right_val, cond_and);
                continue;
            }

            // Arithmetic operators
            std::optional<z3::expr> right_zero_opt;
            if (right_value.is_numeral()) right_zero_opt = (right_value == ctx.int_val(0));
            else right_zero_opt = ctx.bool_val(false);
            auto right_zero = *right_zero_opt;
            auto right_nonzero = !right_zero;
            if (op == "+") { result.emplace_back(left_value + right_value, cond_and); continue; }
            if (op == "-") { result.emplace_back(left_value - right_value, cond_and); continue; }
            if (op == "*") { result.emplace_back(left_value * right_value, cond_and); continue; }
            if (op == "/")
            {
                result.emplace_back(left_value / right_value, cond_and && right_nonzero);
                result.emplace_back(DivByZeroException(), cond_and && right_zero);
                continue;
            }
            if (op == "%")
            {
                result.emplace_back(left_value % right_value, cond_and && right_nonzero);
                result.emplace_back(ModByZeroException(), cond_and && right_zero);
                continue;
            }

            // Comparison operators
            if (op == "==") { result.emplace_back(left_value == right_value, cond_and); continue; }
            if (op == "!=") { result.emplace_back(left_value != right_value, cond_and); continue; }
            if (op == "<") { result.emplace_back(left_value < right_value, cond_and); continue; }
            if (op == ">") { result.emplace_back(left_value > right_value, cond_and); continue; }
            if (op == "<=") { result.emplace_back(left_value <= right_value, cond_and); continue; }
            if (op == ">=") { result.emplace_back(left_value >= right_value, cond_and); continue; }

            // Bitwise operators
            if (op == "&") { result.emplace_back(left_value & right_value, cond_and); continue; }
            if (op == "|") { result.emplace_back(left_value | right_value, cond_and); continue; }

            // Subscription
            if (op == "[]") { result.emplace_back(left_value[right_value], cond_and); continue; }
        }
    }

    return result;
}

/**
 * Handle ternary operation results.
 *
 * @param cond_node The condition part of the ternary operator
 * @param then_node Then operand
 * @param else_node Else operand
 * @param ctx       Z3 context to be used
 * @return          Evaluation result
 */
EvalResult handle_ternary_op(const EvalResult& cond_node, const EvalResult& then_node, const EvalResult& else_node, z3::context& ctx)
{
    // TODO
    return {};
}

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
SETNode handle_assign(SymbolicInterpreter& parent, SymbolicEnvironment& env, z3::context& ctx, const Node& stmt, const Chunk* left_node, const Node* right_ptr, const Trace& current_trace, bool is_element)
{
    auto right = parent.evaluate(right_ptr, current_trace);
    if (right.get_result().empty())
        log_error("Assignment expression", right, "is empty!");
    EvalResult eval_result;
    for (const auto& [right_val, right_cond] : right.get_result())
        // if (!env.add_constraint(std::any_cast<z3::expr>(right_cond), *ctx).unsatisfiable(*ctx, assign))
        eval_result.emplace_back(right_val, right_cond);

    auto children = MOVE(right.get_children());
    auto new_trace = right.get_trace();
    new_trace.addTrace(stmt);
    SETNode node{eval_result, new_trace, env, &stmt};

    for (const auto& [value_wrap, cond] : eval_result)
    {
        SymbolicEnvironment new_env;
        if (EvalResultType{value_wrap.index()} == EvalResultType::EXCEPTION)
            new_env = env.set_exception(get_value<EvalResultType::EXCEPTION>(value_wrap));
        else
        {
            auto value = get_value<EvalResultType::VALUE>(value_wrap);
            if (is_element) value = value[ctx.int_val(0)];
            new_env = env.set(Variable::from_z3(left_node->str(), value), value, ctx);
        }
        new_env = new_env.add_constraint(std::any_cast<z3::expr>(cond), ctx);
        children.push_back(std::make_shared<SETNode>(EvalResult{}, new_trace, new_env));
    }

    node.set_children(MOVE(children));
    return node;
}

SymbolicInterpreter::SymbolicInterpreter(z3::context& ctx, const SymbolicEnvironment& env)
: stmt_visitor(std::make_unique<SymbolicStmtVisitor>(*this)),
  expr_visitor(std::make_unique<SymbolicExprVisitor>(*this))
{
    stmt_visitor->set_context(ctx);
    stmt_visitor->set_environment(env);
    expr_visitor->set_context(ctx);
    expr_visitor->set_environment(env);
}

SymbolicInterpreter::SymbolicInterpreter(const SymbolicInterpreter& other)
: stmt_visitor(std::make_unique<SymbolicStmtVisitor>(*this)),
  expr_visitor(std::make_unique<SymbolicExprVisitor>(*this))
{
    stmt_visitor->set_context(*other.stmt_visitor->get_context());
    stmt_visitor->set_environment(other.stmt_visitor->get_environment());
    expr_visitor->set_context(*other.expr_visitor->get_context());
    expr_visitor->set_environment(other.expr_visitor->get_environment());
}
SymbolicInterpreter& SymbolicInterpreter::operator=(const SymbolicInterpreter& other) &
{
    stmt_visitor->set_context(*other.stmt_visitor->get_context());
    stmt_visitor->set_environment(other.stmt_visitor->get_environment());
    expr_visitor->set_context(*other.expr_visitor->get_context());
    expr_visitor->set_environment(other.expr_visitor->get_environment());
    return *this;
}
SymbolicInterpreter::SymbolicInterpreter(SymbolicInterpreter&& other) noexcept = default;
SymbolicInterpreter& SymbolicInterpreter::operator=(SymbolicInterpreter&& other) & noexcept = default;

SETNode SymbolicInterpreter::execute(const Node* stmt, const Trace& current_trace) const
{
    return std::any_cast<SETNode>(stmt->accept(stmt_visitor.get(), current_trace));
}

SETNode SymbolicInterpreter::evaluate(const Node* expr, const Trace& current_trace) const
{
    return std::any_cast<SETNode>(expr->accept(expr_visitor.get(), current_trace));
}

const SymbolicEnvironment& SymbolicInterpreter::get_environment() const
{
    return stmt_visitor->get_environment();
}

void SymbolicInterpreter::set_environment(const SymbolicEnvironment& env) const
{
    stmt_visitor->set_environment(env);
    expr_visitor->set_environment(env);
}