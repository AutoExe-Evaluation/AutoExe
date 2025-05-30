#include "value_interpreter.h"

ValueInterpreter::ValueInterpreter(const ValueEnvironment& env)
    : stmt_visitor(std::make_unique<StmtVisitor>(*this)),
      expr_visitor(std::make_unique<ExprVisitor>(*this))
{
    stmt_visitor->set_environment(env);
    expr_visitor->set_environment(env);
}

ValueInterpreter::ValueInterpreter(const ValueInterpreter& other)
    : stmt_visitor(std::make_unique<StmtVisitor>(*this)),
      expr_visitor(std::make_unique<ExprVisitor>(*this))
{
    stmt_visitor->set_environment(other.stmt_visitor->get_environment());
    expr_visitor->set_environment(other.expr_visitor->get_environment());
}
ValueInterpreter& ValueInterpreter::operator=(const ValueInterpreter& other) &
{
    stmt_visitor->set_environment(other.stmt_visitor->get_environment());
    expr_visitor->set_environment(other.expr_visitor->get_environment());
    return *this;
}
ValueInterpreter::ValueInterpreter(ValueInterpreter&& other) noexcept = default;
ValueInterpreter& ValueInterpreter::operator=(ValueInterpreter&& other) & noexcept = default;

void ValueInterpreter::execute(const Node* stmt) const
{
    stmt->accept(stmt_visitor.get(), {});
}

ProgramValue ValueInterpreter::evaluate(const Node* expr) const
{
    return expr->accept(expr_visitor.get(), {});
}