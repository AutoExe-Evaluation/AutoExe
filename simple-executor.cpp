#include <map>

#include "simple-executor.h"

/**
 * Display a SimpleNode Tree
 *
 * @param root  The root of the tree to display
 * @param level Current indentation level
 */
void display_simple(const SimpleNode* root, int level)
{
    if (!root) return;
    std::string indent;
    for (int i = 0; i < level; ++i) indent += "  ";
    info("%s", indent.c_str());
    std::string result = ::to_string(root->get_trace()) + " " + (root->get_next_pc() ? root->get_next_pc()->str() : "null");
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
        display_simple(child.get(), level + 1);
    }
}

/**
 * Handle assignment to variables.
 *
 * @param parent        Parent interpreter
 * @param functions     Current function repo
 * @param stmt          Assignment/declaration statement
 * @param left_node     Variable assigned to
 * @param right_ptr     Value to be assigned (can be Empty)
 * @param current_trace Current trace
 * @param is_element    True if the variable refers to an element
 * @return              Evaluation result
 */
SimpleNode handle_assign(SimpleInterpreter& parent, FunctionRepo& functions, const Node& stmt, const Chunk* left_node, const Node* right_ptr, const Trace& current_trace, bool is_element)
{
    auto right = parent.evaluate(right_ptr, current_trace);
    auto children = MOVE(right.get_children());
    auto new_trace = right.get_trace();
    // new_trace.addTrace(stmt);
    SimpleNode node{new_trace, functions, &stmt};
    children.push_back(std::make_shared<SimpleNode>(new_trace));
    node.set_children(MOVE(children));
    return node;
}

SimpleInterpreter::SimpleInterpreter(const FunctionRepo& functions)
    : stmt_visitor(std::make_unique<SimpleStmtVisitor>(*this)),
      expr_visitor(std::make_unique<SimpleExprVisitor>(*this))
{
    stmt_visitor->set_functions(functions);
    expr_visitor->set_functions(functions);
}

SimpleInterpreter::SimpleInterpreter(const SimpleInterpreter& other)
    : stmt_visitor(std::make_unique<SimpleStmtVisitor>(*this)),
      expr_visitor(std::make_unique<SimpleExprVisitor>(*this))
{
    stmt_visitor->set_functions(other.stmt_visitor->get_functions());
    expr_visitor->set_functions(other.expr_visitor->get_functions());
}
SimpleInterpreter& SimpleInterpreter::operator=(const SimpleInterpreter& other) &
{
    stmt_visitor->set_functions(other.stmt_visitor->get_functions());
    expr_visitor->set_functions(other.expr_visitor->get_functions());
    return *this;
}
SimpleInterpreter::SimpleInterpreter(SimpleInterpreter&& other) noexcept = default;
SimpleInterpreter& SimpleInterpreter::operator=(SimpleInterpreter&& other) & noexcept = default;

SimpleNode SimpleInterpreter::execute(const Node* stmt, const Trace& current_trace) const
{
    return std::any_cast<SimpleNode>(stmt->accept(stmt_visitor.get(), current_trace));
}

SimpleNode SimpleInterpreter::evaluate(const Node* expr, const Trace& current_trace) const
{
    return std::any_cast<SimpleNode>(expr->accept(expr_visitor.get(), current_trace));
}

const FunctionRepo& SimpleInterpreter::get_functions() const
{
    return stmt_visitor->get_functions();
}

void SimpleInterpreter::set_functions(const FunctionRepo& functions) const
{
    stmt_visitor->set_functions(functions);
    expr_visitor->set_functions(functions);
}