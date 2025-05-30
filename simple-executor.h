/**
 * Simple Executor Simple Version
 * Trying to generate an execution trace based on concolic execution on the universal AST
 */

#ifndef SIMPLE_EXECUTOR_H
#define SIMPLE_EXECUTOR_H

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

using FunctionRepo = std::unordered_map<std::string, std::function<std::vector<Trace> (const Trace&)>>;

inline std::ostream& operator<<(std::ostream& out, const FunctionRepo& functions)
{
    out << "{";
    bool first = true;
    for (const auto& [name, func]: functions)
    {
        if (first) first = false;
        else out << ", ";
        out << name;
    }
    out << "}";
    return out;
}

/**
 * Simple Execution Tree (SET) Node: Environment + Next PC
 */
class SimpleNode
{
private:
    Trace trace;
    FunctionRepo functions;
    const Node* next_pc = nullptr;
    std::vector<std::shared_ptr<SimpleNode>> children;

public:
    [[nodiscard]] SimpleNode() = default;
    [[nodiscard]] explicit SimpleNode(
        const Trace& trace,
        const FunctionRepo& functions = {},
        const Node* next_pc = nullptr
    ) : trace(trace), functions(functions), next_pc(next_pc) {}

    [[nodiscard]] const Trace& get_trace() const { return trace; }
    [[nodiscard]] const FunctionRepo& get_functions() const { return functions; }
    [[nodiscard]] const Node* get_next_pc() const { return next_pc; }
    [[nodiscard]] std::vector<std::shared_ptr<SimpleNode>>& get_children() { return children; }
    void set_children(std::vector<std::shared_ptr<SimpleNode>> new_children)
    {
        children = MOVE(new_children);
    }

    // iterator & accessor
    [[nodiscard]] auto begin() { return children.begin(); }
    [[nodiscard]] auto begin() const { return children.begin(); }
    [[nodiscard]] auto end() { return children.end(); }
    [[nodiscard]] auto end() const { return children.end(); }
    [[nodiscard]] auto size() const { return children.size(); }
    [[nodiscard]] bool empty() const { return children.empty(); }
    [[nodiscard]] SimpleNode* operator[](std::size_t index)
    {
        if (index >= size()) error("Index %d out of range! (size = %d)\n", index, size());
        return children[index].get();
    }
    [[nodiscard]] const SimpleNode* operator[](std::size_t index) const
    {
        if (index >= size()) error("Index %d out of range! (size = %d)\n", index, size());
        return children[index].get();
    }
    void set(std::size_t index, SimpleNode node)
    {
        if (index >= size()) error("Index %d out of range! (size = %d)\n", index, size());
        children[index] = std::make_shared<SimpleNode>(MOVE(node));
    }

    [[nodiscard]] std::string to_string() const
    {
        return "{next_pc: " + (next_pc ? next_pc->str() : "null") + "} " + ::to_string(trace);
    }

    friend std::ostream& operator<<(std::ostream& out, const SimpleNode& node)
    {
        out << node.to_string();
        return out;
    }
};

/**
 * Display a SimpleNode Tree
 *
 * @param root  The root of the tree to display
 * @param level Current indentation level
 */
void display_simple(const SimpleNode* root, int level = 0);

[[nodiscard]] static std::vector<SimpleNode*> get_leaves(SimpleNode& root)
{
    if (root.empty()) return {&root};

    std::vector<SimpleNode*> leaves;
    for (const auto& child : root)
        for (auto leaf : get_leaves(*child))
            leaves.push_back(leaf);
    return leaves;
}

// Tree operations
[[nodiscard]] static bool tree_contains(const SimpleNode& tree, const SimpleNode& node)
{
    if (&tree == &node) return true;
    for (const auto& child : tree)
        if (tree_contains(*child, node))
            return true;
    return false;
}

static void tree_path_iterator_aux(
    SimpleNode* root, std::vector<std::pair<Path, SimpleNode*>>& path, Path current_path
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

[[nodiscard]] static std::vector<std::pair<Path, SimpleNode*>> tree_path_iterator(SimpleNode& root)
{
    std::vector<std::pair<Path, SimpleNode*>> path;
    tree_path_iterator_aux(&root, path, {});
    return path;
}

[[nodiscard]] static SimpleNode tree_replace(SimpleNode* tree, SimpleNode* before, SimpleNode after)
{
    for (const auto& [path, child] : tree_path_iterator(*tree))
        if (child == before)
        {
            // In-place replacement
            if (path.empty()) return after;
            SimpleNode* cur_parent = tree;
            auto last_index = path[0];
            SimpleNode* cur_tree = (*cur_parent)[last_index];
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

class SimpleStmtVisitor;
class SimpleExprVisitor;
class SimpleInterpreter;

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
[[nodiscard]] SimpleNode handle_assign(SimpleInterpreter& parent, FunctionRepo& functions, const Node& stmt, const Chunk* left_node, const Node* right_ptr, const Trace& current_trace, bool is_element = false);

/**
 * Simple interpreter for the universal AST
 */
class SimpleInterpreter
{
private:
    std::unique_ptr<SimpleStmtVisitor> stmt_visitor;
    std::unique_ptr<SimpleExprVisitor> expr_visitor;

public:
    [[nodiscard]] explicit SimpleInterpreter(const FunctionRepo& functions = {});
    [[nodiscard]] SimpleInterpreter(const SimpleInterpreter& other);
    [[nodiscard]] SimpleInterpreter& operator=(const SimpleInterpreter& other) &;
    [[nodiscard]] SimpleInterpreter(SimpleInterpreter&& other) noexcept;
    [[nodiscard]] SimpleInterpreter& operator=(SimpleInterpreter&& other) & noexcept;

    // execute = statement, evaluate = expression
    SimpleNode execute(const Node* stmt, const Trace& current_trace) const;
    SimpleNode evaluate(const Node* expr, const Trace& current_trace) const;
    [[nodiscard]] const FunctionRepo& get_functions() const;
    void set_functions(const FunctionRepo& functions) const;
};

// Simple Visitor (std::any is always SimpleNode)
class SimpleStmtVisitor : public NodeVisitor<std::any>
{
private:
    FunctionRepo functions;
    SimpleInterpreter& parent;

    [[nodiscard]] SimpleNode handle_loop(const Node& loop_stmt, const Trace& current_trace) const
    {
        auto cond_expr = loop_stmt.getCond()->get();
        auto body = loop_stmt.getBody()->get();
        auto eval_result = parent.evaluate(cond_expr, current_trace);
        SimpleNode node{current_trace, functions, &loop_stmt};
        node.set_children(MOVE(eval_result.get_children()));

        Trace new_trace = current_trace;
        if (body->kind != BLOCK) new_trace.addTrace(*body);
        SimpleInterpreter then_int{functions};
        auto body_result = then_int.execute(body, new_trace);
        node.get_children().push_back(std::make_shared<SimpleNode>(body_result));
        node.get_children().push_back(std::make_shared<SimpleNode>(current_trace));
        return node;
    }

public:
    [[nodiscard]] explicit SimpleStmtVisitor(SimpleInterpreter& parent) : parent(parent) {}
    [[nodiscard]] const FunctionRepo& get_functions() const { return functions; }
    void set_functions(const FunctionRepo& functions) { this->functions = functions; }

    std::any default_value(const Trace& current_trace) override
    {
        return SimpleNode{current_trace, functions};
    }

    std::any apply(const IfThenElse& if_stmt, const Trace& current_trace) override
    {
        auto cond_expr = if_stmt.getCond()->get();
        auto eval_result = parent.evaluate(cond_expr, current_trace);
        SimpleNode node{current_trace, functions, &if_stmt};
        node.set_children(MOVE(eval_result.get_children()));

        Trace new_trace = current_trace;
        for (const auto& child : if_stmt)
        {
            if (child == *if_stmt.getCond()) continue;
            const Node* inner = child.get();
            if (child->kind == ELIF_STMT)
            {
                new_trace.addTrace(*child);
                auto inner_result = parent.evaluate(child->getCond()->get(), new_trace);
                for (const auto& inner_child : inner_result.get_children())
                    node.get_children().push_back(MOVE(inner_child));
                inner = child->getBody()->get();
            }
            Trace inner_trace = new_trace;
            if (child->kind != BLOCK && child->kind != EMPTY) inner_trace.addTrace(*inner);
            SimpleInterpreter inner_int{functions};
            node.get_children().push_back(std::make_shared<SimpleNode>(inner_int.execute(inner, inner_trace)));
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
        return SimpleNode{current_trace, functions, nullptr};
    }

    std::any apply(const Continue& continue_stmt, const Trace& current_trace) override
    {
        return SimpleNode{current_trace, functions, nullptr};
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

        std::optional<SimpleNode> result;
        std::vector<SimpleNode*> children;

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

            for (auto child : children)
            {
                auto new_functions = functions;
                for (const auto& func : child->get_functions())
                    new_functions.insert(func);
                SimpleInterpreter child_int{new_functions};
                Trace new_trace = child->get_trace();
                new_trace.addTrace(*stmt);
                auto subtree = child_int.execute(stmt, new_trace);
                result = tree_replace(&*result, child, subtree);
            }
            children = get_leaves(*result);
        }
        if (!result) log_error("Invalid block:", block.str());
        return *result;
    }

    std::any apply(const Expr& expr, const Trace& current_trace) override
    {
        if (!expr.detail) return default_value(current_trace);
        auto inner = parent.evaluate(expr.detail.get(), current_trace);
        return inner;
    }

    std::any apply(const Decl& decl, const Trace& current_trace) override
    {
        if (decl.empty()) return default_value(current_trace);
        auto current_functions = functions;
        for (const auto& [var_chunk, index] : decl.vars)
        {
            auto right = decl.child[index].get();
            if (right->kind == EMPTY) continue;
            auto result = handle_assign(parent, current_functions, decl, &var_chunk, right, current_trace, decl.is_python_for_range);
            current_functions = result.get_children().back()->get_functions();
        }
        return SimpleNode{current_trace, current_functions};
    }

    std::any apply(const Return& return_stmt, const Trace& current_trace) override
    {
        auto expr = parent.evaluate(return_stmt.begin()->get(), current_trace);
        auto new_trace = expr.get_trace();
#ifdef PYTHON_PARSER
        new_trace.addTrace(return_stmt, Event::RET);
#endif
        return SimpleNode{new_trace, functions, nullptr};
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

        auto new_functions = functions;
        new_functions[func_name] = [this, params = MOVE(params), &func, func_name](const Trace& current_trace) mutable -> std::vector<Trace> {
            parent.set_functions(functions);

            auto new_trace = current_trace;
            new_trace.addTrace(func, Event::CLL);
            auto block_result = parent.execute(func.getBody()->get(), new_trace);
            std::vector<Trace> traces;
            for (auto node : get_leaves(block_result))
                traces.push_back(node->get_trace());
            return traces;
        };

        return SimpleNode{current_trace, new_functions};
    }
};

class SimpleExprVisitor : public NodeVisitor<std::any>
{
private:
    FunctionRepo functions;
    SimpleInterpreter& parent;

    template<typename F>
    [[nodiscard]] SimpleNode handle_call(const Node& expr, const Trace& current_trace, const F& func)
    {
        std::vector<std::shared_ptr<SimpleNode>> children;
        for (const auto& child : expr)
        {
            auto eval_result = parent.evaluate(child.get(), current_trace);
            for (auto& inner_child : eval_result.get_children())
                children.push_back(MOVE(inner_child));
        }
        SimpleNode node{current_trace, functions};
        node.set_children(MOVE(children));
        auto leaves = get_leaves(node);

        for (auto leaf : leaves)
        {
            auto traces = func(leaf->get_trace());
            for (int i = 0; i < traces.size(); ++i)
            {
                node = tree_replace(&node, leaf, SimpleNode{traces[i], functions});
                children.push_back(std::make_shared<SimpleNode>(traces[i], functions));
            }
        }
        return node;
    }

public:
    [[nodiscard]] explicit SimpleExprVisitor(SimpleInterpreter& parent) : parent(parent) {}
    [[nodiscard]] const FunctionRepo& get_functions() const { return functions; }
    void set_functions(const FunctionRepo& functions) { this->functions = functions; }

    std::any default_value(const Trace& current_trace) override
    {
        return SimpleNode{current_trace, functions};
    }

    std::any apply(const BooleanLiteral& boolean, const Trace& current_trace) override
    {
        auto str = boolean.str();
#ifdef PYTHON_PARSER
        if (str != "True" && str != "False") log_error("Invalid boolean literal:", str);
#else
        if (str != "true" && str != "false") log_error("Invalid boolean literal:", str);
#endif
        return SimpleNode{current_trace, functions};
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
        return SimpleNode{current_trace, functions};
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
        return SimpleNode{current_trace, functions};
    }

    std::any apply(const CharLiteral& ch, const Trace& current_trace) override
    {
        auto inner = ch.str();
        if (inner.size() < 2 || inner[0] != '\'' || inner.back() != '\'')
            log_error("Invalid char literal:", inner);
        inner = unescape(inner.substr(1, inner.size() - 2), true);
        if (inner.size() > 2 || (inner.size() == 2 && inner[0] != '\\'))
            log_error("Multi-character char literal:", inner);
        return SimpleNode{current_trace, functions};
    }

#ifdef PYTHON_PARSER
    std::any apply(const ListLiteral& list, const Trace& current_trace) override
    {
        auto inner = list.str();
        if (inner.size() < 2 || inner[0] != '[' || inner.back() != ']')
            log_error("Invalid list literal:", inner);

        if (list.empty())
            return SimpleNode{current_trace, functions};

        return handle_call(list, current_trace, [](const Trace& trace) -> std::vector<Trace> {
            return {trace};
        });
    }

    std::any apply(const SliceLiteral& slice, const Trace& current_trace) override
    {
        log_error("Bare slice literal:", slice.str());
    }
#endif

    std::any apply(const Assignment& assign, const Trace& current_trace) override
    {
        return handle_assign(parent, functions, assign, assign.child[0].get(), assign.child[1].get(), current_trace);
    }

    std::any apply(const Unary& unary, const Trace& current_trace) override
    {
        auto inner = parent.evaluate(unary.child[0].get(), current_trace);
        SimpleNode node{current_trace, functions};
        node.set_children(MOVE(inner.get_children()));
        return node;
    }

    std::any apply(const Binary& binary, const Trace& current_trace) override
    {
        auto left = parent.evaluate(binary.child[0].get(), current_trace);
        auto right = parent.evaluate(binary.child[1].get(), current_trace);
        auto children = MOVE(left.get_children());
        for (auto& elem : right.get_children())
            children.push_back(MOVE(elem));
        SimpleNode node{current_trace, functions};
        node.set_children(MOVE(children));
        return node;
    }

    std::any apply(const Ternary& ternary, const Trace& current_trace) override
    {
        auto cond_node = parent.evaluate(ternary.getCond()->get(), current_trace);
        auto then_node = parent.evaluate(ternary.child[1].get(), current_trace);
        auto else_node = parent.evaluate(ternary.child[2].get(), current_trace);
        auto children = MOVE(cond_node.get_children());
        for (auto& elem : then_node.get_children())
            children.push_back(MOVE(elem));
        for (auto& elem : else_node.get_children())
            children.push_back(MOVE(elem));
        SimpleNode node{current_trace, functions};
        node.set_children(MOVE(children));
        return node;
    }

    std::any apply(const Subscript& subscript, const Trace& current_trace) override
    {
        auto arg = parent.evaluate(subscript.child[0].get(), current_trace);
        auto indexChild = subscript.child[1].get();
#ifdef PYTHON_PARSER
        SimpleNode index;
        // TODO: handle slices better
        if (auto slice = dynamic_cast<const SliceLiteral*>(indexChild))
            index = arg;
        else index = parent.evaluate(indexChild, current_trace);
#else
        auto index = parent.evaluate(indexChild, current_trace);
#endif
        auto children = MOVE(arg.get_children());
        for (auto& elem : index.get_children())
            children.push_back(MOVE(elem));
        SimpleNode node{current_trace, functions};
        node.set_children(MOVE(children));
        return node;
    }

    std::any apply(const Expr& expr, const Trace& current_trace) override
    {
        SimpleNode node{current_trace, functions};
        if (expr.detail) node = parent.evaluate(expr.detail.get(), current_trace);

        for (const auto& child : expr)
        {
            const Node* inner = child.get();
            Trace new_trace = current_trace;
            if (child->kind != BLOCK && child->kind != EMPTY) new_trace.addTrace(*inner);
            SimpleInterpreter inner_int{functions};
            node.get_children().push_back(std::make_shared<SimpleNode>(inner_int.evaluate(inner, new_trace)));
        }
        return node;
    }

    std::any apply(const Call& expr, const Trace& current_trace) override
    {
        std::vector<std::shared_ptr<SimpleNode>> children;
        auto func_name = expr.func.str();
        if (functions.find(func_name) == functions.end()) return default_value(current_trace);
        auto func = functions[func_name];
        // return handle_call(expr, current_trace, func);
        return default_value(current_trace);
    }
};

#endif //SIMPLE_EXECUTOR_H
