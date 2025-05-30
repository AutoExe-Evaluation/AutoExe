#ifndef AUTOBUG_SYMBOLIC_VALUE_INTERPRETER_H
#define AUTOBUG_SYMBOLIC_VALUE_INTERPRETER_H

#include "interpreter.h"

using ProgramFunctionType = ProgramValue (const std::vector<ProgramValue>&);

class ValueEnvironment : public Environment<ValueStore, ProgramFunctionType>
{
public:
    [[nodiscard]] ValueEnvironment() = default;
    [[nodiscard]] explicit ValueEnvironment(const ValueStore& store, const FunctionRepo& functions = {})
        : Environment(store, functions)
    {
        add_default_functions();
    }

    void add_default_functions() override
    {
        functions["add"] = Function{
            TypeIndex::INT,
            {Variable{"a", TypeIndex::INT}, Variable{"b", TypeIndex::INT}},
            [](const std::vector<ProgramValue>& args) -> ProgramValue {
                return std::get<std::int64_t>(args[0]) + std::get<std::int64_t>(args[1]);
            }
        };
    }

    [[nodiscard]] ValueEnvironment add_function(const std::string& name, const Function& function) const
    {
        auto new_env = *this;
        new_env.functions[name] = function;
        return new_env;
    }
};

class StmtVisitor;
class ExprVisitor;

/**
 * Interpreter for the universal AST
 */
class ValueInterpreter
{
private:
    std::unique_ptr<StmtVisitor> stmt_visitor;
    std::unique_ptr<ExprVisitor> expr_visitor;

public:
    [[nodiscard]] explicit ValueInterpreter(const ValueEnvironment& env);

    [[nodiscard]] ValueInterpreter(const ValueInterpreter& other);
    [[nodiscard]] ValueInterpreter& operator=(const ValueInterpreter& other) &;
    [[nodiscard]] ValueInterpreter(ValueInterpreter&& other) noexcept;
    [[nodiscard]] ValueInterpreter& operator=(ValueInterpreter&& other) & noexcept;

    // execute = statement, evaluate = expression
    void execute(const Node* stmt) const;
    ProgramValue evaluate(const Node* expr) const;
};

// Real interpreter part
// Implemented via two visitors on stmt and expr
class StmtVisitor : public NodeVisitor<ProgramValue>
{
private:
    ValueEnvironment env;
    ValueInterpreter& parent;

public:
    [[nodiscard]] explicit StmtVisitor(ValueInterpreter& parent) : parent(parent) {}
    [[nodiscard]] const ValueEnvironment& get_environment() const { return env; }
    void set_environment(const ValueEnvironment& env) { this->env = env; }

    ProgramValue apply(const IfThenElse& if_stmt, const Trace&) override
    {
        auto cond = if_stmt.getCond()->get();
        bool first = true;
        for (const auto& child : if_stmt)
        {
            if (child == *if_stmt.getCond()) continue;
            if (first)
            {
                first = false;
                if (auto result = parent.evaluate(cond); to_bool(result))
                    parent.execute(child.get());
                continue;
            }
            if (child->kind != ELIF_STMT)
            {
                parent.execute(child.get());
                break;
            }
            if (auto result = parent.evaluate(child->getCond()->get()); to_bool(result))
                parent.execute(child->getBody()->get());
        }
        return {};
    }

    ProgramValue apply(const Block& block, const Trace&) override
    {
        for (const auto& child : block)
            parent.execute(child.get());
        return {};
    }

    ProgramValue apply(const Expr& expr, const Trace&) override
    {
        if (!expr.detail) return {};
        std::ignore = parent.evaluate(expr.detail.get());
        return {};
    }
};

class ExprVisitor : public NodeVisitor<ProgramValue>
{
private:
    ValueEnvironment env;
    ValueInterpreter& parent;

public:
    [[nodiscard]] explicit ExprVisitor(ValueInterpreter& parent) : parent(parent) {}
    [[nodiscard]] const ValueEnvironment& get_environment() const { return env; }
    void set_environment(const ValueEnvironment& env) { this->env = env; }

    ProgramValue apply(const Identifier& identifier, const Trace&) override
    {
        return env.get_store()[identifier.str()].get_value();
    }
    ProgramValue apply(const NumberLiteral& number, const Trace&) override
    {
        auto result = parse_number(number.str());
        if (result.index() == 0) return std::get<0>(result);
        return std::get<1>(result);
    }
    ProgramValue apply(const BooleanLiteral& boolean, const Trace&) override
    {
        auto str = boolean.str();
#ifdef PYTHON_PARSER
        if (str == "True") return true;
        if (str == "False") return false;
#else
        if (str == "true") return true;
        if (str == "false") return false;
#endif
        log_error("Invalid boolean literal:", str);
    }
    ProgramValue apply(const StringLiteral& str, const Trace&) override
    {
        auto inner = str.str();
        if (inner.size() < 2 || ((inner[0] != '\"' || inner.back() != '\"')
                                 #ifdef PYTHON_PARSER
                                 && (inner[0] != '\'' || inner.back() != '\'')
#endif
        ))
            log_error("Invalid string literal:", inner);
        return unescape(inner.substr(1, inner.size() - 2));
    }
    ProgramValue apply(const NullLiteral& null, const Trace&) override
    {
#ifdef PYTHON_PARSER
        if (null.str() != "None")
#elif defined(JAVA_PARSER)
        if (null.str() != "null")
#else
        if (null.str() != "NULL")
#endif
            log_error("Invalid null literal:", null.str());
        return nullptr;
    }
    ProgramValue apply(const CharLiteral& ch, const Trace&) override
    {
        auto inner = ch.str();
        if (inner.size() < 2 || inner[0] != '\'' || inner.back() != '\'')
            log_error("Invalid char literal:", inner);
        inner = unescape(inner.substr(1, inner.size() - 2), true);
        if (inner.size() > 2 || (inner.size() == 2 && inner[0] != '\\'))
            log_error("Multi-character char literal:", inner);
        return inner[0];
    }
    ProgramValue apply(const ListLiteral& list, const Trace&) override
    {
        auto inner = list.str();
        if (inner.size() < 2 || inner[0] != '[' || inner.back() != ']')
            log_error("Invalid list literal:", inner);

        std::vector<ValueType> values;
        for (const auto& elem : list)
            values.emplace_back(parent.evaluate(elem.get()));
        return values;
    }
    ProgramValue apply(const Assignment& assign, const Trace&) override
    {
        // TODO: handling of non-trivial LHS
        auto left_child = assign.child[0].get();
        auto left = parent.evaluate(left_child);
        auto right = parent.evaluate(assign.child[1].get());
        Variable var{left_child->str(), static_cast<TypeIndex>(left.index())};
        env.get_store()[var] = ValueType{right};
        return {};
    }
    ProgramValue apply(const Unary& unary, const Trace&) override
    {
        static std::unordered_map<std::string, std::function<ProgramFunctionType>> unary_ops = {
#define UNARY_FUNC(op) \
            { #op, [](const std::vector<ProgramValue>& args) -> ProgramValue { \
                return std::visit( \
                    overload{ \
                        [](const auto& arg) -> decltype((void)(op arg), ProgramValue{}) { return op arg; }, \
                        [](const ProgramValue& arg) -> ProgramValue { \
                            log_error_sep("", "Invalid operand for ", #op, ": ", ValueType{arg}); \
                        } \
                    }, \
                    args[0] \
                ); \
            }}
            UNARY_FUNC(+),
            UNARY_FUNC(-),
            UNARY_FUNC(*),
            UNARY_FUNC(&),
            UNARY_FUNC(!)
#undef UNARY_FUNC
        };
        auto it = unary_ops.find(unary.op);
        if (it == unary_ops.end())
            log_error("Invalid unary operator:", unary.op);

        auto inner = unary.child[0].get();
        auto result = parent.evaluate(inner);
        return it->second({result});
    }
    ProgramValue apply(const Binary& binary, const Trace&) override
    {
        static std::unordered_map<std::string, std::function<ProgramFunctionType>> binary_ops = {
#define BINARY_FUNC(op) \
            { #op, [](const std::vector<ProgramValue>& args) -> ProgramValue { \
                return std::visit( \
                    overload{ \
                        [](const auto& arg1, const auto& arg2) -> decltype((void)(arg1 op arg2), ProgramValue{}) { return arg1 op arg2; }, \
                        [](const ProgramValue& arg1, const ProgramValue& arg2) -> ProgramValue { \
                            log_error_sep("", "Invalid operand for ", #op, ": ", ValueType{arg1}, " + ", ValueType{arg2}); \
                        } \
                    }, \
                    args[0], args[1] \
                ); \
            }}

#define COMPARISON_FUNC(op) \
            { #op, [](const std::vector<ProgramValue>& args) -> ProgramValue { \
                return std::visit( \
                    overload{ \
                        [](const auto& arg1, const auto& arg2) -> std::enable_if_t< \
                            std::is_same_v<std::decay_t<decltype(arg1)>, std::decay_t<decltype(arg2)>> && \
                            !std::is_same_v<std::decay_t<decltype(arg1)>, std::monostate> && \
                            !std::is_same_v<std::decay_t<decltype(arg1)>, std::vector<ValueType>>, \
                            ProgramValue \
                        > { return arg1 op arg2; }, \
                        [](const ProgramValue& arg1, const ProgramValue& arg2) -> ProgramValue { \
                            log_error_sep("", "Invalid operand for ", #op, ": ", ValueType{arg1}, " + ", ValueType{arg2}); \
                        } \
                    }, \
                    args[0], args[1] \
                ); \
            }}

#define LOGICAL_FUNC(op) \
            { #op, [](const std::vector<ProgramValue>& args) -> ProgramValue { \
                return std::visit( \
                    overload{ \
                        [](const auto& arg1, const auto& arg2) -> std::enable_if_t< \
                            std::is_same_v<std::decay_t<decltype(arg1)>, std::decay_t<decltype(arg2)>> && \
                            !std::is_same_v<std::decay_t<decltype(arg1)>, std::monostate> && \
                            !std::is_same_v<std::decay_t<decltype(arg1)>, std::string> && \
                            !std::is_same_v<std::decay_t<decltype(arg1)>, std::vector<ValueType>>, \
                            ProgramValue \
                        > { return arg1 op arg2; }, \
                        [](const ProgramValue& arg1, const ProgramValue& arg2) -> ProgramValue { \
                            log_error_sep("", "Invalid operand for ", #op, ": ", ValueType{arg1}, " + ", ValueType{arg2}); \
                        } \
                    }, \
                    args[0], args[1] \
                ); \
            }}
            BINARY_FUNC(+),
            BINARY_FUNC(-),
            BINARY_FUNC(*),
            BINARY_FUNC(/),
            BINARY_FUNC(%),
            COMPARISON_FUNC(==),
            COMPARISON_FUNC(!=),
            COMPARISON_FUNC(<),
            COMPARISON_FUNC(>),
            COMPARISON_FUNC(<=),
            COMPARISON_FUNC(>=),
            LOGICAL_FUNC(&&),
            LOGICAL_FUNC(||)
#undef BINARY_FUNC
#undef COMPARISON_FUNC
#undef LOGICAL_FUNC
        };
        auto it = binary_ops.find(binary.op);
        if (it == binary_ops.end())
            log_error("Invalid binary operator:", binary.op);

        auto left = parent.evaluate(binary.child[0].get());
        auto right = parent.evaluate(binary.child[1].get());
        return it->second({left, right});
    }
    // TODO: ternary

    ProgramValue apply(const Expr& expr, const Trace&) override
    {
        if (!expr.detail) return {};
        return parent.evaluate(expr.detail.get());
    }
};

#endif //AUTOBUG_SYMBOLIC_VALUE_INTERPRETER_H
