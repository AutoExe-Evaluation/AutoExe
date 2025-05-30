// A C intepreter

#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <cstdint>

#include <array>
#include <charconv>
#include <functional>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <unordered_map>

#include "AST.h"
#include "constraint-solver.h"
#include "coverage_map.h"

// AST Converter related functions

// Path: a list of indexes
using Path = std::vector<ssize_t>;
using NodeList = std::vector<const Node*>;

/**
 * Unwrap Expr -> details
 *
 * @param root Root of the AST
 * @return     Unwrapped AST root
 */
[[nodiscard]] inline const Node* unwrap(const Node* root)
{
    if (auto expr = dynamic_cast<const Expr*>(root); expr && expr->detail)
        root = expr->detail.get();
    return root;
}

/**
 * Get the subtree denoted by the path
 *
 * @param root Root of the AST
 * @param path Current path
 * @return     The denoted subtree
 */
[[nodiscard]] const Node* get_subtree(const Node* root, const Path& path);

/**
 * Return the next path in the AST
 *
 * @param root          Root of the AST
 * @param path          Current path
 * @param allow_descend Whether to allow descending into the subtree
 * @return              Next sibling path if it exists
 */
[[nodiscard]] std::optional<Path> next_path(const Node* root, const Path& path, bool allow_descend = false);

/**
 * Match AST elements to a list of non-terminals
 *
 * @param symbols A list of non-terminals to be matched
 * @param root    Root of the AST
 * @return        Disengaged if the tree does not match the expected structure, otherwise a list of matched nodes
 */
[[nodiscard]] std::optional<NodeList> match_ast(const Node* root, const std::vector<Kind>& symbols);

/**
 * Substitute with a given mapping in an expression
 *
 * @param expr      The expression to be processed
 * @param ctx       Z3 context to be used
 * @param subst_map Substitution mapping
 * @return          Substituted expression
 */
[[nodiscard]] z3::expr subst(
    const z3::expr& expr, z3::context& ctx, const std::vector<std::pair<z3::expr, z3::expr>>& subst_map
);

/**
 * Variable: Representing a variable in the program
 */
class Variable
{
private:
    std::string name;
    TypeIndex type;

public:
    [[nodiscard]] Variable(std::string_view name, TypeIndex type)
        : name(name), type(type)
    {
        if (type >= TypeIndex::MAX) error("Invalid type index: %d\n", static_cast<int>(type));
    }

    [[nodiscard]] bool operator==(const Variable& other) const
    {
        return name == other.name && type == other.type;
    }

    [[nodiscard]] bool operator!=(const Variable& other) const
    {
        return !(*this == other);
    }

    [[nodiscard]] std::string_view get_name() const noexcept
    {
        return name;
    }

    [[nodiscard]] TypeIndex get_type() const noexcept
    {
        return type;
    }

    [[nodiscard]] std::string to_string() const
    {
        return "Variable(" + name + ", " + get_type_name(type) + ")";
    }

    friend std::ostream& operator<<(std::ostream& out, const Variable& value)
    {
        out << value.to_string();
        return out;
    }

    // Z3 integration
    z3::expr to_z3(z3::context& ctx) const
    {
        switch (type)
        {
            case TypeIndex::BOOL: return ctx.bool_const(name.data());
            case TypeIndex::INT: return ctx.int_const(name.data());
            case TypeIndex::DOUBLE: return ctx.fpa_const<64>(name.data());
            case TypeIndex::CHAR: return ctx.int_const(name.data());
            case TypeIndex::STRING: return ctx.string_const(name.data());
            case TypeIndex::POINTER: return ctx.int_const(name.data()); // TODO: proper pointer type
            case TypeIndex::LIST: return z3_sequence(name, ctx);
            default: error("Invalid type index: %d\n", static_cast<int>(type));
        }
    }

    static Variable from_z3(std::string_view name, const z3::expr& expr)
    {
        auto type = TypeIndex::VOID;
        if (expr.is_bool()) type = TypeIndex::BOOL;
        else if (expr.is_int()) type = TypeIndex::INT;
        else if (expr.is_fpa()) type = TypeIndex::DOUBLE;
        else if (expr.is_string_value() || expr.get_sort().to_string() == "String") type = TypeIndex::STRING;
        else if (expr.is_seq()) type = TypeIndex::LIST;
        else log_error("Unknown expr type:", expr);
        // TODO: Proper handling for char and pointer
        return {name, type};
    }
};

template<>
struct std::hash<Variable>
{
    [[nodiscard]] std::size_t operator()(const Variable& value) const noexcept
    {
        return hash_multi(value.get_name(), value.get_type());
    }
};

/**
 * Store: a mapping from variable to values
 *
 * @tparam T The value type to be stored
 */
template<typename T>
class Store
{
protected:
    using StoreType = std::unordered_map<Variable, T>;
    StoreType env;

public:
    using ElemType = T;

    [[nodiscard]] Store() = default;
    [[nodiscard]] explicit Store(const StoreType& env) : env(env) {}

    [[nodiscard]] std::optional<Variable> get_variable(std::string_view name) const
    {
        for (const auto& [var, _]: env)
            if (var.get_name() == name)
                return var;
        return std::nullopt;
    }

    [[nodiscard]] bool has_variable(std::string_view name) const
    {
        return get_variable(name).has_value();
    }
    [[nodiscard]] bool has_variable(const Variable& var) const
    {
        // ReSharper disable once CppUseAssociativeContains
        return env.find(var) != env.end();
    }

    // Accessor passthrough
    [[nodiscard]] auto begin() noexcept { return env.begin(); }
    [[nodiscard]] auto begin() const noexcept { return env.begin(); }
    [[nodiscard]] auto end() noexcept { return env.end(); }
    [[nodiscard]] auto end() const noexcept { return env.end(); }

    [[nodiscard]] T& operator[](std::string_view name)
    {
        if (auto maybe = get_variable(name))
            return env.find(*maybe)->second;
        log_error("Undefined variable:", name);
    }
    [[nodiscard]] T& operator[](const Variable& var)
    {
        if (auto it = env.find(var); it != env.end())
            return it->second;
        log_error("Undefined variable:", var.to_string());
    }
    [[nodiscard]] const T& operator[](std::string_view name) const
    {
        if (auto maybe = get_variable(name))
            return env.find(*maybe)->second;
        log_error("Undefined variable:", name);
    }
    [[nodiscard]] const T& operator[](const Variable& var) const
    {
        if (auto it = env.find(var); it != env.end())
            return it->second;
        log_error("Undefined variable:", var.to_string());
    }

    // operator passthrough
    [[nodiscard]] bool operator==(const Store& other) const
    {
        return env == other.env;
    }
    [[nodiscard]] bool operator!=(const Store& other) const
    {
        return !(*this == other);
    }

    [[nodiscard]] std::string to_string() const
    {
        std::string result = "{";
        bool first = true;
        for (const auto& [var, value]: env)
        {
            if (first) first = false;
            else result += ", ";
            result += var.to_string() + ": " + value.to_string();
        }
        return result + "}";
    }

    friend std::ostream& operator<<(std::ostream& out, const Store& store)
    {
        out << store.to_string();
        return out;
    }
};

using ValueStore = Store<ValueType>;

/**
 * Environment: store a store and a repository of defined functions
 *
 * @tparam StoreType    The storage type of the variables
 * @tparam FunctionType Function type
 */
template<typename StoreType, typename FunctionType>
class Environment
{
public:
    struct Function
    {
        TypeIndex return_type = TypeIndex::INT;
        std::vector<Variable> arguments;
        std::function<FunctionType> function;
    };
    using FunctionRepo = std::unordered_map<std::string, Function>;

protected:
    StoreType store;
    FunctionRepo functions;

public:
    [[nodiscard]] Environment() = default;
    virtual ~Environment() = default;
    [[nodiscard]] explicit Environment(const StoreType& store, const FunctionRepo& functions = {})
        : store(store), functions(functions)
    {}

    virtual void add_default_functions() = 0;

    [[nodiscard]] StoreType& get_store()
    {
        return store;
    }
    [[nodiscard]] const StoreType& get_store() const
    {
        return store;
    }

    [[nodiscard]] const Function& operator[](const std::string& name) const
    {
        if (auto it = functions.find(name); it != functions.end())
            return it->second;
        log_error("Undefined function:", name);
    }
    [[nodiscard]] bool contains(const std::string& name) const
    {
        return functions.find(name) != functions.end();
    }

    [[nodiscard]] std::string to_string() const
    {
        std::string result = "{store: " + store.to_string() + ", function: {";
        bool first = true;
        for (const auto& [name, func]: functions)
        {
            if (first) first = false;
            else result += ", ";
            result += name + ": (" + ::to_string(func.arguments) + ") -> " + get_type_name(func.return_type);
        }
        return result + "}}";
    }

    friend std::ostream& operator<<(std::ostream& out, const Environment& env)
    {
        out << env.to_string();
        return out;
    }
};

// Convert between C++ expression and Z3 expression
[[nodiscard]] z3::expr to_z3_expr(const ProgramValue& value, z3::context& ctx);
[[nodiscard]] ProgramValue from_z3_expr(const z3::expr& expr);

// Coerce all compatible expressions to bool
[[nodiscard]] bool to_bool(const ProgramValue& value);

#endif //INTERPRETER_H
