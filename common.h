// Common definitions and functions

#ifndef AUTOBUG_SYMBOLIC_COMMON_H
#define AUTOBUG_SYMBOLIC_COMMON_H

#include <cstdint>

#include <any>
#include <exception>
#include <iostream>
#include <optional>
#include <random>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <unordered_map>

#define PURE        __attribute__((__const__))

#define RED         (option_tty? "\33[31m": "")
#define GREEN       (option_tty? "\33[32m": "")
#define YELLOW      (option_tty? "\33[33m": "")
#define BLUE        (option_tty? "\33[34m": "")
#define MAGENTA     (option_tty? "\33[35m": "")
#define CYAN        (option_tty? "\33[36m": "")
#define OFF         (option_tty? "\33[0m" : "")
#define BR_GREEN    ("\33[32m")
#define BR_BLACK    ("\33[90m")
#define BR_OFF      ("\33[0m")

#define STR2(x)     #x
#define STR(x)      STR2(x)

#define MOVE(...)   static_cast<std::remove_reference_t<decltype(__VA_ARGS__)>&&>(__VA_ARGS__)
#define FWD(...)    static_cast<decltype(__VA_ARGS__)&&>(__VA_ARGS__)

inline bool option_tty     = false;
inline bool option_quiet   = false;
inline uint32_t option_gap = 16;

#undef NDEBUG

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

/*
 * Report an error and exit.
 */
[[noreturn]] void error(const char *msg, ...);

/*
 * Print a warning message.
 */
void warning(const char *msg, ...);

/*
 * Print an info message.
 */
void info(const char *msg, ...);

// IOStreams logging functions
inline std::ostream& operator<<(std::ostream& out, std::monostate);
template<typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& value);
template<typename T>
std::ostream& operator<<(std::ostream& out, const std::optional<T>& value);
template<typename T, typename U>
std::ostream& operator<<(std::ostream& out, const std::pair<T, U>& value);

// Exception type, use exception* for now
using ExceptionType = const std::exception*;

inline std::ostream& operator<<(std::ostream& out, ExceptionType exception);

// Evaluation result can be either a value or an exception
// Modeled by a vector-of-pairs, where the second element refers to the condition under which this result is produced
// std::any here always represents a z3::expr, used to avoid z3++.h dependency
using EvalResult = std::vector<std::pair<std::variant<std::any, ExceptionType>, std::any>>;

std::ostream& operator<<(std::ostream& out, const EvalResult& result);

// monostate output
inline std::ostream& operator<<(std::ostream& out, std::monostate)
{
    out << "Void";
    return out;
}

// exception output
inline std::ostream& operator<<(std::ostream& out, ExceptionType exception)
{
    out << "Exception[" << exception->what() << "]";
    return out;
}

// Convenience vector<T> output
template<typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& value)
{
    out << "[";
    bool first = true;
    for (const auto& elem : value)
    {
        if (first) first = false;
        else out << ", ";
        out << elem;
    }
    out << "]";
    return out;
}

// Convenience optional<T> output
template<typename T>
std::ostream& operator<<(std::ostream& out, const std::optional<T>& value)
{
    if (value) out << "Some(" << *value << ")";
    else out << "None";
    return out;
}

// Convenience variant output
template<typename... Ts>
std::ostream& operator<<(std::ostream& out, const std::variant<Ts...>& value)
{
    std::visit([&out](const auto& v) { out << v; }, value);
    return out;
}

// Convenience pair output
template<typename T, typename U>
std::ostream& operator<<(std::ostream& out, const std::pair<T, U>& value)
{
    out << "(" << value.first << ", " << value.second << ")";
    return out;
}

// to_string helper
template<typename T>
[[nodiscard]] std::string to_string(const T& value)
{
    std::ostringstream oss;
    oss << value;
    return oss.str();
}

template<typename... Args>
static std::string to_string_sep(std::string_view sep, Args&&... args)
{
    std::ostringstream oss;
    auto delimit = [first = true, sep, &oss] () mutable
    {
        if (first) first = false;
        else oss << sep;
    };
    ((delimit(), oss << FWD(args)), ...);
    return oss.str();
}

/*
 * Report an error and exit.
 */
template<typename... Args>
[[noreturn]] void log_error(Args&&... args)
{
    error("%s", to_string_sep(" ", FWD(args)...).c_str());
}

/*
 * Report an error and exit. (Custom separator flavor)
 */
template<typename... Args>
[[noreturn]] void log_error_sep(std::string_view sep, Args&&... args)
{
    error("%s", to_string_sep(sep, FWD(args)...).c_str());
}

/*
 * Print a warning message.
 */
template<typename... Args>
void log_warn(Args&&... args)
{
    warning("%s", to_string_sep(" ", FWD(args)...).c_str());
}

/*
 * Print a warning message. (Custom separator flavor)
 */
template<typename... Args>
void log_warn_sep(std::string_view sep, Args&&... args)
{
    warning("%s", to_string_sep(sep, FWD(args)...).c_str());
}

/*
 * Print an info message.
 */
template<typename... Args>
void log_info(Args&&... args)
{
    info("%s\n", to_string_sep(" ", FWD(args)...).c_str());
}

/*
 * Print an info message. (Custom separator flavor)
 */
template<typename... Args>
void log_info_sep(std::string_view sep, Args&&... args)
{
    info("%s\n", to_string_sep(sep, FWD(args)...).c_str());
}

/*
 * Print to the standard output.
 */
template<typename... Args>
void print(Args&&... args)
{
    std::cout << to_string_sep(" ", FWD(args)...) << '\n';
}

/*
 * Print to the standard output. (Custom separator flavor)
 */
template<typename... Args>
void print_sep(std::string_view sep, Args&&... args)
{
    std::cout << to_string_sep(sep, FWD(args)...) << '\n';
}

/*
 * Print to the standard output. (Custom ending flavor)
 */
template<typename... Args>
void print_end(std::string_view ending, Args&&... args)
{
    std::cout << to_string_sep(" ", FWD(args)...) << ending;
}

/*
 * Memory functions
 */
void *xmalloc(size_t size);
void xfree(void *ptr);
char *xstrdup(const char *str);

/**
 * Hash combine function
 *
 * @param seed Original seed
 * @param v    The value to be appended
 *
 * @tparam T   The type of the new value
 */
template<typename T>
void hash_combine(std::size_t& seed, const T& v)
{
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

/**
 * Hash multiple values
 */
template<typename... Args>
std::size_t hash_multi(const Args&... args)
{
    std::size_t seed = 0;
    (hash_combine(seed, args), ...);
    return seed;
}

// Overload helper
template<typename... Ops>
struct overload : Ops...
{
    using Ops::operator()...;
};
template<typename... Ops>
overload(Ops...) -> overload<Ops...>;

// Random helper
static std::default_random_engine& get_random_engine()
{
    static std::default_random_engine engine{std::random_device{}()};
    return engine;
}

template<typename T>
[[nodiscard]] T random_int(T lo, T hi)
{
    std::uniform_int_distribution distrib{lo, hi};
    return distrib(get_random_engine());
}
template<typename T>
[[nodiscard]] T random(T hi)
{
    return random_int(T{0}, hi);
}

// String utilities
[[nodiscard]] inline std::string replace(std::string_view str, std::string_view from, std::string_view to)
{
    std::string result{str};
    if (from.empty() || str.empty()) return result;
    std::size_t start_pos = 0;
    while ((start_pos = result.find(from, start_pos)) != std::string::npos)
    {
        result = result.replace(start_pos, from.length(), to);
        start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
    return result;
}

[[nodiscard]] inline std::string unescape(std::string_view str, bool is_char = false)
{
    // TODO: More escape characters
    static std::unordered_map<std::string, std::string> escapes = {
        { "\\\\" , "\\" },
        { "\\n", "\n" },
        { "\\r", "\r" },
        { "\\t", "\t" }
    };
    std::string result{str};
    for (const auto& [orig, post] : escapes)
        result = replace(result, orig, post);
    if (is_char) result = replace(result, "'", "");
    else result = replace(result, "\"", "");
    return result;
}

[[nodiscard]] inline std::string to_lowercase(std::string_view str)
{
    std::string result(str.size(), '\0');
    for (int i = 0; i < str.size(); ++i) result[i] = std::tolower(str[i]);
    return result;
}

/**
 * Parse an integer or double from string
 *
 * @param str String to be parsed
 * @return    64-bit signed integer or floating point value
 */
[[nodiscard]] std::variant<std::int64_t, double> parse_number(std::string_view str);

// itertools helper
template<typename T>
void cartesian_product_helper(
    std::vector<std::vector<T>>& accum, std::vector<T> stack,
    const std::vector<std::vector<T>>& sequences, std::size_t index
)
{
    for (const auto& i : sequences[index])
    {
        stack.push_back(i);
        if (index + 1 == sequences.size())
            accum.push_back(stack);
        else
            cartesian_product_helper(accum, stack, sequences, index + 1);
        stack.pop_back();
    }
}

/**
 * Cartesian product of elements
 *
 * @tparam T    Element type
 * @param items Elements to be processed
 * @return      All possible combinations of elements
 */
template<typename T>
[[nodiscard]] std::vector<std::vector<T>> cartesian_product(const std::vector<std::vector<T>>& items)
{
    std::vector<std::vector<T>> accum;
    std::vector<T> stack;
    if (items.size() > 0)
        cartesian_product_helper(accum, stack, items, 0);
    return accum;
}

#endif //AUTOBUG_SYMBOLIC_COMMON_H
