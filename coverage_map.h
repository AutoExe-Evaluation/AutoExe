#ifndef AUTOBUG_SYMBOLIC_COVERAGE_MAP_H
#define AUTOBUG_SYMBOLIC_COVERAGE_MAP_H

#include <iostream>
#include <string>
#include <unordered_set>
#include <utility>

#include "AST.h"
#include "AutoBug.h"

template<typename T, typename U>
struct std::hash<std::pair<T, U>>
{
    std::size_t operator()(const std::pair<T, U>& pair) const
    {
        return hash_multi(pair.first, pair.second);
    }
};

class Trace
{
private:
    TRACE trace;

public:
    using coverage_t = std::unordered_set<std::pair<std::string, uint32_t>>;

    Trace() = default;
    explicit Trace(TRACE trace) :trace{MOVE(trace)} {}

    void addTrace(const File& file, uint32_t lineno, Event type = Event::EXE)
    {
        trace.push_front({file.path, lineno, 1, type});
    }
    void addTrace(const Node& node, Event type = Event::EXE)
    {
        const File& file = File::get(node.id);
        auto lineno = file.lineno(node.lo);
        addTrace(file, lineno, type);
    }

    std::string to_string() const;
    friend std::ostream& operator<<(std::ostream& out, const Trace& self)
    {
        out << self.to_string();
        return out;
    }

    coverage_t getCoverage() const
    {
        coverage_t cov;
        for (const auto& entry : trace)
            cov.emplace(entry.file, entry.line);
        return cov;
    }

    friend bool operator==(const Trace& lhs, const Trace& rhs)
    {
        return lhs.getCoverage() == rhs.getCoverage();
    }
    friend bool operator!=(const Trace& lhs, const Trace& rhs)
    {
        return lhs.getCoverage() != rhs.getCoverage();
    }

    std::string getSlice() const
    {
        bool prev = option_quiet; option_quiet = true;
        summarizeTrace(default_config(), trace, get_file_buf_handle(), nullptr);
        option_quiet = prev;
        log_info_sep("", "Generated slice for ", *this, ":");
        std::string result{get_file_buf()};
        log_info(result);
        return result;
    }
};

template<>
struct std::hash<Trace>
{
    std::size_t operator()(const Trace& trace) const
    {
        std::size_t hash_sum{};
        for (const auto& cov : trace.getCoverage())
            hash_sum ^= std::hash<Trace::coverage_t::key_type>{}(cov);
        return hash_sum;
    }
};

extern std::unordered_set<Trace> COVERAGE_MAP;

#endif //AUTOBUG_SYMBOLIC_COVERAGE_MAP_H
