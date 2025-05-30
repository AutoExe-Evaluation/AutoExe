#include "coverage_map.h"
#include "parser.h"

std::unordered_set<Trace> COVERAGE_MAP;

std::string Trace::to_string() const
{
    std::string result = "<";
    std::string last_name;
    bool first = true, inner_first = true;
    for (auto it = trace.rbegin(); it != trace.rend(); ++it)
    {
        const auto& entry = *it;
        if (entry.file != last_name)
        {
            if (first) first = false;
            else result += "/";
            result += getBasename(entry.file); result += ":";
            last_name = entry.file;
            inner_first = true;
        }
        if (inner_first) inner_first = false;
        else result += ",";
        result += ::to_string(entry.line);
        if (entry.event == Event::CLL)
            result += " (CLL)";
        else if (entry.event == Event::RET)
            result += " (RET)";
    }
    result += ">";
    return result;
}