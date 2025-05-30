#include <sstream>
#include <unordered_map>

#include "liboai.h"

#include "constraint-solver.h"

// #define USE_OPENAI
// #define USE_DEEPINFRA
#define USE_LOCAL_OLLAMA

static liboai::OpenAI& get_openai()
{
    static liboai::OpenAI oai
#ifdef USE_LOCAL_OLLAMA
        ("http://localhost:11434/v1")
#elif defined(USE_DEEPINFRA)
        ("https://api.deepinfra.com/v1/openai")
#elif !defined(USE_OPENAI)
        ("http://127.0.0.1:8080")
#endif
    ;
    oai.auth.SetMaxTimeout(1000 * 12000);
#ifdef USE_OPENAI
    if (!oai.auth.SetKeyFile(std::filesystem::path{std::getenv("HOME")} / "openai_key.txt"))
       log_error("OpenAI Authorization Failed!");
#elif defined(USE_DEEPINFRA)
    if (!oai.auth.SetKeyFile(std::filesystem::path{std::getenv("HOME")} / "deepinfra_key.txt"))
        log_error("DeepInfra Authorization Failed!");
#endif
    return oai;
}

static liboai::Conversation& get_conv()
{
    static liboai::Conversation conv;
    if (!conv.SetSystemData(
#if !defined(USE_OPENAI) && !defined(USE_LOCAL_OLLAMA)
        "<｜User｜>"
#endif
        "You are acting as a constraint solver that can determine whether a given postcondition "
        "is guaranteed to be true after executing the given code segment. "
        "In the code segment, preconditions (if present) is denoted by an assert() statement with // PRE comment, "
        "and postconditions (if present) is denoted by an assert() statement with // POST comment. "
        "You should assume that any input to the function will satisfy the precondition. "
        "Please respond one of verifiable, unverifiable or unknown as the final line of the output. "
        "Please put your response on the final line of the output; do not add any format or explanation after that. "
        "In addition, if the result is unverifiable, please provide at least one counterexample (an input to the given function) that cause the postcondition to fail. "
        "The format of the final line should either be: \"Result: verifiable\", \"Result: unknown\", and \"Result: unverifiable, with counter example: <your counterexample>\" "
        "Do not put any format specifier like bold or box on the final line; it should be pure text."
        "All results should be written on the final line without any formatting such as code blocks or bold text. "
        "PLEASE ENSURE your final line of output is of the above form with one of verifiable, unverifiable or unknown. "
        "Importantly, please disregard any executing path that cause the program to exit before reaching the postcondition assert. "
        "Specifically, unreachable() in the program denotes a point/branch in code that is unreachable and should be treated as an implicit precondition that input will guarantee that this point of code is never reached."
        "REPEAT: PLEASE disregard the case where the postcondition cannot be reached; these represents implicit preconditions, and should not be treated as postcondition violation. "
        "For instance, if (x > 1) { assert(0); } else { ... } should be treated as an implicit precondition that x is never > 1, as the assert(0) is assumed to be never reachable. "
        "For example, if len(value) > 1: assert False should be treated as an implicit precondition that len(value) <= 1, instead of treating as unverifiable. "
        "Such cases should be treated as implicit preconditions instead of treated as postcondition violations. "
        "In other words, it should be assumed that inputs will guarantee that such branch will never be entered. "
        "**In other words**, you should not treat assert False as a postcondition violation; please **assume** that the input given will guarantee that branch stating assert False is not reachable. "
        // "Please disregard any comments like # ... and /* ... */; they represents irrelevant code segments that does not affect the verification process."
        // "These gap comments should be **ignored**; they do not represent unknown code pieces. "
        "Note that in this setting, verifiable means that for all possible assignments of the symbolic variables that satisfy the precondition, "
        "the given postcondition always hold, and unverifiable means that there are at least one possible assignment (satisfying the precondition) "
        "that can allow the given postcondition to not hold. "
        // "Contradiction should **only** be reported if the postcondition cannot be reached at all with any valid input satisfying the precondition."
#if !defined(USE_OPENAI) && !defined(USE_LOCAL_OLLAMA)
        "<｜Assistant｜>"
#endif
    ))
        log_error("System data addition failed.");
    return conv;
}

static std::pair<std::string, bool> is_unsat_openai(std::string_view slice)
{
    static const auto& oai = get_openai();
    static auto& conv = get_conv();
    static std::unordered_map<std::string, std::string> response_cache;
    auto str = to_string(slice);
    auto query =
#if !defined(USE_OPENAI) && !defined(USE_LOCAL_OLLAMA)
        "<｜User｜>"
#endif
        "Please analyze the following code: " + str
#if !defined(USE_OPENAI) && !defined(USE_LOCAL_OLLAMA)
        + "<｜Assistant｜>"
#endif
        ;
    log_info("Sending to LLM with the following message:", query);
    if (auto it = response_cache.find(str); it != response_cache.end())
    {
        log_info("Cached response:", it->second);
        return {it->second, false};
    }
    LLM_QUERIES.emplace_back(query, z3::unknown);
    if (!conv.AddUserData(query))
        log_error("User data addition failed.");
    if (!conv.Update(oai.ChatCompletion->create(
#if defined(USE_OPENAI) || defined(USE_LOCAL_OLLAMA)
        llm_model
#elif defined(USE_DEEPINFRA)
        // "deepseek-ai/DeepSeek-R1-Distill-Llama-70B"
        "meta-llama/Llama-3.3-70B-Instruct"
#else
        "deepseek-r1"
#endif
    , conv)))
        log_error("Response update failed!");
    auto response = conv.GetLastResponse();
    log_info("Response:", response);
    response_cache[str] = response;
    return {response, true};
}

/**
 * Check a constraint using Z3
 *
 * @param formula    The constraint to check
 * @param ctx        Z3 context to be used
 * @param timeout_ms Timeout for Z3 in milliseconds
 * @param tries      Number of tries permitted
 * @return           The result of the check
 */
z3::check_result check_z3(const z3::expr& formula, z3::context& ctx,
                          unsigned int timeout_ms, unsigned int tries)
{
    if (tries == 0) return z3::unknown;

    z3::solver solver{ctx};
    auto timeout = random_int(150u, timeout_ms);
    solver.set("timeout", timeout);
    solver.add(formula);

    log_info_sep("", "Z3 Solving ", to_string(formula), " with timeout ", timeout, "... Remaining tries: ", tries);
    if (auto result = solver.check(); result != z3::unknown) return result;
    return check_z3(formula, ctx, timeout_ms, tries - 1);
}

/**
 * Check a constraint using LLM
 *
 * @param slice      Current code slice
 * @return           The result of the check
 */
z3::check_result check_llm(std::string_view slice)
{
    auto [result, updated] = is_unsat_openai(slice);

    // for now, we assume that the final line contains the answer
    int index = static_cast<int>(result.size()) - 1;
    while (index >= 0 && !std::isalnum(result[index])) --index;
    if (index < 0) log_error("Empty response!");
    auto find_result = result.rfind('\n', index);
    if (find_result == std::string::npos) find_result = 0;
    auto final_line = result.substr(find_result);
    if (to_lowercase(final_line).find("unverifiable") != std::string::npos)
    {
        log_info("Detected result: unsat");
        if (updated) LLM_QUERIES.back().second = z3::unsat;
        return z3::unsat;
    }
    if (to_lowercase(final_line).find("verifiable") != std::string::npos)
    {
        log_info("Detected result: sat");
        if (updated) LLM_QUERIES.back().second = z3::sat;
        return z3::sat;
    }
#if 0
    if (to_lowercase(final_line).find("contradiction") != std::string::npos)
    {
        log_info("Detected result: impossible");
        if (updated) LLM_QUERIES.back().second = z3::sat;
        return z3::sat;
    }
#endif
    log_info("Detected result: unknown");
    return z3::unknown;
}

/**
 * Make a Z3 sequence
 *
 * @param name Name of the sequence
 * @param ctx  Z3 context to be used
 * @return     An expr referencing a sequence
 */
z3::expr z3_sequence(std::string_view name, z3::context& ctx)
{
    auto elem_sort = ctx.string_sort();
    return z3::to_expr(ctx, Z3_mk_const(
        ctx, Z3_mk_string_symbol(ctx, name.data()),
        Z3_mk_seq_sort(ctx, elem_sort)
    ));
}

/**
 * Make several Z3 sequences
 *
 * @param names Names of the sequences
 * @param ctx   Z3 context to be used
 * @return      A list of expr referencing the sequences
 */
std::vector<z3::expr> z3_sequences(const std::vector<std::string>& names, z3::context& ctx)
{
    std::vector<z3::expr> result; result.reserve(names.size());
    for (const auto& name : names)
        result.push_back(z3_sequence(name, ctx));
    return result;
}

/**
 * Make several Z3 sequences
 *
 * @param names Names of the sequences delimited by whitespaces
 * @param ctx   Z3 context to be used
 * @return      A list of expr referencing the sequences
 */
[[nodiscard]] std::vector<z3::expr> z3_sequences(const std::string& names, z3::context& ctx)
{
    std::istringstream iss{names};
    std::string temp;
    std::vector<z3::expr> result;
    while (iss >> temp)
        result.push_back(z3_sequence(temp, ctx));
    return result;
}
