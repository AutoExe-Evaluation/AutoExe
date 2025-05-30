#include <cassert>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <vector>
#include <set>
#include <optional>
#include <string>

#include <argparse/argparse.hpp>
#include <nlohmann/json.hpp>

#include "parser.h"
#include "symbolic-executor.h"
#ifndef USE_SYMBOLIC_EXEC
#include "simple-executor.h"
#endif

using json = nlohmann::json;
namespace fs = std::filesystem;

#ifdef USE_SYMBOLIC_EXEC
void test_z3()
{
    print("find_model_example1");
    z3::context c;
    z3::expr x = c.int_const("x");
    z3::expr y = c.int_const("y");
    z3::solver s(c);

    s.add(x >= 1);
    s.add(y < x + 3);
    print("s =", s);
    print("s.check():", s.check());

    z3::model m = s.get_model();
    print("m =", m);

    // traversing the model
    for (int i = 0; i < m.size(); i++)
    {
        z3::func_decl v = m[i];
        // this problem contains only constants
        assert(v.arity() == 0);
        print(v.name(), "=", m.get_const_interp(v));
    }
    // we can evaluate expressions in the model.
    print("x + y + 1 =", m.eval(x + y + 1));
}

void test_solver()
{
    print("\nSolver Test");
    z3::context ctx;
    auto n = ctx.int_const("n");
    auto idx = ctx.int_const("idx");
    auto seqs = z3_sequences("seq other_seq", ctx);
    const auto& seq = seqs[0];
    const auto& other_seq = seqs[1];

    auto formula = z3::forall(
        seq, n, other_seq,
        z3::implies(
            z3::exists(idx, idx >= 0 && idx < seq.length() && seq[idx] == n),
            z3::exists(idx, z3::concat(seq, other_seq)[idx] == n)
        )
    );
    print(formula);
    print();

    auto check1 = check_z3(formula, ctx);
    print("check(formula) =", check1);
    auto check2 = is_unsat_z3(!formula, ctx);
    print("is_unsat_z3(!formula) =", check2 ? "true" : "false");
}
#endif

std::vector<const Func*> getAllFuncs(const File* file)
{
    std::set<std::string, std::less<>> funcNames;
    std::vector<const Func*> funcs;
    for (auto linepos : file->lines)
    {
        auto lineno = file->lineno(linepos);
        Chunk line(*file, file->pos(lineno).pos, file->pos(lineno + 1).pos, LINE);
        auto func = getFunc(line);
        if (!func) continue;
        if (auto funcName = func->name.str(); funcNames.find(funcName) == funcNames.end())
        {
            funcNames.insert(funcName);
            funcs.push_back(func);
        }
    }
    return funcs;
}

void match_example(const File* file)
{
    print("\nMatch AST Example");
    for (auto func : getAllFuncs(file))
    {
        log_info("\nCurrent function:", func->name.str());
        printNodeTree(*func);
        log_info();

        if (auto result = match_ast(func, {ASSIGN, IDENTIFIER, BINARY_EXPR}))
        {
            log_info("Assignment expression found:");
            int cnt = 0;
            for (auto node : *result)
            {
                ++cnt;
                log_info_sep("", cnt, ": ", node->str());
            }
        }
        else log_info("No assignment expression found.");
    }
}

#ifdef USE_SYMBOLIC_EXEC
void test_symbolic()
{
    print("\nSymbolic Execution Test");
    z3::context ctx;
    Variable x{"x", TypeIndex::INT};
    Variable y{"y", TypeIndex::INT};
    SymbolicEnvironment env{
        SymbolicStore{{{x, x.to_z3(ctx)}, {y, y.to_z3(ctx)}}}
    };
    print("Env before:", env);

    // x = 2 * y
    env = env.set(x, 2 * y.to_z3(ctx), ctx);
    print("Env after x = 2 * y:", env);

    // if x < 0:
    auto env_1 = env.add_constraint(
        env.get_store().apply_to(x.to_z3(ctx) < ctx.int_val(0), ctx), ctx);
    auto env_2 = env.add_constraint(
        env.get_store().apply_to(!(x.to_z3(ctx) < ctx.int_val(0)), ctx), ctx);
    print("Branch x < 0:", env_1);
    print("Other branch:", env_2);

    // x = -x
    auto env_1_1 = env_1.set(x, ctx.int_val(-1) * x.to_z3(ctx), ctx);
    print("Env after x = -x:", env_1_1);

    // Concretize
    auto env_concrete1 = env_1_1.concretize(
        ValueStore{{{y, ValueType{-2}}}}, ctx
    );
    print("Concretize with y = -2:", env_concrete1);
    auto env_concrete2 = env_1_1.concretize(
        ValueStore{{{y, ValueType{2}}}}, ctx
    );
    print("Concretize with y = 2:", env_concrete2);
}

void test_interpreter(const File* file)
{
    print("\nInterpreter Test");
    auto func = getAllFuncs(file)[0];
    if (!func) error("No function present!");
    auto body = dynamic_cast<Block*>(func->child.back().get());
    if (!body) error("No function body present!");

    Variable x{"x", TypeIndex::INT};
    Variable y{"y", TypeIndex::INT};
    z3::context ctx;
    SymbolicEnvironment env{
        SymbolicStore{{{x, 2 * y.to_z3(ctx)}, {y, y.to_z3(ctx)}}}
    };
    print("Env begin:", env);
    auto result1 = concur(env, body, ValueStore{{{y, ValueType{-2}}}}, ctx);
    print("Evaluate result with y = -2:", result1);
    auto result2 = concur(env, body, ValueStore{{{y, ValueType{2}}}}, ctx);
    print("Evaluate result with y = 2:", result2);
}
#endif

std::string test_symbolic_exec(
    const File* file, bool skip_slice = false,
    std::string_view entry_point = "entry", bool auto_entry = false
)
{
    print("\nSymbolic Interpreter Test");

    if (skip_slice)
    {
        std::string result_str;
        if (check_llm(std::string_view{file->file.begin(), file->file.size()}) == z3::unsat)
            result_str = "unsat: 1";
        else result_str = "sat: 1";
        print("Verification result count:", result_str);
        return result_str;
    }

    Variable x{"x", TypeIndex::INT};
    Variable y{"y", TypeIndex::INT};
    z3::context ctx;
#ifdef USE_SYMBOLIC_EXEC
    SymbolicEnvironment env{
    //    SymbolicStore{{{x, x.to_z3(ctx)}, {y, y.to_z3(ctx)}}}
    };
    SymbolicInterpreter interpreter{ctx, env};
#else
    FunctionRepo functions;
    SimpleInterpreter interpreter;
#endif
    const Func* target = nullptr;
    std::string entry_name{entry_point};
    if (auto_entry)
    {
        std::string last;
        for (auto func : getAllFuncs(file))
            last = func->name.str();
        log_info("Automatically selected entry:", last);
        entry_name = last;
    }
    for (auto func : getAllFuncs(file))
        if (func->name.str() != entry_name)
        {
            auto result = interpreter.execute(func, {});
#ifdef USE_SYMBOLIC_EXEC
            env = result.get_env();
            interpreter.set_environment(env);
#else
            functions = result.get_functions();
            interpreter.set_functions(functions);
#endif
        }
        else target = func;
    if (!target) error("No entry function present!");
    for (int i = 0; i + 1 < target->child.size(); ++i)
    {
        const auto& param = target->child[i];
        auto decl = param->str();
#ifdef PYTHON_PARSER
        auto name = decl;
#else
        auto index = decl.find(' ');
        if (index == std::string::npos) log_error("Invalid decl:", decl);
        auto name = decl.substr(index + 1);
#endif
        Variable var{name, TypeIndex::LIST};
#ifdef USE_SYMBOLIC_EXEC
        env = env.set(var, var.to_z3(ctx), ctx);
        print("Add variable:", name);
#endif
    }
#ifdef USE_SYMBOLIC_EXEC
    interpreter.set_environment(env);
#endif
    auto body = dynamic_cast<Block*>(target->child.back().get());
    if (!body) error("No function body present!");

#ifdef USE_SYMBOLIC_EXEC
    print("Env begin:", interpreter.get_environment());
#else
    print("Functions begin:", interpreter.get_functions());
#endif
    auto tree = interpreter.execute(body, {});
    print("Execution tree:");
    COVERAGE_MAP.clear();
#ifdef USE_SYMBOLIC_EXEC
    display_set(&tree, ctx);
#else
    display_simple(&tree);
#endif

    print("\nUnique traces generated:");
    int cnt = 0;
    for (const auto& trace : COVERAGE_MAP)
        print_sep("", ++cnt, ": ", trace);

    print();
    std::vector<std::pair<Trace, std::string>> slices;
    for (const auto& trace : COVERAGE_MAP)
    {
        auto slice = trace.getSlice();
        slices.emplace_back(trace, slice);
    }
    std::sort(slices.begin(), slices.end(), [](const auto& a, const auto& b) {
        const auto& slice1 = a.second;
        const auto& slice2 = b.second;
        return slice1.size() < slice2.size() || (slice1.size() == slice2.size() && slice1 < slice2);
    });

    print();
    std::map<z3::check_result, int> result_cnt;
    // bool verified = true;
    for (const auto& [trace, slice] : slices)
    {
        print("\n=====> Verification for trace:", trace, "<=====");
        /*if (check_llm(slice) != z3::sat)
        {
            verified = false;
            break;
        }*/
        ++result_cnt[check_llm(slice)];
    }
    // print("Verification result:", verified ? "True" : "False");
    std::vector<std::pair<z3::check_result, int>> result_vec(result_cnt.begin(), result_cnt.end());
    std::sort(result_vec.begin(), result_vec.end(), [](const auto& a, const auto& b) {
        const auto& cnt1 = a.second;
        const auto& cnt2 = b.second;
        return cnt1 > cnt2 || (cnt1 == cnt2 && a.first < b.first);
    });
    std::string result_str;
    bool first = true;
    for (const auto& [result, count] : result_vec)
    {
        if (first) first = false;
        else result_str += ", ";
        result_str += ::to_string(result) + ": " + ::to_string(count);
    }
    print("Verification result count:", result_str);
    return result_str;
}

int main(int argc, char** argv)
{
    argparse::ArgumentParser parser{argv[0]};
    parser.add_argument("path_to_source_dir").help("Directory containing the sources");
    parser.add_argument("path_to_source").help("File containing the source to analyze");
    parser.add_argument("--skip-slice").help("Skip slicing and send the whole program to LLM").flag();
    auto& group = parser.add_mutually_exclusive_group();
    group.add_argument("--entry-point").help("Name of the function that serves as the entry point")
         .default_value(std::string{"entry"});
    group.add_argument("--auto-entry").help("Automatically select the last function as the entry point").flag();
    parser.add_argument("--model").help("Name of the LLM model to be used")
          .default_value(std::string{"gpt-4o-mini"});
    parser.add_argument("--output").help("Output file for results");
    parser.add_argument("--result-output").help("Output JSON file for query text and results");

    try
    {
        parser.parse_args(argc, argv);
    }
    catch (const std::exception& e)
    {
        print(e.what());
        return EXIT_FAILURE;
    }
    llm_model = parser.get<std::string>("--model");
    print("Using model:", llm_model);

    // test_z3();
    // test_solver();
    print();

    buildIndex(argv[1]);
    auto file = loadFile(argv[2]);
    if (!file)
    {
        print(std::string{argv[1]} + " does not exist!");
        return EXIT_FAILURE;
    }
    match_example(file);

    // test_symbolic();
    // test_interpreter(file);
    auto result_str = test_symbolic_exec(
        file, parser["--skip-slice"] == true,
        parser.get<std::string>("--entry-point"), parser["--auto-entry"] == true
    );
    result_str += "\n";
    bool first = true;
    json total = json::array();
    for (const auto& [query, result] : LLM_QUERIES)
    {
        if (first) first = false;
        else result_str += ", ";
        result_str += std::to_string(query.size());
        json o;
        o["query_text"] = query;
        o["query_result"] = ::to_string(result);
        total.push_back(o);
    }
    if (auto result_file = parser.present("--output"))
    {
        std::ofstream out{*result_file};
        out << result_str << "\n";
    }
    else
        print("Result:", result_str);
    if (auto result_file = parser.present("--result-output"))
    {
        json json_dict;
        if (!fs::exists(*result_file)) json_dict = json::array();
        else
        {
            std::ifstream in{*result_file};
            json_dict = json::parse(in);
        }
        json_dict.push_back(total);
        std::ofstream out{*result_file};
        out << std::setw(4) << json_dict << '\n';
    }
    return EXIT_SUCCESS;
}

