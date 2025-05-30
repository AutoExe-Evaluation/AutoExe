#include <cstdlib>

#include <getopt.h>

#include "AutoBug.h"
#include "parser.h"

static void summarizeTraceOut(const Config &config, const TRACE &T)
{
    FILE *out = stdout;
    if (strcmp(config.output, "-") != 0)
    {
        out = fopen(config.output, "w");
        if (out == nullptr)
            error("failed to open output file \"%s\" for writing: %s",
                  config.output, strerror(errno));
    }
    FILE *out_chunks = nullptr;
    bool out_chunks_opened = false;
    if (config.output_chunks)
    {
        if (strcmp(config.output_chunks, "-") == 0)
            out_chunks = stdout;
        else
        {
            out_chunks = fopen(config.output_chunks, "w");
            if (out_chunks == nullptr)
                error("failed to open output chunks file \"%s\" for writing: %s",
                      config.output_chunks, strerror(errno));
            out_chunks_opened = true;
        }
    }
    summarizeTrace(config, T, out, out_chunks);
    fclose(out);
    if (out_chunks_opened) fclose(out_chunks);
}

/*
 * Parse integer from string.
 */
static int64_t parseInt(const char *opt, const char *val, int64_t lb, int64_t ub)
{
    bool neg = (val[0] == '-');
    val += (neg? 1: 0);
    char *end = NULL;
    errno = 0;
    int64_t r = strtoull(val, &end, 0);
    r = (neg? -r: r);
    if (errno != 0 || end == NULL || *end != '\0' || r < lb || r > ub)
        error("failed to parse `%s' option; expected integer within the "
              "range %zd..%zd; found \"%s\"", opt, lb, ub, val);
    return r;
}

/*
 * Parse a Boolean from a string.
 */
static bool parseBool(const char *opt, const char *val)
{
    if (val == nullptr)
        return true;
    if (strcmp(val, "true") == 0 || strcmp(val, "1") == 0)
        return true;
    if (strcmp(val, "false") == 0 || strcmp(val, "0") == 0)
        return false;
    error("failed to parse `%s' option; expected true or false; "
          "found \"%s\"", opt, val);
}

/*
 * Options.
 */
enum Option
{
    OPTION_CONTEXT,
    OPTION_LABEL_CRASH,
    OPTION_LABEL_FIX,
    OPTION_OUTPUT,
    OPTION_OUTPUT_CHUNKS,
    OPTION_TOKENS,
    OPTION_TTY,
    OPTION_WINDOW,
    OPTION_QUIET,
    OPTION_HELP,
};

/*
 * Usage message.
 */
static void usage(FILE *stream, const char *progname)
{
    fprintf(stream,
            "%susage%s: %s [OPTIONS] src-path/ crash.trace\n"
            "\n"
            "OPTIONS:\n"
            "\t--context[=false]\n"
            "\t\tPrint revelant context (types/defines/globals/etc.).\n"
            "\t\tDefault: true\n"
            "\t--label-crash LABEL\n"
            "\t\tAnnotate the crash location with LABEL.\n"
            "\t\tDefault: \"CRASH LOCATION\"\n"
            "\t--label-fix LABEL\n"
            "\t\tAnnotate the fix location(s) with LABEL.\n"
            "\t\tDefault: (none)\n"
            "\t--output FILE, -o FILE\n"
            "\t\tWrite result to FILE.\n"
            "\t\tDefault: FILE=stdout\n"
            "\t--output-chunks FILE\n"
            "\t\tWrite chunk ranges to FILE.\n"
            "\t\tDefault: (none)\n"
            "\t--tokens NUM, -T NUM\n"
            "\t\tLimit output to NUM tokens.\n"
            "\t\tDefault: 1000\n"
            "\t--window WINDOW, -w WINDOW\n"
            "\t\tPrint an additional WINDOW lines of context around the crash "
            "location.\n"
            "\t\tDefault: WINDOW=0\n"
            "\t--tty[=false]\n"
            "\t\tForce terminal mode (prints colors)\n"
            "\t\tDefault: off\n"
            "\t--quiet, -q\n"
            "\t\tQuieter output (only output the final slice)"
            "\t\tDefault: off\n"
            "\t--help, -h\n"
            "\t\tPrint this help message.\n"
            "\n",
            YELLOW, OFF, progname);
}

/*
 * Main.
 */
int main(int argc, char **argv)
{
    option_tty = isatty(STDERR_FILENO);
    int option_window = 0;
    size_t option_tokens = 1000;
    std::string option_output("-");
    std::string option_output_chunks;
    std::string option_label_crash("CRASH LOCATION");
    std::string option_label_fix("");
    bool option_context = true;

    const int req_arg = required_argument, opt_arg = optional_argument,
        no_arg = no_argument;
    static const struct option long_options[] =
        {
            {"context",       opt_arg, nullptr, OPTION_CONTEXT},
            {"label-crash",   req_arg, nullptr, OPTION_LABEL_CRASH},
            {"label-fix",     req_arg, nullptr, OPTION_LABEL_FIX},
            {"output",        req_arg, nullptr, OPTION_OUTPUT},
            {"output-chunks", opt_arg, nullptr, OPTION_OUTPUT_CHUNKS},
            {"tokens",        req_arg, nullptr, OPTION_TOKENS},
            {"tty",           opt_arg, nullptr, OPTION_TTY},
            {"window",        req_arg, nullptr, OPTION_WINDOW},
            {"quiet",          no_arg, nullptr, OPTION_QUIET},
            {"help",           no_arg, nullptr, OPTION_HELP},
            {nullptr,         no_arg, nullptr, 0}
        };

    while (true)
    {
        int idx;
        int opt = getopt_long(argc, argv, "o:w:T:qh", long_options, &idx);
        if (opt < 0)
            break;
        switch (opt)
        {
            case OPTION_CONTEXT:
                option_context = parseBool("--context", optarg);
                break;
            case OPTION_LABEL_CRASH:
                option_label_crash = optarg;
                break;
            case OPTION_LABEL_FIX:
                option_label_fix = optarg;
                break;
            case OPTION_OUTPUT: case 'o':
                option_output = optarg;
                break;
            case OPTION_OUTPUT_CHUNKS:
                option_output_chunks = optarg ? optarg : "-";
                break;
            case OPTION_TOKENS: case 'T':
                option_tokens = parseInt("--tokens", optarg, 0, MAX_TOKENS);
                break;
            case OPTION_TTY:
                option_tty = parseBool("--tty", optarg);
                break;
            case OPTION_WINDOW: case 'w':
                option_window = parseInt("--window", optarg, 0, 1000);
                break;
            case OPTION_QUIET: case 'q':
                option_quiet = true;
                break;
            case OPTION_HELP:
                usage(stdout, argv[0]);
                exit(EXIT_SUCCESS);
            default:
                usage(stderr, argv[0]);
                exit(EXIT_FAILURE);
        }
    }

    if (argv[optind+0] == nullptr)
        error("missing source path; try `%s --help' for more information",
              argv[0]);
    if (argv[optind+1] == nullptr)
        error("missing trace file; try `%s --help' for more information",
              argv[0]);
    if (argv[optind+2] != nullptr)
        error("extraneous argument \"%s\"; try `%s --help' for more "
              "information", argv[optind+2], argv[0]);

    const char *path = argv[optind+0], *trace = argv[optind+1];
    buildIndex(path);
    TRACE T;
    parseTrace(trace, T);

    Config config = {0};
    config.context       = option_context;
    config.window        = option_window;
    config.tokens        = option_tokens;
    config.label.crash   =
        (option_label_crash == ""? nullptr: option_label_crash.c_str());
    config.label.fix     =
        (option_label_fix == ""? nullptr: option_label_fix.c_str());
    config.output        = option_output.c_str();
    config.output_chunks = option_output_chunks.empty() ? nullptr : option_output_chunks.c_str();

    summarizeTraceOut(config, T);

    return 0;
}

