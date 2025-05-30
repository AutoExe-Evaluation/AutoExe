#ifndef AUTOBUG_SYMBOLIC_AUTOBUG_H
#define AUTOBUG_SYMBOLIC_AUTOBUG_H

#include <cstdio>
#include <deque>
#include <memory>

#include "common.h"

/*
 * Configuration.
 */
struct Config
{
    bool context;              // Print context?
    int window;                // Window size
    size_t tokens;             // Max tokens
    struct
    {
        const char *crash;     // Crash label
        const char *fix;       // Fix label
    } label;
    const char *output;        // Output filename
    const char *output_chunks; // Output chunks filename
};

static constexpr size_t MAX_TOKENS = 6400000000;

inline Config default_config()
{
    return {false, 1000, MAX_TOKENS, {"TARGET POINT", nullptr}, nullptr, nullptr};
}

inline char* get_file_buf()
{
    static auto buf = std::make_unique<char[]>(MAX_TOKENS);
    return buf.get();
}

inline FILE* get_file_buf_handle()
{
    static auto file_ptr = fmemopen(get_file_buf(), MAX_TOKENS, "w");
    if (file_ptr == nullptr) log_error("Failed to open buffer stream");
    rewind(file_ptr);
    return file_ptr;
}

enum class Event
{
    EXE,
    CLL,
    RET
};

struct ENTRY
{
    const char *file;
    unsigned line;
    int thread;
    Event event;
};

typedef std::deque<ENTRY> TRACE;

/*
 * Parse a trace file.
 */
void parseTrace(const char *filename, TRACE &T);

/*
 * Summarize a trace.
 */
void summarizeTrace(const Config &config, const TRACE &T, FILE* out, FILE* out_chunks);

#endif //AUTOBUG_SYMBOLIC_AUTOBUG_H
