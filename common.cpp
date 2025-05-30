#include <cassert>
#include <cerrno>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <charconv>
#include <string>
#include <vector>

#include <getopt.h>
#include <sys/mman.h>
#include <unistd.h>

#include "common.h"

/*
 * Report an error and exit.
 */
[[noreturn]] void error(const char *msg, ...)
{
    fprintf(stderr, "%serror%s: ", RED, OFF);
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    putc('\n', stderr);
    exit(EXIT_FAILURE);
}

/*
 * Print a warning message.
 */
void warning(const char *msg, ...)
{
    fprintf(stderr, "%swarning%s: ", YELLOW, OFF);
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    putc('\n', stderr);
}

/*
 * Print an info message.
 */
void info(const char *msg, ...)
{
    if (option_quiet) return;
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
}

void *xmalloc(size_t size)
{
    void *ptr = malloc(size);
    if (ptr == NULL)
        error("failed to allocated %zu bytes of memory: %s", size,
              strerror(errno));
    return ptr;
}

void xfree(void *ptr)
{
    free(ptr);
}

char *xstrdup(const char *str)
{
    size_t len = strlen(str);
    char *alt = (char *)xmalloc(len+1);
    memcpy(alt, str, len+1);
    return alt;
}

/**
 * Parse an integer or double from string
 *
 * @param str String to be parsed
 * @return    64-bit signed integer or floating point value
 */
[[nodiscard]] std::variant<std::int64_t, double> parse_number(std::string_view str)
{
    std::int64_t value = 0;
    if (auto [_, ec] = std::from_chars(str.data(), str.data() + str.size(), value);
        ec == std::errc{})
        return value;

    // from_chars FP is not available in most implementations; use stod for now
    try
    {
        return std::stod(std::string{str});
    }
    catch (std::exception& e)
    {
        log_error_sep("", "Invalid number literal ", str, ": ", e.what());
    }
}