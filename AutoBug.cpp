#include <cassert>
#include <cerrno>
#include <cctype>
#include <climits>
#include <cstdint>
#include <cstring>

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "AutoBug.h"
#include "itree.h"
#include "parser.h"

/********************************************************************/
/* SUMMARY                                                          */
/********************************************************************/

/*
 * Annotations.
 */
typedef std::map<Loc, std::string> Annotations;

static bool intersects(const NodePtr &ast, uint32_t lo, uint32_t hi)
{
    return std::max(ast->lo, lo) <= std::min(ast->hi, hi);
}

/*
 * Write helper object.
 */
struct Writer
{
    FILE *stream;
    FILE *stream_chunks;
    const Annotations &As;
    unsigned col = 0;
    std::vector<Chunk> chunks;

    Writer(FILE *stream, FILE *stream_chunks, const Annotations &As)
    : stream(stream), stream_chunks(stream_chunks), As(As)
    {
        ;
    }

    const char *getColor(Chunk x)
    {
        switch (x.kind)
        {
            case GAP:
                return OFF;
            case COMMENT:
                return BLUE;
            case IF_STMT:
            case FOR_STMT:
            case WHILE_STMT:
            case DO_WHILE_STMT:
            case SWITCH_STMT:
            case BREAK_STMT:
            case CONTINUE_STMT:
            case GOTO_STMT:
            case RETURN_STMT:
            case CASE_STMT:
            case DEFAULT_STMT:
            case LABEL_STMT:
                return YELLOW;
            case FUNC_DECL:
            case STRUCT_DECL:
            case DEFINE:
                return GREEN;
            case BLOCK:
            case STRUCT_BODY:
            case ENUM_BODY:
                return CYAN;
            case DECL:
                return MAGENTA;
            default:
                return RED;
        }
    }

    void write(char c)
    {
        fputc(c, stream);
        col++;
        col += (c == '\t'? 8 - (col % 8): 0);
        col =  (c == '\n'? 0: col);
    }

    void write(const char *s)
    {
        for (size_t i = 0; s[i] != '\0'; i++)
            write(s[i]);
    }

    void write(Chunk x, std::string write_color = OFF)
    {
        const char *data = x.data();
        const char *color = (write_color != OFF && write_color != "" ? write_color.c_str() : getColor(x));
        chunks.push_back(x);
        fputs(color, stream);
        for (uint32_t i = x.lo; i < x.hi; i++)
        {
            char c = data[i];
            if (c != '\n')
            {
                write(c);
                continue;
            }
            Loc pos   = (x.lb() + (i - x.lo));
            Loc start = pos - col;
            auto j = As.lower_bound(start);
            if (j != As.end() && j->first <= pos)
            {
                fputs(OFF, stream);
                for (; j != As.end() && j->first <= pos; ++j)
                {
                    while (col < 63)
                        write(' ');
#ifdef PYTHON_PARSER
                    fprintf(stream, " # %s", j->second.c_str());
#else
                    fprintf(stream, " /* %s */", j->second.c_str());
#endif
                }
                fputs(color, stream);
            }
            write('\n');
        }
        fputs(write_color != "" ? BR_OFF : OFF, stream);
    }

    void print_chunks()
    {
        if (stream == nullptr) return;
        for (const Chunk &x : chunks)
        {
            const File &file = File::get(x.id);
            fprintf(stream_chunks, "%s: %d - %d\n", file.path, x.lo, x.hi);
        }
    }
};

/*
 * Print implicit preconditions inside this gap
 */
static void printAsserts(Writer &W, Chunk gap)
{
    const auto* func = getFunc(gap);
    std::string pre_str;
    std::string cond_str;
    if (func != nullptr)
    {
        for (const auto& node : **func->getBody())
        {
            if (node->lb() < gap.lb() || node->ub() > gap.ub()) continue;
            const Node* cond = nullptr;
            switch (node->kind)
            {
                case IF_STMT:
                {
                    const auto& if_stmt = dynamic_cast<const IfThenElse&>(*node);
                    if (if_stmt.child.back()->kind != EMPTY) continue;
                    cond = if_stmt.getCond()->get();
                    break;
                }
                case WHILE_STMT:
                case FOR_STMT:
                    cond = node->getCond()->get();
                    break;
                default: break;
            }
            if (cond == nullptr) continue;
            if (!cond_str.empty())
#ifdef PYTHON_PARSER
                cond_str += " and ";
            cond_str += "not (" + cond->str() + ")";
#else
                cond_str += " && ";
            cond_str += "!(" + cond->str() + ")";

#ifndef JAVA_PARSER
            if (node->kind == FOR_STMT)
                pre_str += dynamic_cast<const For&>(*node).child[0]->str() + "; ";
#endif
#endif
        }
    }


#ifdef PYTHON_PARSER
    if (cond_str.empty())
        W.write("pass\n");
    else
    {
        std::string result = "assert " + cond_str + "  # PRE\n";
        W.write(result.c_str());
    }
#else
    if (cond_str.empty())
        W.write("/* ... */\n");
    else
    {
        std::string result = pre_str + "assert(" + cond_str + ");  // PRE\n";
        W.write(result.c_str());
    }
#endif
}

/*
 * Print a "gap" that has been sliced away.
 */
static void printGap(Writer &W, Chunk gap, bool write_assert)
{
//    info("%sGAP%s: \"%s\"\n", MAGENTA, OFF, gap.str().c_str());

    if (gap.size() <= option_gap)
    {
        W.write(gap);
        return;
    }

    const char *data = gap.data();
    uint32_t lo = gap.lo, hi = gap.hi;

    bool nl = false;
    uint32_t i = lo;
    for (; i < hi; i++)
    {
        if (data[i] == '\n')
        {
            nl = true;
            for (i++; i < hi && isspace(data[i]) && i != '\n'; i++)
                ;
            break;
        }
        if (!isspace(data[i]))
            break;
    }

    uint32_t j = hi-1;
    for (; j >= i; j--)
    {
        if (data[j] == '\n')
        {
            nl = true;
            j++;
            break;
        }
        if (!isspace(data[j]))
            break;
    }

    if (i >= j)
    {
        // Entire gap is whitespace == no snip
        W.write(gap);
        return;
    }

    uint32_t k = ++j;
    for (; k >= i; k--)
    {
        if (data[k] == '\n')
        {
            nl = true;
            k++;
            break;
        }
    }

    W.write(Chunk(gap, lo, i));
    if (!nl || k < i)
    {
        // No newline in gap:
#if defined(PYTHON_PARSER) || defined(JAVA_PARSER)
        if (write_assert) W.write("unreachable()");                 // inline snip
        else W.write(Chunk(gap, i, j));
#else
        if (write_assert) W.write("__builtin_unreachable();");                 // inline snip
        else W.write("/* ... */");
#endif
        W.write(Chunk(gap, j, hi));
    }
    else
    {
        // At least one newline in gap:
#if defined(PYTHON_PARSER) || defined(JAVA_PARSER)
        if (write_assert) W.write("unreachable()\n");                 // inline snip
        // else W.write("pass\n");
        else printAsserts(W, Chunk(gap, i, k));
#else
        if (write_assert) W.write("__builtin_unreachable();\n");               // multiline snip
        // else W.write("/* ... */\n");
        else printAsserts(W, Chunk(gap, i, k));
#endif
        for (uint32_t i = k; i < hi; i++)
            W.write((isspace(data[i])? data[i]: ' '));
    }
}

/*
 * Count the number of tokens.
 */
static size_t countTokens(Chunk x)
{
    const char *data = x.data();
    size_t count = 0;
    for (size_t i = x.lo; i < x.hi; i++)
    {
        if (isspace(data[i]))
            continue;
        count++;
        for (; i < x.hi && (isalnum(data[i]) || data[i] == '_'); i++)
            ;
        if (i < x.hi && !isspace(data[i]))
            count++;
    }
    return count;
}

/*
 * Print helper object.
 */
struct Printer
{
    FILE *stream;
    FILE *stream_chunks;
    interval::Tree<Chunk, Loc> chunks;
    std::set<Chunk> chunk_allow_assert; // Only print an assert if the next chunk is in this set

    Printer(FILE *stream, FILE *stream_chunks) : stream(stream), stream_chunks(stream_chunks)
    {
        ;
    }

    bool push(const Chunk &c, bool allow_assert = false)
    {
        // Step (1): find all intersecting chunks:
        std::vector<Chunk> old;
        for (auto i = chunks.scan(c.lb(), c.ub()), iend = chunks.end();
                i != iend; ++i)
        {
            const auto &x = *i;
            if (x.lo == c.lo && x.hi == c.hi)
            {
                // The chunk has been previously added; so this is a NOP
                return false;
            }
            old.push_back(*i);
        }

        // Step (2): remove all intersecting chunks:
        for (const auto &x: old)
        {
            chunks.erase(x);
            chunk_allow_assert.erase(x);
        }
        
        // Step (3): re-insert any partial chunk:
        for (const auto &x: old)
        {
            if (x.lo < c.lo)
            {
                Chunk y(x, x.lo, c.lo);
                chunks.insert(y);
            }
            if (x.hi > c.hi)
            {
                Chunk y(x, c.hi, x.hi);
                chunks.insert(y);
            }
        }

        // Step (4) insert c:
        chunks.insert(c);
        if (allow_assert) chunk_allow_assert.insert(c);
        return true;
    }
    bool push(const Chunk &c, uint32_t lo, uint32_t hi, bool allow_assert = false)
    {
        if (hi <= lo)
            return true;
        Chunk d(c, lo, hi);
        bool trivial = true;
        for (size_t i = 0; trivial && i < d.size(); i++)
            trivial = trivial && isspace(d[i]);
        if (trivial)
            return true;
        return push(d, allow_assert);
    }

    bool contains(const Chunk &c)
    {
        return chunks.contains(c.lb(), c.ub());
    }

    /*
     * Print to file.
     */
    Writer print(const Annotations &As) const
    {
        if (chunks.size() == 0)
#ifdef PYTHON_PARSER
            fputs("# no code\n", stream);
#else
            fputs("/* no code */\n", stream);
#endif
        Writer W(stream, stream_chunks, As);

        std::vector<const File *> files;
        for (auto i = chunks.begin(), iend = chunks.end();
                i != iend; )
        {
            const File &file = i->lb().file();
            files.push_back(&file);
            i = chunks.scan(file.ub(), Loc::max());
        }

        for (auto i = files.rbegin(), iend = files.rend();
                i != iend; ++i)
        {
            const auto *file = *i;
#ifdef PYTHON_PARSER
            W.write("\n\n# FILE: ");
#else
            W.write("\n\n/* FILE: ");
#endif
            const char *base = getBasename(file->path);
            W.write(base);
            xfree((void *)base);
#ifdef PYTHON_PARSER
            W.write("\n");
#else
            W.write(" */\n");
#endif
            Loc pos = file->lb();
            bool first = true;
            for (auto j = chunks.scan(file->lb(), file->ub()),
                    jend = chunks.end(); j != jend;)
            {
                const auto &c = *j;
                if (first)
                {
                    first = false;
                    W.write(Chunk(c, file->lb().pos, c.lo));
                }
                bool found = chunk_allow_assert.find(c) != chunk_allow_assert.end();
                // W.write(c, found ? BR_GREEN : OFF);
                W.write(c);

                ++j;
                if (j != jend)
                {
                    pos = j->lb();
                }

                if (c.ub() < pos)
                    // W.write(Chunk(c, c.hi, pos.pos), BR_BLACK);
                    printGap(W, Chunk(c, c.hi, pos.pos, GAP), found);
            }
        }
        return W;
    }

    /*
     * Test if any part of a file is printed.
     */
    bool isPrinted(const File &file) const
    {
        auto i = chunks.scan(file.lb(), file.ub());
        return (i != chunks.end());
    }

    /*
     * Get the total token count.
     */
    size_t getTokenCount(void) const
    {
        size_t count = 0;
        for (const auto &x: chunks)
            count += countTokens(x);
        return count;
    }
};

/*
 * Build useful mappings
 */
static void buildInfo(void)
{
    for (const auto &entry: ITEMS)
    {
        const NodePtr &node = entry.second;
        switch (node->kind)
        {
            case STRUCT_DECL:
            {
                const Struct &s = dynamic_cast<Struct &>(*node);
                auto i = s.getBody();
                if (i == s.end())
                    break;
                for (const auto &child: **i)
                {
                    if (child->kind != DECL)
                        continue;
                    const Decl &decl = dynamic_cast<Decl &>(*child);
                    for (const auto &v: decl.vars)
                        FIELDS.insert({v.first, &s});
                }
                break;
            }
            case ENUM_DECL:
            {
                const Enum &e = dynamic_cast<Enum &>(*node);
                auto i = e.getBody();
                if (i == e.end())
                    break;
                for (const auto &child: **i)
                {
                    if (child->kind != ENUM_ITEM)
                        continue;
                    const EnumItem &item = dynamic_cast<EnumItem &>(*child);
                    ENUMS.insert({item.name, &e});
                }
                break;
            }
            case TYPEDEF:
            {
                const Typedef &t = dynamic_cast<Typedef &>(*node);
                for (const auto &name: t.names)
                    TYPES.insert({name, &t});
                switch (t.type->kind)
                {
                    case STRUCT_DECL:
                    {
                        const Struct &s = dynamic_cast<Struct &>(*t.type);
                        auto i = s.getBody();
                        if (i == s.end())
                            break;
                        for (const auto &child: **i)
                        {
                            if (child->kind != DECL)
                                continue;
                            const Decl &decl = dynamic_cast<Decl &>(*child);
                            for (const auto &v: decl.vars)
                                FIELDS.insert({v.first, &t});
                        }
                        break;
                    }
                    case ENUM_DECL:
                    {
                        const Enum &e = dynamic_cast<Enum &>(*t.type);
                        auto i = e.getBody();
                        if (i == e.end())
                            break;
                        for (const auto &child: **i)
                        {
                            if (child->kind != ENUM_ITEM)
                                continue;
                            const EnumItem &item = dynamic_cast<EnumItem &>(*child);
                            ENUMS.insert({item.name, &t});
                        }
                        break;
                    }
                    default:
                        break;
                }
                break;
            }
            case DECL:
            {
                const Decl &d = dynamic_cast<Decl &>(*node);
                for (const auto &entry: d.vars)
                    GLOBALS.insert({entry.first, &d});
                break;
            }
            default:
                break;
        }
    }
}

/*
 * Build the set of variable declarations for the given AST.
 */
void buildVars(const NodePtr &ast, uint32_t lo, uint32_t hi, uint32_t body_hi, Vars &Vs)
{
    if (lo >= hi)
        return;
    switch (ast->kind)
    {
        case DECL:
        {
            const Decl &decl = dynamic_cast<Decl &>(*ast);
            for (const auto &entry: decl.vars)
            {
                Chunk x = entry.first;
#ifdef PYTHON_PARSER
                Vs.insert(x, decl, lo, body_hi, hi);
#else
                Vs.insert(x, decl, lo, hi);
#endif
            }
            break;
        }
        case IF_STMT: case FOR_STMT: case WHILE_STMT:
        {
            for (const auto& child : *ast)
            {
                if (child == *ast->getCond())
                    buildVars(child, ast->lo, ast->hi, body_hi, Vs);
                else buildVars(child, child->lo, child->hi, body_hi, Vs);
            }
            return;
        }
        case EXPR:
            Vs.addUse(ast);
            break;
        default:
            break;
    }
    for (auto &child: *ast)
        buildVars(child, child->lo, hi, body_hi, Vs);
}

/*
 * Print the "body" to ensure control-flow is rendered correctly.
 */
static void printBody(Printer &P, const NodePtr &ast, bool top_level = false)
{
    switch (ast->kind)
    {
        case BLOCK: case STRUCT_BODY: case ENUM_BODY:
        {
            auto i = ast->begin(), iend = ast->end();
            if (i == iend)
            {
                P.push(*ast);
                return;
            }
            P.push(*ast, ast->lo, (*i)->lo, !top_level && ast->kind == BLOCK);
            auto j = i;
            for (++i; i != iend; ++i)
                j = i;
            P.push(*ast, (*j)->hi, ast->hi);
            return;
        }
        case BREAK_STMT:
        case CONTINUE_STMT:
        case GOTO_STMT:
            // Print simple blocks:
            P.push(*ast, ast->lo, ast->hi);
            return;
        case EMPTY:
            return;
        default:
        {
            auto i = ast->begin(), iend = ast->end();
            if (i != iend)
            {
                // Find right-most child:
                for (auto j = i; j != iend; ++j)
                    i = j;
            }
            if (i != iend)
            {
                const NodePtr &child = *i;
                if (child->hi == ast->hi)
                {
                    printBody(P, child);
                    return;
                }
            }
            // No right-most child = attempt to find terminating ";"
            const char *data = ast->data();
            for (uint32_t i = ast->hi-1; i >= ast->lo; i--)
            {
                if (data[i] == ';')
                {
                    P.push(*ast, i, i+1);
                    return;
                }
            }
            // Give up:
            return;
        }
    }
};

/*
 * Print comments.
 */
static void printComments(Printer &P, const NodePtr &ast,
    uint32_t lo, uint32_t hi, bool allow_assert = false)
{
    if (!intersects(ast, lo, hi))
        return;
    const char *data = ast->data();
    std::vector<Chunk> comments;
    for (const auto &child: *ast)
    {
        if (comments.size() > 0)
        {
            const auto &comment = comments.back();
            bool ok = true;
            int nl = 0;
            for (uint32_t i = comment.hi; ok && i < child->lo; i++)
            {
                nl += (data[i] == '\n');
                ok = isspace(data[i]) && nl <= 1;
            }
            if (!ok)
                comments.clear();
        }
        if (intersects(child, lo, hi))
        {
            for (const auto &comment: comments)
                P.push(comment, allow_assert);
            comments.clear();
        }
        else if (child->hi <= lo && child->kind == COMMENT)
            comments.push_back(*child);
        else if (child->lo >= hi && child->kind == COMMENT)
        {
            bool ok = true;
            for (uint32_t i = hi; ok && i < child->lo; i++)
                ok = isspace(data[i]) && data[i] != '\n';
            if (!ok)
                return;
            P.push(*child, allow_assert);
            hi = child->hi;
        }
    }
}

/*
 * Print top-level item comments.
 */
static void printItemComments(Printer &P, const Node &item)
{
    const File &file = item.file();
    unsigned lineno = file.lineno(item.lo);
    Loc ub = file.pos(lineno);
    while (true)
    {
        lineno--;
        if (lineno == 0)
            return;
        Loc lb = file.pos(lineno);
        std::vector<Chunk> comments;
        for (auto i = SCOPES.scan(lb, ub), iend = SCOPES.end(); i != iend; ++i)
        {
            const auto &node = i->node;
            if (node->kind != COMMENT)
                return;
            comments.push_back(*node);
        }
        if (comments.size() == 0)
            return;
        for (const auto &comment: comments)
            P.push(comment);
    }
}

/*
 * Print a location and relevant context.
 */
static void print(Printer &P, const NodePtr &ast, uint32_t lo, uint32_t hi, bool allow_assert = false, bool allow_last = false)
{
    if (!intersects(ast, lo, hi))
        return;
    if (P.contains(*ast))
        return;

    // Print all children:
    bool is_cond = (ast->kind == IF_STMT || ast->kind == WHILE_STMT
#if !defined(PYTHON_PARSER) && !defined(JAVA_PARSER)
        || ast->kind == FOR_STMT
#endif
    );
    allow_assert = allow_assert || is_cond;
    for (auto &child: *ast)
        print(P, child, lo, hi, allow_assert, *ast->getCond() == child);
    printComments(P, ast, lo, hi, allow_assert);

    // Print all non-child fragments:
    uint32_t lb = ast->lo;
    auto i = ast->begin(), iend = ast->end();
    for (; i != iend; ++i)
    {
        const NodePtr &child = *i;
        P.push(*ast, lb, child->lo, allow_assert);
        lb = child->hi;
    }
    P.push(*ast, lb, ast->hi, allow_assert && allow_last);

    // Print body terminators:
    auto j = ast->getBody();
    if (j != iend)
    {
        const NodePtr &body = *j;
        printBody(P, body);
    }
    else if (ast->kind == IF_STMT)
    {
        for (const auto& child : *ast)
        {
            if (child == *ast->getCond()) continue;
            printBody(P, child);
        }
    }

    // Include any conditional context:
    i = ast->getCond();
    if (i != iend)
    {
        const NodePtr &cond = *i;
        print(P, cond, cond->lo, cond->hi, allow_assert, true);
    }
}

/*
 * Print a struct and relevant fields.
 */
static void printStruct(Printer &P, const std::set<Chunk> &Fs,
    const Node &node)
{
    const Struct &s = dynamic_cast<const Struct &>(node);
    auto j = s.getBody();
    const NodePtr &body = *j;
    P.push(s, s.lo, body->lo);
    P.push(s, body->hi, s.hi);
    printBody(P, body);
    for (const auto &child: *body)
    {
        if (child->kind != DECL)
            continue;
        const Decl &decl = dynamic_cast<const Decl &>(*child);
        bool found = false;
        for (const auto &entry: decl.vars)
        {
            found = (Fs.find(entry.first) != Fs.end());
            if (found)
                break;
        }
        if (found)
        {
            P.push(decl);
            printComments(P, body, decl.lo, decl.hi);
        }
    }
}

/*
 * Print an number and relevant items.
 */
static void printEnum(Printer &P, const std::set<Chunk> &Ns,
    const Node &node)
{
    const Enum &e = dynamic_cast<const Enum &>(node);
    auto j = e.getBody();
    const NodePtr &body = *j;
    P.push(e, e.lo, body->lo);
    P.push(e, body->hi, e.hi);
    printBody(P, body);
    for (const auto &child: *body)
    {
        if (child->kind != ENUM_ITEM)
            continue;
        const EnumItem &item = dynamic_cast<const EnumItem &>(*child);
        if (Ns.find(item.name) == Ns.end())
            continue;
        P.push(item);
        printComments(P, body, item.lo, item.hi);
    }
}

/*
 * Print a typedef and relevant fields.
 */
static void printTypedef(Printer &P, const std::set<Chunk> &Ns,
    const std::set<Chunk> &Fs, const Node &node)
{
    const Typedef &d = dynamic_cast<const Typedef &>(node);
    P.push(d, d.lo, d.type->lo);
    P.push(d, d.type->hi, d.hi);
    switch (d.type->kind)
    {
        case STRUCT_DECL:
            printStruct(P, Fs, *d.type);
            break;
        case ENUM_DECL:
            printEnum(P, Ns, *d.type);
            break;
        default:
            P.push(*d.type);
            break;
    }
}

/*
 * Print all relevant #includes.
 */
static void printIncludes(Printer &P)
{
    for (const auto &entry: FILES)
    {
        const File &file = entry.second;
        if (!P.isPrinted(file))
            continue;
        for (auto i = SCOPES.scan(file.lb(), file.ub()), iend = SCOPES.end();
                i != iend; ++i)
        {
            const NodePtr &node = i->node;
            if (node->kind != INCLUDE)
                continue;
            const Include &inc = dynamic_cast<const Include &>(*node);
            Chunk path = getIncludePath(inc.path);
            if (path.empty())
                continue;
            std::vector<const char *> candidates;
            resolvePath(path.str().c_str(), candidates);
            bool found = false;
            for (const auto *candidate: candidates)
            {
                auto j = FILES.find(candidate);
                if (j == FILES.end())
                    continue;
                found = P.isPrinted(j->second);
                if (found)
                    break;
            }
            if (found)
                P.push(inc);
        }
    }
}

/*
 * Print all locations using the given var.
 */
static void printVar(Printer &P, const Vars &Vs, const NodePtr &root, const NodePtr &ast,
    const Var *v, std::set<Chunk> &Ts)
{
#if 0
    auto k = Vs.v2e.find(v);
    if (k == Vs.v2e.end())
        return;
    auto &es = k->second;
    for (const auto *e: es)
        print(P, Vs, root, ast, e->lo, e->hi);
#endif
    bool allow_push = true;
#ifdef PYTHON_PARSER
    allow_push = ast->hi <= v->real_hi;
#endif
    if (allow_push) P.push(v->decl);
    for (const auto &t: v->decl.types)
        Ts.insert(t);
}

static void setLive(const Vars &Vs, const NodePtr &ast, uint32_t lo, uint32_t hi, std::set<const Var *> &Ls);

/*
 * Slicing algorithm.
 */
static void slice(Printer &P, const Vars &Vs, const NodePtr &root, 
    const NodePtr &ast, uint32_t lo, uint32_t hi, std::set<Chunk> &Ns,
    std::set<Chunk> &Ts, std::set<Chunk> &Fs, std::set<const Var *> &Ls)
{
    if (!intersects(ast, lo, hi))
        return;
    print(P, root, lo, hi);
    for (auto &child: *ast)
        slice(P, Vs, root, child, lo, hi, Ns, Ts, Fs, Ls);
    printComments(P, ast, lo, hi);

    switch (ast->kind)
    {
        case EXPR:
        {
            const Expr &expr = dynamic_cast<Expr &>(*ast);
            for (const auto &n: expr.names)
                Ns.insert(n);
            for (const auto &t: expr.types)
                Ts.insert(t);
            for (const auto &f: expr.fields)
                Fs.insert(f);
            auto j = Vs.e2v.find(&expr);
            if (j == Vs.e2v.end())
                return;
            auto &vs = j->second;
            for (const auto *v: vs)
                printVar(P, Vs, root, ast, v, Ts);
            return;
        }
        case DECL:
        {
            const Decl &decl = dynamic_cast<Decl &>(*ast);
            for (const auto &t: decl.types)
                Ts.insert(t);
            auto j = Vs.d2v.find(&decl);
            if (j == Vs.d2v.end())
                return;
            auto &vs = j->second;
            for (const auto *v: vs)
                printVar(P, Vs, root, ast, v, Ts);
            return;
        }
        case IF_STMT: case FOR_STMT: case WHILE_STMT:
        {
            // set the condition live
            const auto& cond = *ast->getCond();
            setLive(Vs, cond, cond->lo, cond->hi, Ls);
#if defined(PYTHON_PARSER) || defined(JAVA_PARSER)
            if (ast->kind == FOR_STMT)
            {
                const auto& decl = *ast->begin();
                setLive(Vs, decl, decl->lo, decl->hi, Ls);
            }
#endif
            return;
        }
        default:
            return;
    }
}

/*
 * Determine if the given range points to code or not.
 */
static bool isReturn(const NodePtr &ast, uint32_t lo, uint32_t hi)
{
    if (!intersects(ast, lo, hi))
        return false;
    switch (ast->kind)
    {
        case RETURN_STMT:
            return true;
        default:
            for (const auto &child: *ast)
                if (isReturn(child, lo, hi))
                    return true;
            return false;
    }
}

/*
 * Determine if the code is data-flow dependent.
 */
static bool isLive(const Vars &Vs, const NodePtr &ast,
    uint32_t lo, uint32_t hi, std::set<const Var *> &Ls,
    bool cond_0 = false, bool live_0 = false)
{
    if (!intersects(ast, lo, hi))
        return live_0;

    bool live = live_0;
    switch (ast->kind)
    {
        case EXPR:
        {
            const Expr &expr = dynamic_cast<Expr &>(*ast);
            for (const auto *v: Vs.get(expr))
            {
                // This expression is live if:
                // (1) It writes to a variable of interest
                //     (i.e., there variable of interest is an lval); or
                // (2) It reads from a variable of interest and this is a
                //     condition
                live = live ||
                    ((cond_0? true:
                      std::binary_search(expr.lvals.begin(), expr.lvals.end(), v->x)) &&
                     Ls.find(v) != Ls.end());
            }
            if (live)
            {
                for (const auto *v: Vs.get(expr))
                    Ls.insert(v);
            }
            break;
        }
        case DECL:
        {
            // TODO: fine-grained decl handling
            const Decl &decl = dynamic_cast<Decl &>(*ast);
            for (const auto *v: Vs.get(decl))
                live = live || (Ls.find(v) != Ls.end());
            if (live)
            {
                for (const auto &child: decl)
                {
                    if (child->kind != EXPR)
                        continue;
                    const Expr &expr = dynamic_cast<Expr &>(*child);
                    for (const auto *v: Vs.get(expr))
                        Ls.insert(v);
                }
            }
            return live;
        }
        case ASSERT:
            live = true;
            break;
        default:
            break;
    }
    auto j = ast->getCond();
    for (auto i = ast->begin(), iend = ast->end(); i != iend; ++i)
    {
        const auto &child = *i;
        bool cond = cond_0 || (i == j);
        bool r = isLive(Vs, child, lo, hi, Ls, cond, live_0);
        live = (live || r);
    }
    return live;
}

/*
 * Set the variables in the expression as live.
 */
static void setLive(const Vars &Vs, const NodePtr &ast,
    uint32_t lo, uint32_t hi, std::set<const Var *> &Ls)
{
    if (!intersects(ast, lo, hi))
        return;
    switch (ast->kind)
    {
        case EXPR:
        {
            const Expr &expr = dynamic_cast<Expr &>(*ast);
            for (const auto *v: Vs.get(expr))
            {
                info("%sLIVE%s: %s\n", YELLOW, OFF, v->x.str().c_str());
                Ls.insert(v);
            }
            break;
        }
        case DECL:
        {
            const Decl &decl = dynamic_cast<Decl &>(*ast);
            for (const auto *v: Vs.get(decl))
            {
                info("%sLIVE%s: %s\n", BLUE, OFF, v->x.str().c_str());
                Ls.insert(v);
            }
            break;
        }
        default:
            break;
    }
    for (auto &child: *ast)
        setLive(Vs, child, lo, hi, Ls);
}

/*
 * Find the matching call-expression.
 */
static const Call *getCall(const NodePtr &ast, uint32_t lo, uint32_t hi)
{
    if (!intersects(ast, lo, hi))
        return nullptr;
    switch (ast->kind)
    {
        case CALL:
            break;
        default:
            for (auto &child: *ast)
            {
                const auto *r = getCall(child, lo, hi);
                if (r != nullptr) return r;
            }
            return nullptr;
    }
    const Call &call = dynamic_cast<Call &>(*ast);
    return &call;
}

/*
 * Parse a location.
 */
static Loc loadLoc(const char *path, unsigned line)
{
    const File *file = loadFile(path);
    if (file == nullptr)
        error("failed to find file \"%s\" for location \"%s:%u\"", path,
            path, line);
    return file->pos(line);
}

/*
 * Annotate a location.
 */
static void annotate(Annotations &As, Loc loc, const char *msg, ...)
{
#if 0
    va_list ap;
    va_start(ap, msg);
    char buf[100];
    int r = vsnprintf(buf, sizeof(buf)-1, msg, ap);
    va_end(ap);
    if (r < 0 || (size_t)r >= sizeof(buf)-1)
        return;
    As.insert({loc, buf});
#endif
}

/*
 * Tool command.
 */
struct Command
{
    const int16_t window;
    const char * const label;
    const char * const loc;

    Command(int16_t window, const char *label, const char *loc) :
        window(window), label(label == nullptr? loc: label), loc(loc)
    {
        ;
    }
};

/*
 * Print additional context.
 */
static void printContext(Printer &P, Annotations &As,
    std::set<Chunk> &Ns, std::set<Chunk> &Ts, std::set<Chunk> &Fs)
{
    buildInfo();

    /*
     * Expand macro pass.
     */
    std::vector<Chunk> Tmp;
    for (const auto &n: Ns)
    {
        for (auto i = ITEMS.find(n), iend = ITEMS.end();
                i != iend && i->first == n; ++i)
        {
            NodePtr &node = i->second;
            if (node->kind != DEFINE)
                continue;
            P.push(*node);
            annotate(As, node->ub(), "name %s", n.str().c_str());
            
            const Define &d = dynamic_cast<const Define &>(*node);
            for (const auto &n: d.names)
            {
                // Marcos are not parsed like expressions, so we try to guess
                // what each name means.
                int f = (int)(FIELDS.find(n)  != FIELDS.end());
                int t = (int)(TYPES.find(n)   != TYPES.end());
                int m = (int)(ENUMS.find(n)   != ENUMS.end() ||
                              GLOBALS.find(n) != GLOBALS.end());
                if (f + t + m != 1)
                    continue;   // Unknown or ambigious
                if (f) Fs.insert(n);
                if (t) Ts.insert(n);
                if (m) Tmp.push_back(n);
            }
        }
    }
    for (const auto &n: Tmp)
        Ns.insert(n);

    for (const auto &f: Fs)
    {
        auto i = FIELDS.find(f);
        if (i == FIELDS.end())
            continue;           // Unknown field
        const Node *n = i->second;
        ++i;
        if (i == FIELDS.end() || i->first != f)
        {
            // Field is unique to struct
            switch (n->kind)
            {
                case STRUCT_DECL:
                    Ts.insert(dynamic_cast<const Struct *>(n)->name);
                    break;
                case TYPEDEF:
                {
                    const Typedef *t = dynamic_cast<const Typedef *>(n);
                    for (const auto &name: t->names)
                        Ts.insert(name);
                    break;
                }
                default:
                    break;
            }
        }
    }
    for (const auto &t: Ts)
    {
        for (auto i = ITEMS.find(t), iend = ITEMS.end();
                i != iend && i->first == t; ++i)
        {
            NodePtr &node = i->second;
            switch (node->kind)
            {
                case STRUCT_DECL:
                {
                    printStruct(P, Fs, *node);
                    printItemComments(P, *node);
                    annotate(As, node->ub(), "type %s", t.str().c_str());
                    break;
                }
                case ENUM_DECL:
                {
                    printEnum(P, Ns, *node);
                    printItemComments(P, *node);
                    annotate(As, node->ub(), "type %s", t.str().c_str());
                    break;
                }
                case DEFINE:
                {
                    P.push(*node);
                    annotate(As, node->ub(), "type %s", t.str().c_str());
                    break;
                }
                default:
                    break;
            }
        }
        for (auto i = TYPES.find(t), iend = TYPES.end();
                i != iend && i->first == t; ++i)
        {
            const Typedef *d = i->second;
            printTypedef(P, Ns, Fs, *d);
            printItemComments(P, *d);
            annotate(As, d->ub(), "type %s", t.str().c_str());
        }
    }
    for (const auto &n: Ns)
    {
        for (auto i = ITEMS.find(n), iend = ITEMS.end();
                i != iend && i->first == n; ++i)
        {
            NodePtr &node = i->second;
            switch (node->kind)
            {
                case FUNC_DECL:
                {
                    const Func &f = dynamic_cast<const Func &>(*node);
                    const NodePtr &body = *f.getBody();
                    P.push(f, f.lo, body->lo);
                    printBody(P, body);
                    annotate(As, node->ub(), "name %s", n.str().c_str());
                    break;
                }
#if 0
                case DEFINE:
                    P.push(*node);
                    annotate(As, node->ub(), "name %s", n.str().c_str());
                    break;
#endif
                default:
                    break;
            }
        }
        for (auto i = ENUMS.find(n), iend = ENUMS.end();
                i != iend && i->first == n; ++i)
        {
            const Node *node = i->second;
            switch (node->kind)
            {
                case ENUM_DECL:
                    printEnum(P, Ns, *node);
                    break;
                case TYPEDEF:
                    printTypedef(P, Ns, Fs, *node);
                    break;
                default:
                    continue;
            }
            printItemComments(P, *node);
            annotate(As, node->ub(), "name %s", n.str().c_str());
        }
        for (auto i = GLOBALS.find(n), iend = GLOBALS.end();
                i != iend && i->first == n; ++i)
        {
            const Decl *d = i->second;
            P.push(*d);
            annotate(As, d->ub(), "name %s", n.str().c_str());
        }
    }
    printIncludes(P);
}

/*
 * Parse a location.
 */
static bool parseLoc(const char *loc, const char **file, unsigned *line)
{
    int last = -1, i;
    for (i = 0; loc[i] != '\0'; i++)
        last = (loc[i] == ':'? i: last);
    if (last <= 0)
        return false;
    unsigned lineno = 0;
    bool ok = true;
    for (i = last + 1; ok && loc[i] != '\0'; i++)
    {
        ok = ok && isdigit(loc[i]);
        lineno = 10 * lineno + (loc[i] - '0');
    }
    ok = ok && (lineno != 0);
    if (!ok)
        return false;

    char buf[PATH_MAX];
    if ((size_t)last >= sizeof(buf)-1)
        return false;
    memcpy(buf, loc, last);
    buf[last] = '\0';

    static std::set<const char *, CStrCmp> cache;
    auto j = cache.find(buf);
    if (j == cache.end())
    {
        const char *str = xstrdup(buf);
        j = cache.insert(str).first;
    }
    *file = *j;
    *line = lineno;
    return true;
}

/*
 * Parse a trace file.
 */
void parseTrace(const char *filename, TRACE &T)
{
    FILE *stream = fopen(filename, "r");
    if (stream == NULL)
        error("failed to open file \"%s\" for reading: %s",
            filename, strerror(errno));
    char e[4], loc[PATH_MAX + 32];
    int thread;
    while (fscanf(stream, " %3s #%d %" STR(PATH_MAX) "s",
            e, &thread, loc) == 3)
    {
        Event event;
        if (strcmp(e, "EXE") == 0)
            event = Event::EXE;
        else if (strcmp(e, "CLL") == 0)
            event = Event::CLL;
        else if (strcmp(e, "RET") == 0)
            event = Event::RET;
        else
            error("failed to parse trace \"%s\"; unknown event type \"%s\"",
                filename, e);
        const char *file;
        unsigned line;
        if (!parseLoc(loc, &file, &line))
            error("failed to parse trace \"%s\"; invalid location \"%s\"",
                filename, loc);
        ENTRY entry = {file, line, thread, event};
        T.push_front(entry);
    }
    char c;
    while (isspace(c = getc(stream)) && c != EOF)
        ;
    if (!feof(stream))
        error("failed to parse trace \"%s\"; invalid format", filename);
    fclose(stream);
    T.shrink_to_fit();
}

/*
 * Summarize a trace.
 */
void summarizeTrace(const Config &config, const TRACE &T, FILE* out, FILE* out_chunks)
{
    auto getLine = [](const ENTRY &E) -> Chunk
    {
        Loc loc = loadLoc(E.file, E.line);
        const File &file = loc.file();
        int line = loc.lineno();
        Loc end = file.pos(line+1);
        return Chunk(file, loc.pos, end.pos, LINE);
    };
    auto getFunc = [](const Chunk &line) -> const Func *
    {
        // We implicitly assume one function per line.
        auto i = SCOPES.scan(line.lb(), line.ub());
#ifdef PYTHON_PARSER
        while (i != SCOPES.end() && i->node->kind != FUNC_DECL) ++i;
#endif
        if (i == SCOPES.end()) return nullptr;
        const NodePtr &node = i->node;
        if (node->kind != FUNC_DECL) return nullptr;
        const Func &f = dynamic_cast<const Func &>(*node);
        return &f;
    };

    Annotations As;
    Printer P(out, out_chunks);

    std::set<Chunk> Ns, Ts, Fs;
    int n = 0;
    std::set<const Var *> Ls;           // Live variables-of-interest
    std::set<std::string> funcNames;
    for (auto i = T.begin(), iend = T.end(); i != iend; ++i)
    {
        const auto &E = *i;
        Chunk line = getLine(E);
        const auto *r = getFunc(line);
        if (r == nullptr) continue;
        const Func &f = *r;
        f.analyze();
        auto funcName = f.name.str();
        if (funcNames.find(funcName) == funcNames.end())
        {
            funcNames.insert(funcName);
            info("Current function: %s\n", funcName.c_str());
            printNodeTree(f);
        }
        Vars *Vs = &f.vars;

        // For debugging:
        std::string l;
        {
            const char *data = line.data();
            uint32_t j = line.hi-1;
            for (; j >= line.lo && isspace(data[j]); j--)
                ;
            uint32_t i = line.lo;
            for (; i < line.hi && isspace(data[i]); i++)
                ;
            for (uint32_t k = i; k <= j; k++)
            {
                if (data[k] == '\n')
                    l += "\\n";
                else
                    l += data[k];
            }
        }

        switch (E.event)
        {
            case Event::CLL:
            {
                auto j = i; ++j;
                if (j == iend) continue;
                Chunk line2 = getLine(*j);
                const auto *r = getFunc(line2);
                if (r == nullptr) continue;
                const Func &g = *r;
                g.analyze();
                Vars *Ws = &g.vars;

                const auto *call = getCall(*g.getBody(), line2.lo, line2.hi);
                // assert(call != nullptr);
                if (call == nullptr) continue;
                auto k = call->begin();
                bool any = false;
                for (const auto &child: f)
                {
                    if (k == call->end())    break;
                    if (child->kind != DECL) continue;
                    const Decl &param = dynamic_cast<Decl &>(*child);
                    bool live = false;
                    for (const auto *v: Vs->get(param))
                        live = live || (Ls.find(v) != Ls.end());
                    if (live)
                    {
                        const auto &arg = *k;
                        setLive(*Ws, arg, arg->lo, arg->hi, Ls);
                    }
                    any = any || live;
                    ++k;
                }
                if (any)
                {
                    const NodePtr &body = *g.getBody();
                    P.push(g, g.lo, body->lo);
                    printBody(P, body);
                    // printItemComments(P, *node);
                    slice(P, *Vs, body, body, line2.lo, line2.hi, Ns, Ts, Fs, Ls);
                }
                continue;
            }
            case Event::RET:
            {
                auto j = i;
                if (j == T.begin()) continue;
                --j;
                if (j == iend) continue;
                line = getLine(*j);
                const auto *t = getFunc(line);
                if (t == nullptr) continue;
                const Func &f = *t;
                f.analyze();
                Vars *Vs = &f.vars;

                if (!isLive(*Vs, *f.getBody(), line.lo, line.hi, Ls, false, (n == 0)))
                    continue;

                ++j; ++j;
                if (j == iend) continue;
                Chunk line2 = getLine(*j);
                const auto *r = getFunc(line2);
                if (r == nullptr) continue;
                const Func &g = *r;
                g.analyze();
                Vars *Ws = &g.vars;

                // Find the return statement:
                while (!isReturn(*g.getBody(), line2.lo, line2.hi))
                {
                    ++j;
                    if (j == iend) break;
                    line2 = getLine(*j);
                    const auto *s = getFunc(line2);
                    if (s != r)
                    {
                        j = iend;
                        break;
                    }
                }
                if (j == iend) continue;
                info("CALL   = %s%s%s", CYAN, line.str().c_str(), OFF);
                info("RETURN = %s%s%s", CYAN, line2.str().c_str(), OFF);

                const auto *call = getCall(*f.getBody(), line.lo, line.hi);
                if (call != nullptr)
                {
                    auto k = g.begin();
                    for (const auto &arg: *call)
                    {
                        if (k == f.end())      break;
                        if (arg->kind != EXPR) continue;
                        const Expr &expr = dynamic_cast<Expr &>(*arg);
                        bool live = (expr.lvals.size() > 0);
                        if (live)
                        {
                            const auto &param = *k;
                            info("LIVE: %s\n", param->str().c_str());
                            setLive(*Ws, param, param->lo, param->hi, Ls);
                        }
                        ++k;
                    }
                }
                setLive(*Ws, *g.getBody(), line2.lo, line2.hi, Ls);

                const NodePtr &body = *g.getBody();
                P.push(g, g.lo, body->lo);
                printBody(P, body);
                // printItemComments(P, *node);
                info("SLICE: %s", line2.str().c_str());
                slice(P, *Vs, body, body, line2.lo, line2.hi, Ns, Ts, Fs, Ls);
                continue;
            }
            default:
                break;
        }

        const NodePtr &body = *f.getBody();
        if (isLive(*Vs, body, line.lo, line.hi, Ls, false, (n == 0)))
        {
            P.push(f, f.lo, body->lo);
            printBody(P, body, true);
            // printItemComments(P, *node);
            slice(P, *Vs, body, body, line.lo, line.hi, Ns, Ts, Fs, Ls);

            if (n == 0)
            {
                // if (config.label.crash != nullptr)
                //     As.insert({line.lb(), config.label.crash});
#if 0
                for (int i = -config.window; i < config.window; i++)
                {
                    const File &file = line.file();
                    Loc lo = file.pos(line-i);
                    Loc hi = file.pos(line-i+1);
                    print(P, node, lo.pos, hi.pos);
                }
#endif
            }
        }

        info("%sEXE%s %s:%u:\n", GREEN, OFF, E.file, E.line);
        info("\tline = %s%s%s\n", CYAN, l.c_str(), OFF);
        info("\tlive = {");
        bool prev = false;
        for (const auto *v: Ls)
        {
            info("%s%s", (prev? ",": ""), v->x.str().c_str());
            prev = true;
        }
        info("}\n");

        if (P.getTokenCount() >= config.tokens)
            break;

        n++;
    }

    if (config.context)
        printContext(P, As, Ns, Ts, Fs);

    info("----------------------------------------\n");
    option_tty = isatty(fileno(out));
    Writer writer = P.print(As);
    fputc('\n', out);

    info("----------------------------------------\n");
    info("Names = ");
    for (const auto &n: Ns)
        info("%s ", n.str().c_str());
    info("\n");
    info("Types = ");
    for (const auto &t: Ts)
        info("%s ", t.str().c_str());
    info("\n");
    info("Fields = ");
    for (const auto &f: Fs)
        info("%s ", f.str().c_str());
    info("\n");

    if (out_chunks != nullptr)
    {
        fprintf(out_chunks, "------START CHUNKS------\n");
        writer.print_chunks();
        fprintf(out_chunks, "------END CHUNKS------\n");
    }
}
