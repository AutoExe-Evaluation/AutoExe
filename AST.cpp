#include <cstdint>

#include <algorithm>
#include <deque>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <unistd.h>

#include "itree.h"
#include "AST.h"

std::vector<const File *> File::files;

const File &Loc::file(void) const
{
    return File::get(id);
}
unsigned Loc::lineno(void) const
{
    return file().lineno(pos);
}
std::string Loc::str(void) const
{
    std::string s;
    const File &f = file();
    s += f.path;
    s += ':';
    s += std::to_string(f.lineno(pos));
    return s;
}

/*
 * Extract a pathname from a #include path.
 */
Chunk getIncludePath(Chunk path)
{
    if (path.size() < 2 ||
        (!(path[0] == '"' && path[path.size()-1] == '"') &&
         !(path[0] == '<' && path[path.size()-1] == '>')))
        return Chunk();
    // TODO: relative paths are not handled; so just remove relatative
    //       prefix as a temporary workaround:
    uint32_t offset = 1;
    while (path[offset] == '.' || path[offset] == '/')
        offset++;
    return Chunk(path, path.lo+offset, path.hi-1);
}

/*
 * Expand macros.
 */
void expandMacros(const Chunk &n, std::set<Chunk> &names)
{
    if (!names.insert(n).second)
        return;
    for (auto i = ITEMS.find(n), iend = ITEMS.end();
         i != iend && i->first == n; ++i)
    {
        NodePtr &node = i->second;
        if (node->kind != DEFINE)
            continue;
        const Define &d = dynamic_cast<const Define &>(*node);
        for (const auto &m: d.names)
            expandMacros(m, names);
    }
}

Files FILES;        // All loaded source files
Index INDEX;        // Filename -> absolute path

Items  ITEMS;
Scopes SCOPES;

Fields  FIELDS;
Enums   ENUMS;
Types   TYPES;
Globals GLOBALS;

void printChunks(const std::string &indent, const std::string &name, const std::vector<Chunk> &chunks)
{
    if (chunks.empty()) return;
    info("%s%s = {", indent.c_str(), name.c_str());
    bool first = true;
    for (const auto &chunk : chunks)
    {
        if (first) first = false;
        else info(", ");
        auto str = chunk.str();
        info("%s", str.substr(0, 20).c_str());
        if (str.size() >= 20) info("...");
    }
    info("}\n");
}

void printExpr(int level, const std::string &indent, const Expr* expr)
{
    if (!expr) return;

    // Print variable info
    printChunks(indent, "names", expr->names);
    printChunks(indent, "types", expr->types);
    printChunks(indent, "fields", expr->fields);
    printChunks(indent, "lvals", expr->lvals);

    // Print detail expression
    auto detail = expr->detail.get();
    if (!detail) return;
    info("%sDetail:\n", indent.c_str());
    printNodeTree(*detail, level + 1);
}

void printNodeTree(const Node& root, int level)
{
    std::string indent;
    for (int i = 0; i < level; ++i) indent += "  ";
    info("%s%s", indent.c_str(), root.repr().c_str());

    auto str = root.str();
    info(" %s", str.substr(0, 20).c_str());
    if (str.size() >= 20) info("...\n");
    else info("\n");

    if (auto expr = dynamic_cast<const Expr*>(&root))
        printExpr(level, indent, expr);

    if (auto decl = dynamic_cast<const Decl*>(&root))
    {
        info("%sVars: ", indent.c_str());
        bool first = true;
        for (const auto& [chunk, index] : decl->vars)
        {
            if (first) first = false;
            else info(", ");
            info("%s (%zu)", chunk.str().c_str(), index);
        }
        info("\n%sTypes: ", indent.c_str());
        first = true;
        for (const auto& chunk : decl->types)
        {
            if (first) first = false;
            else info(", ");
            info("%s", chunk.str().c_str());
        }
        info("\n");
    }

    if (root.begin() == nullptr) return;
    auto cond = root.getCond();
    auto body = root.getBody();
    int cnt = 0;
    for (const auto& child : root)
    {
        info("%s|-[Child %d", indent.c_str(), cnt);
        if (cond != nullptr && *cond != nullptr && *cond == child)
            info(", Cond");
        if (body != nullptr && *body != nullptr && *body == child)
            info(", Body");
        info("]\n");
        if (child == nullptr)
        {
            info("%s  <NULL>\n", indent.c_str());
            continue;
        }
        printNodeTree(*child, level + 1);
        ++cnt;
    }
}

const Func* getFunc(const Chunk& line)
{
    // We implicitly assume one function per line.
    auto i = SCOPES.scan(line.lb(), line.ub());
    if (i == SCOPES.end()) return nullptr;
    const NodePtr &node = i->node;
    if (node->kind != FUNC_DECL) return nullptr;
    return &dynamic_cast<const Func &>(*node);
}
