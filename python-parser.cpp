#include <cassert>
#include <cerrno>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <deque>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

#include <ftw.h>
#include <getopt.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <libgen.h>

#include "tree_sitter/api.h"

#include "itree.h"
#include "parser.h"

extern "C" TSLanguage *tree_sitter_python();

static TSParser *parser = nullptr;

struct TSNodeItr
{
    TSNode node;
    int i;
};

static inline TSNodeItr begin(const TSNode &node)
{
    TSNodeItr itr = {node, 0};
    return itr;
}
static inline TSNodeItr end(const TSNode &node)
{
    TSNodeItr itr = {node, (int)ts_node_child_count(node)};
    return itr;
}
static inline int size(const TSNode &node)
{
    return (int)ts_node_child_count(node);
}
static inline TSNodeItr &operator++(TSNodeItr &itr)
{
    itr.i++;
    int max = ts_node_child_count(itr.node);
    if (itr.i >= max)
        itr.i = max;
    return itr;
}
static inline TSNodeItr &operator--(TSNodeItr &itr)
{
    itr.i--;
    if (itr.i < 0)
        itr.i = 0;
    return itr;
}
static inline TSNodeItr &operator+=(TSNodeItr &itr, int i)
{
    itr.i += i;
    int max = ts_node_child_count(itr.node);
    if (itr.i >= max)
        itr.i = max;
    return itr;
}
static inline TSNodeItr &operator-=(TSNodeItr &itr, int i)
{
    itr.i -= i;
    if (itr.i < 0)
        itr.i = 0;
    return itr;
}
static inline TSNode operator*(const TSNodeItr &itr)
{
    return ts_node_child(itr.node, itr.i);
}
static inline bool operator==(const TSNodeItr &itr1, const TSNodeItr &itr2)
{
    return (itr1.i == itr2.i && ts_node_eq(itr1.node, itr2.node));
}
static inline bool operator!=(const TSNodeItr &itr1, const TSNodeItr &itr2)
{
    return !(itr1 == itr2);
}
static inline bool operator==(const TSNode &node, std::nullptr_t null)
{
    return ts_node_is_null(node);
}
static inline bool operator==(const TSNode &node, const char *type)
{
    return (strcmp(ts_node_type(node), type) == 0);
}
static inline bool operator!=(const TSNode &node, const char *type)
{
    return !(node == type);
}
static inline bool operator!=(const TSNode &node, std::nullptr_t null)
{
    return !(node == null);
}
static inline uint32_t lo(const TSNode &node)
{
    return ts_node_start_byte(node);
}
static inline uint32_t hi(const TSNode &node)
{
    return ts_node_end_byte(node);
}
static inline TSNode get(const TSNode &node, const char *field)
{
    return ts_node_child_by_field_name(node, field, strlen(field));
}
static inline TSNode get(const TSNode &node, unsigned i)
{
    return ts_node_child(node, i);
}
static TSNodeItr after(const TSNode &node, const char *type)
{
    auto i = begin(node), iend = end(node);
    for (; i != iend && *i != type; ++i)
            ;
    ++i;
    return i;
}
static std::string str(const TSNode &node)
{
    std::string s;
    char *str = ts_node_string(node);
    s += str;
    free(str);
    return s;
}

static void printTree(const File &file, TSNode current, int level = 0)
{
    std::string indent;
    for (int i = 0; i < level; ++i) indent += "  ";
    int child_cnt = ts_node_child_count(current);
    info("%stype: %s, child # = %d", indent.c_str(), ts_node_type(current), child_cnt);
    auto [start_r, start_c] = ts_node_start_point(current);
    auto [end_r, end_c] = ts_node_end_point(current);
    info(", start = [%d:%d], end = [%d:%d], ", start_r, start_c, end_r, end_c);
    if (ts_node_is_named(current))
        info("named node");
    else info("unnamed node");
    info("\n");
    auto str = Chunk(file, lo(current), hi(current)).str();
    info("%sContent: %s", indent.c_str(), str.substr(0, 20).c_str());
    if (str.size() >= 20) info("...\n");
    else info("\n");
    int cnt = 0;
    for (auto child: current)
    {
        info("%s|-[Child %d]\n", indent.c_str(), cnt);
        printTree(file, child, level + 1);
        ++cnt;
    }
}

static Chunk parseChunk(const File &file, TSNode node, int kind)
{
    if (node == nullptr)
        return Chunk();
    return Chunk(file, lo(node), hi(node), kind);
}

static void parseParam(const File &file, TSNode param, Decl &decl)
{
    for (auto child: param)
    {
        if (child == "identifier")
        {
            Chunk x(file, lo(child), hi(child), NAME);
            NodePtr e(new Empty(file, x.hi));
            info("%sPARAM%s %s\n", RED, OFF, x.str().c_str());
            decl.insertVar(x, e);
        }
        else if (child == "type_identifier")
        {
            Chunk t = Chunk(file, lo(child), hi(child), NAME);
            decl.insertType(t);
        }
        else if (param == "array_declarator" && child == "[")
            break;
        else
            parseParam(file, child, decl);
    }
}

static std::map<std::string, std::set<std::string>> funcFirstNames;

static NodePtr getFirstParam(const File &file, TSNode node)
{
    if (node != "function_definition") return nullptr;
    TSNode params = get(node, "parameters");
    if (params == nullptr) return nullptr;
    TSNode* result = nullptr;
    for (auto child: params)
    {
        if (child == "identifier")
        {
            result = &child;
            break;
        }
    }
    if (result == nullptr) return nullptr;
    NodePtr node_ptr(new Decl(file, lo(*result), hi(*result)));
    return node_ptr;
}

static Chunk parseFuncDecl(const File &file, TSNode node,
    std::vector<NodePtr> &Ps)
{
    if (node == nullptr)
        return Chunk();
    else if (node == "identifier")
        return Chunk(file, lo(node), hi(node), NAME);
    else if (node == "function_definition")
    {
        Chunk f = parseFuncDecl(file, get(node, "name"), Ps);
        TSNode params = get(node, "parameters");
        if (params == nullptr)
            return f;
        for (auto child: params)
        {
            if (child == "identifier")
            {
                NodePtr node(new Decl(file, lo(child), hi(child)));
                Decl &decl = dynamic_cast<Decl &>(*node);
                Chunk x(file, lo(child), hi(child), NAME);
                NodePtr e(new Empty(file, x.hi));
                info("%sPARAM%s %s\n", RED, OFF, x.str().c_str());
                decl.insertVar(x, e);
                if (decl.size() > 0)
                    Ps.push_back(std::move(node));
            }
        }
        return f;
    }
    else
        return parseFuncDecl(file, get(node, "declarator"), Ps);
}

static Chunk parseIdentifier(const File &file, TSNode node)
{
    info("parseIdent(%s%s%s)\n", YELLOW, Chunk(file, lo(node), hi(node)).str().c_str(), OFF);

    Chunk x;
    if (node == "identifier")
    {
        x = Chunk(file, lo(node), hi(node), NAME);
        return x;
    }
    for (auto child: node)
    {
        x = parseIdentifier(file, child);
        if (!x.empty())
            break;
    }
    return x;
}

static void parseLVal(const File &file, TSNode node, std::set<Chunk> &lvals)
{
    if (node == nullptr)
        return;
    Chunk x(file, lo(node), hi(node), NAME);
    if (node == "identifier")
        lvals.insert(x);
    else if (node == "subscript_expression")
        parseLVal(file, get(node, "argument"), lvals);
    else if (node == "attribute")
        parseLVal(file, get(node, "object"), lvals);
    else if (node == "pointer_expression")
        parseLVal(file, get(node, "argument"), lvals);
    else if (node == "parenthesized_expression")
    {
        auto i = after(node, "(");
        if (i != end(node))
            parseLVal(file, *i, lvals);
    }
}

std::string strip_op(const std::string &op)
{
    std::size_t start = 0;
    if (start < op.size() && op[start] == '(') ++start;
    if (start < op.size() && op[start] == '\"') ++start;

    std::size_t end = op.size();
    if (end > 0 && op[end - 1] == ')') --end;
    if (end > 0 && op[end - 1] == '\"') --end;

    return op.substr(start, end - start);
}

static NodePtr parseExpr(const File &file, TSNode node, bool is_lval = false);
static NodePtr getDetail(const File &file, TSNode node, bool is_lval = false)
{
    return std::move(static_cast<Expr*>(parseExpr(file, node, true).get())->detail);
}

static void parseExpr(const File &file, TSNode node,
                      std::set<Chunk> &names, std::set<Chunk> &types,
                      std::set<Chunk> &fields, std::set<Chunk> &lvals,
                      std::vector<NodePtr> &calls, NodePtr &detail, bool is_lval = false)
{
    if (node == nullptr)
        return;
    Chunk x(file, lo(node), hi(node), NAME);
    ssize_t preserve = -1;
    if (node == "identifier")
    {
        names.insert(x);
        detail = std::make_unique<Identifier>(file, lo(node), hi(node));
    }
    else if (node == "type_identifier")
    {
        types.insert(x);
        detail = std::make_unique<Type>(file, lo(node), hi(node));
    }
    else if (node == "field_identifier")
    {
        fields.insert(x);
        detail = std::make_unique<Identifier>(file, lo(node), hi(node));
    }
    else if (node == "attribute")
    {
        NodePtr objDetail = getDetail(file, get(node, "object"));
        NodePtr attrDetail = getDetail(file, get(node, "attribute"));
        detail = std::make_unique<Attribute>(
            file, lo(node), hi(node), std::move(objDetail), ".", std::move(attrDetail));
    }
    else if (node == "integer" || node == "float")
        detail = std::make_unique<NumberLiteral>(file, lo(node), hi(node));
    else if (node == "true" || node == "false")
        detail = std::make_unique<BooleanLiteral>(file, lo(node), hi(node));
    else if (node == "none")
        detail = std::make_unique<NullLiteral>(file, lo(node), hi(node));
    else if (node == "string" || node == "concatenated_string")
        detail = std::make_unique<StringLiteral>(file, lo(node), hi(node));
    else if (node == "list")
    {
        std::vector<NodePtr> elems;
        for (auto child: node)
        {
            if (child == "[" || child == "]" || child == "," || child == "...")
                continue;
            NodePtr elem = parseExpr(file, child);
            if (elem != nullptr)
                elems.push_back(std::move(elem));
        }
        detail = std::make_unique<ListLiteral>(file, lo(node), hi(node), elems);
        return;
    }
    else if (node == "slice")
    {
        NodePtr nodes[3] = {nullptr, nullptr, nullptr};
        int index = 0;
        for (auto child : node)
        {
            if (child == ":") { ++index; continue; }
            if (index >= 3) log_error("Invalid slice literal");
            nodes[index] = parseExpr(file, child);
        }
        detail = std::make_unique<SliceLiteral>(file, lo(node), hi(node), MOVE(nodes[0]), MOVE(nodes[1]), MOVE(nodes[2]));
        return;
    }
    else if (node == "parenthesized_expression")
    {
        auto inner = after(node, "(");
        if (inner != end(node))
            preserve = inner.i;
    }
    else if (node == "call")
    {
        TSNode func = get(node, "function");
        bool is_member = false;
        std::vector<NodePtr> argv;
        if (func == "attribute")
        {
            auto name = Chunk(file, lo(func), hi(func)).str();
            TSNode object = get(func, "object");
            func = get(func, "attribute");
            info("\t%sMEMBER CALL%s (%s) [%s:%u] %s%s%s\n", YELLOW, OFF, name.c_str(),
                file.path, file.lineno(lo(node)), CYAN, Chunk(file, lo(node), hi(node)).str().c_str(), OFF);
            is_member = true;
            argv.push_back(parseExpr(file, object));
            parseLVal(file, object, lvals);
        }

        Chunk f(file, lo(func), hi(func), NAME);
        parseExpr(file, func, names, types, fields, lvals, calls, detail);
        TSNode args = get(node, "arguments");
        std::string potential_name = Chunk(file, lo(func), hi(func)).str() + ".__fields__";
        if (!is_member && funcFirstNames.find(potential_name) != funcFirstNames.end())
        {
            info("\t%sMEMBER CALL%s (%s) [%s:%u] %s%s%s\n", YELLOW, OFF, potential_name.c_str(),
                file.path, file.lineno(lo(node)), CYAN, Chunk(file, lo(node), hi(node)).str().c_str(), OFF);
            argv.push_back(NodePtr(new Empty(file, lo(args))));
        }
        for (auto child: args)
        {
            if (child == "(" || child == ")" || child == "," || child == "...")
                continue;
            NodePtr arg = parseExpr(file, child, /*is_lval=*/true);
            if (arg != nullptr)
                argv.push_back(std::move(arg));
        }
        for (const auto& arg : argv)
            for (const auto& name : dynamic_cast<const Expr*>(arg.get())->names)
                names.insert(name);
        NodePtr call(new Call(file, lo(node), hi(node), f, argv));
        info("\t%sCALL%s [%s:%u] %s%s%s\n", YELLOW, OFF, file.path,
                file.lineno(call->lo), CYAN, call->str().c_str(), OFF);
        calls.push_back(std::move(call));

        for (auto child: args)
        {
            if (child == "(" || child == ")" || child == "," || child == "...")
                continue;
            NodePtr arg = parseExpr(file, child, /*is_lval=*/true);
            if (arg != nullptr)
                argv.push_back(std::move(arg));
        }
        detail = std::make_unique<Call>(file, lo(node), hi(node), *detail, argv);
        return;
    }
    else if (node == "assignment" || node == "augmented_assignment")
    {
        TSNode left = get(node, "left");
        parseLVal(file, left, lvals);
        NodePtr leftDetail = getDetail(file, left, true);
        auto op = (node == "assignment" ? "=" : strip_op(str(get(node, "operator"))));
        NodePtr rightDetail = getDetail(file, get(node, "right"), true);
        detail = std::make_unique<Assignment>(
            file, lo(node), hi(node), std::move(leftDetail), op, std::move(rightDetail));
    }
    else if (node == "update_expression" || node == "unary_operator" || node == "not_operator")
    {
        TSNode arg = get(node, "argument");
        if (node == "update_expression") parseLVal(file, arg, lvals);
        NodePtr argDetail = getDetail(file, arg, node == "update_expression");
        std::string op;
        if (node == "not_operator") op = "not";
        else op = strip_op(str(get(node, "operator")));
        detail = std::make_unique<Unary>(file, lo(node), hi(node), std::move(argDetail), op);
    }
    else if (is_lval && node == "pointer_expression" &&
                get(node, "operator") == "&")
    {
        TSNode arg = get(node, "argument");
        auto op = strip_op(str(get(node, "operator")));
        if (is_lval && op == "&")
            parseLVal(file, arg, lvals);
        parseLVal(file, arg, lvals);
        NodePtr argDetail = getDetail(file, arg, true);
        detail = std::make_unique<Unary>(file, lo(node), hi(node), std::move(argDetail), op);
    }
    else if (node == "binary_operator" || node == "comparison_operator" || node == "boolean_operator")
    {
        // TODO: support chained comparison
        NodePtr leftDetail, rightDetail;
        std::string op;
        if (node == "binary_operator" || node == "boolean_operator")
        {
            leftDetail = getDetail(file, get(node, "left"));
            op = strip_op(str(get(node, "operator")));
            rightDetail = getDetail(file, get(node, "right"));
        }
        else
        {
            leftDetail = getDetail(file, get(node, 0u));
            op = strip_op(str(get(node, "operators")));
            rightDetail = getDetail(file, get(node, 2u));
        }
        detail = std::make_unique<Binary>(
            file, lo(node), hi(node), std::move(leftDetail), op, std::move(rightDetail));
    }
    else if (node == "conditional_expression")
    {
        NodePtr cond = getDetail(file, get(node, 2u));
        NodePtr thenDetail = getDetail(file, get(node, 0u));
        NodePtr elseDetail = getDetail(file, get(node, 4u));
        detail = std::make_unique<Ternary>(
            file, lo(node), hi(node), std::move(cond), std::move(thenDetail), std::move(elseDetail));
    }
    else if (node == "subscript")
    {
        // TODO: multi-dim
        NodePtr argDetail = getDetail(file, get(node, "value"));
        NodePtr indexDetail = getDetail(file, get(node, "subscript"));
        detail = std::make_unique<Subscript>(
            file, lo(node), hi(node), std::move(argDetail), std::move(indexDetail));
    }
    else if (node == "expression_statement")
        preserve = 0;
    // TODO: sizeof, ...

    ssize_t cnt = 0;
    for (auto child: node)
    {
        NodePtr innerDetail = nullptr;
        parseExpr(file, child, names, types, fields, lvals, calls, innerDetail);
        if (preserve == cnt) detail = std::move(innerDetail);
        ++cnt;
    }
}

static NodePtr parseExpr(const File &file, TSNode node, bool is_lval)
{
    if (node == nullptr)
        return NodePtr(nullptr);
    std::set<Chunk> names, types, fields, lvals;
    std::vector<NodePtr> calls;
    NodePtr detail = nullptr;
    parseExpr(file, node, names, types, fields, lvals, calls, detail, is_lval);
    calls.shrink_to_fit();
    NodePtr e(new Expr(file, lo(node), hi(node), names, types, fields, lvals, calls, std::move(detail)));
    return e;
}

static void parseDecl(const File &file, TSNode node, Decl &decl)
{
    info("parseDecl(%s%s%s)\n", YELLOW, str(node).c_str(), OFF);
    if (node == nullptr)
        return;
    if (node == "identifier")
    {
        Chunk x = Chunk(file, lo(node), hi(node), NAME);
        NodePtr e(new Empty(file, hi(node)));
        decl.insertVar(x, e);
    }
    else if (node == "type_identifier")
    {
        Chunk t = Chunk(file, lo(node), hi(node), NAME);
        decl.insertType(t);
    }
    else if (node == "init_declarator")
    {
        Chunk x = parseIdentifier(file, get(node, "declarator"));
        if (x.empty())
            return;
        NodePtr e = parseExpr(file, get(node, "value"));
        decl.insertVar(x, e);
    }
    else if (node == "declaration_declarator")
    {
        Chunk x = parseIdentifier(file, get(node, "declarator"));
        if (x.empty())
            return;
        NodePtr e(new Empty(file, hi(node)));
        decl.insertVar(x, e);
    }
    else if (node == "function_declarator")
        parseDecl(file, get(node, "declarator"), decl);
    else
    {
        for (auto child: node)
            parseDecl(file, child, decl);
    }
}

static NodePtr parseForInit(const File &file, TSNode left, TSNode right)
{
    if (left == nullptr || right == nullptr)
        return nullptr;
    DeclPtr decl(new Decl(file, lo(left), hi(right), true));
    Chunk x = Chunk(file, lo(left), hi(left), NAME);
    NodePtr e = parseExpr(file, right);
    decl->insertVar(x, e);
    return decl;
}

static DeclPtr parseFirstDecl(const File &file, TSNode node, TSNode id, TSNode right, const std::string& func_name, const std::string& name)
{
    if (funcFirstNames[func_name].find(name) != funcFirstNames[func_name].end())
        return nullptr;
    funcFirstNames[func_name].insert(name);

    // treat as init declarator
    DeclPtr decl(new Decl(file, lo(node), hi(node)));
    Chunk x = parseIdentifier(file, id);
    if (x.empty())
        return nullptr;
    NodePtr e = parseExpr(file, right);
    decl->insertVar(x, e);

    info("%sFIRST DECL [%s]%s %s%s%s ", GREEN, name.c_str(), OFF, YELLOW, decl->str().c_str(), OFF);
    for (auto &x: decl->vars)
        info("%s ", x.first.str().c_str());
    fputc('\n', stderr);
    return decl;
}

static NodePtr parseAST(const File &file, TSNode node, Chunk funcName, NodePtr first = nullptr)
{
    if (node == nullptr)
        return NodePtr(nullptr);
    else if (node == "comment")
        return NodePtr(new Comment(file, lo(node), hi(node)));
    else if (node == "block")
    {
        std::vector<NodePtr> elems;
        for (auto child: node)
        {
            NodePtr x = parseAST(file, child, funcName);
            if (x)
                elems.push_back(std::move(x));
        }
        return NodePtr(new Block(file, lo(node), hi(node), elems));
    }
    else if (node == "declaration")
    {
        DeclPtr decl(new Decl(file, lo(node), hi(node)));
        parseDecl(file, node, *decl);
        info("%sDECL%s %s%s%s ", GREEN, OFF, YELLOW, decl->str().c_str(), OFF);
        for (auto &x: decl->vars)
            info("%s ", x.first.str().c_str());
        fputc('\n', stderr);
        return decl;
    }
    else if (node == "expression_statement")
    {
        auto first = begin(node);
        if (*first == "assignment")
        {
            // treat first assignment as declaration
            auto id = get(*first, "left");
            auto right = get(*first, "right");
            if (id != "identifier") return parseExpr(file, node);
            auto name = Chunk(file, lo(id), hi(id)).str();
            auto func_name = funcName.str();
            auto decl = parseFirstDecl(file, node, id, right, func_name, name);
            if (decl != nullptr)
                return decl;
        }
        return parseExpr(file, node);
    }
    else if (node == "assert_statement")
    {
        std::set<Chunk> names, types, fields, lvals;
        std::vector<NodePtr> calls;
        auto it = begin(node);
        TSNode func = *it;
        NodePtr detail = nullptr;
        parseExpr(file, func, names, types, fields, lvals, calls, detail);
        std::vector<NodePtr> argv;
        for (auto child: node)
        {
            if (child == "assert") continue;
            if (child == "(" || child == ")" || child == "," || child == "...")
                continue;
            NodePtr arg = parseExpr(file, child, /*is_lval=*/true);
            info("%sARG = %s%s\n", RED, arg->str().c_str(), OFF);
            if (arg != nullptr)
                argv.push_back(std::move(arg));
        }
        NodePtr call(new Assert(file, lo(node), hi(node), argv));
        info("\t%sASSERT%s [%s:%u] %s%s%s\n", YELLOW, OFF, file.path,
            file.lineno(call->lo), CYAN, call->str().c_str(), OFF);
        calls.push_back(std::move(call));
        NodePtr e(new Expr(file, lo(node), hi(node), names, types, fields, lvals, calls, MOVE(detail)));
        return e;
    }
    else if (node == "if_statement")
    {
        NodePtr _cond = parseExpr(file, get(node, "condition"));
        NodePtr _then = parseAST(file, get(node, "consequence"), funcName);
        if (_cond && _then)
        {
            std::vector<NodePtr> child;
            child.push_back(MOVE(_then));
            NodePtr _else = NodePtr{new Empty(file, hi(node))};
            for (const auto& clause : node)
            {
                if (clause == "elif_clause")
                {
                    NodePtr elif_cond = parseExpr(file, get(clause, "condition"));
                    NodePtr elif_body = parseAST(file, get(clause, "consequence"), funcName);
                    child.emplace_back(new Elif(file, lo(clause), hi(clause), elif_cond, elif_body));
                }
                else if (clause == "else_clause")
                    _else = parseAST(file, clause, funcName);
            }
            child.push_back(MOVE(_else));
            return NodePtr(new IfThenElse(file, lo(node), hi(node), _cond, child));
        }
    }
    else if (node == "else_clause")
    {
        auto i = get(node, "body");
        return parseAST(file, i, funcName);
    }
    else if (node == "switch_statement")
    {
        NodePtr _cond = parseExpr(file, get(node, "condition"));
        NodePtr _body = parseAST(file, get(node, "body"), funcName);
        if (_cond && _body)
            return NodePtr(new Switch(file, lo(node), hi(node),
                _cond, _body));
    }
    else if (node == "while_statement")
    {
        NodePtr _cond = parseExpr(file, get(node, "condition"));
        NodePtr _body = parseAST(file, get(node, "body"), funcName);
        if (_cond && _body)
            return NodePtr(new While(file, lo(node), hi(node),
                _cond, _body));
    }
    else if (node == "do_statement")
    {
        NodePtr _body = parseAST(file, get(node, "body"), funcName);
        NodePtr _cond = parseExpr(file, get(node, "condition"));
        if (_cond && _body)
            return NodePtr(new DoWhile(file, lo(node), hi(node),
                _body, _cond));
    }
    else if (node == "for_statement")
    {
        // TODO: for else
        NodePtr _init = parseForInit(file, get(node, "left"), get(node, "right"));
        NodePtr _cond = parseExpr(file, get(node, "right")), _incr = nullptr;
        NodePtr _body = parseAST(file, get(node, "body"), funcName);
        if (_body)
        {
            auto fixup = [] (NodePtr &next, NodePtr &node)
            {
                if (node == nullptr)
                    node = NodePtr(new Empty(next->file(), next->lo));
            };
            fixup(_body, _incr);
            fixup(_incr, _cond);
            fixup(_cond, _init);
            return NodePtr(new For(file, lo(node), hi(node),
                _init, _cond, _incr, _body));
        }
    }
    else if (node == "break_statement")
    {
        return NodePtr(new Break(file, lo(node), hi(node)));
    }
    else if (node == "continue_statement")
    {
        return NodePtr(new Continue(file, lo(node), hi(node)));
    }
    else if (node == "case_statement")
    {
        auto i = after(node, ":");
        if (i == end(node))
            return NodePtr(nullptr);
        std::vector<NodePtr> elems;
        for (auto child: node)
        {
            NodePtr x = parseAST(file, child, funcName);
            if (x)
                elems.push_back(std::move(x));
        }
        i = begin(node);
        if (*i == "default")
            return NodePtr(new Default(file, lo(node), hi(node), elems));
        else if (*i == "case")
        {
            ++i;
            NodePtr _cond = parseExpr(file, *i);
            if (_cond)
                return
                    NodePtr(new Case(file, lo(node), hi(node), _cond, elems));
        }
    }
    else if (node == "labeled_statement")
    {
        NodePtr _cond = parseExpr(file, get(node, "label"));
        auto i = after(node, ":");
        NodePtr _body = parseAST(file, *i, funcName);
        if (_cond && _body)
            return NodePtr(new Label(file, lo(node), hi(node), _cond, _body));
    }
    else if (node == "goto_statement")
    {
        NodePtr _labl = parseExpr(file, get(node, "label"));
        if (_labl)
            return NodePtr(new Goto(file, lo(node), hi(node), _labl));
    }
    else if (node == "return_statement")
    {
        auto i = after(node, "return");
        auto child = *i;
        if (child != nullptr && child != ";")
        {
            NodePtr _val = parseExpr(file, child);
            if (_val != nullptr)
                return NodePtr(new Return(file, lo(node), hi(node), _val));
        }
        else if (first != nullptr)
            return NodePtr(new Return(file, lo(node), hi(node), first));
        else
            return NodePtr(new Return(file, lo(node), hi(node)));
    }
    else if (node == "preproc_if" || node == "preproc_ifdef")
    {
        std::vector<NodePtr> elems;
        for (auto child: node)
        {
            NodePtr x = parseAST(file, child, funcName);
            if (x)
                elems.push_back(std::move(x));
        }
        return NodePtr(new PreprocIf(file, lo(node), hi(node), elems));
    }
    else if (node == "{" || node == "}")
        ;
    else
    {
        warning("node type \"%s\" not handled", ts_node_type(node));
    }

    return NodePtr(nullptr);
}

static void initParser(void)
{
    if (parser != nullptr)
        return;
    parser = ts_parser_new();
    ts_parser_set_language(parser, tree_sitter_python());
}

static void parseFunc(const File &file, TSNode func, const Struct* parent = nullptr)
{
    std::vector<NodePtr> Ps;
    Chunk name = parseFuncDecl(file, func, Ps);
    NodePtr first, first2;
    if (parent != nullptr && func == "function_definition")
    {
        first = getFirstParam(file, func); 
        first2 = getFirstParam(file, func);
    }

    TSNode params = get(func, "parameters");
    if (params != nullptr)
    {
        auto funcName = name.str();
        for (auto child: params)
        {
            if (child != "identifier") continue;
            auto childName = Chunk(file, lo(child), hi(child)).str();
            log_info("FirstName Param:", funcName, childName);
            funcFirstNames[funcName].insert(childName);
        }
    }
    NodePtr body = parseAST(file, get(func, "body"), name, std::move(first));
    if (body == nullptr)
    {
        if (first2 == nullptr)
            body = NodePtr(new Empty(file, hi(func)));
        else body = NodePtr(new Return(file, hi(func), hi(func), first2));
    }
    else if (auto ptr = dynamic_cast<Block*>(body.get()))
    {
        if (first2 && (ptr->child.empty() || dynamic_cast<Return*>(ptr->child.back().get()) == nullptr))
        {
            info("INSERT RETURN [%s] %s%s%s [%u]\n", file.path, RED,
                name.str().c_str(), OFF, file.lineno(hi(func)));
            ptr->child.push_back(NodePtr(new Return(file, hi(func), hi(func), first2)));
        }
    }

    if (name.kind != END)
    {
        auto name_str = name.str();
        if (parent != nullptr) name_str = parent->name.str() + "." + name_str;
        info("FUNC [%s] %s%s%s [%u..%u]\n", file.path, RED,
            name_str.c_str(), OFF,
            file.lineno(lo(func)), file.lineno(hi(func)));
        NodePtr f(new Func(file, lo(func), hi(func), name, Ps, body, parent));
        auto i = ITEMS.emplace(std::piecewise_construct,
            std::forward_as_tuple(name),
            std::forward_as_tuple(std::move(f)));
        SCOPES.insert(i->second);
    }
    else
        info("FAIL [%s] [%u..%u] %s\n", file.path,
            file.lineno(lo(func)), file.lineno(hi(func)), str(func).c_str());
}

static void parseField(const File &file, TSNode param, Decl &decl)
{
    for (auto child: param)
    {
        if (child == "field_identifier")
        {
            Chunk x(file, lo(child), hi(child), NAME);
            NodePtr e(new Empty(file, x.hi));
            decl.insertVar(x, e);
        }
        else if (child == "type_identifier")
        {
            Chunk t = Chunk(file, lo(child), hi(child), NAME);
            decl.insertType(t);
        }
        else if (param == "array_declarator" && child == "[")
            break;
        else
            parseField(file, child, decl);
    }
}


static void parseFields(const File &file, TSNode node,
    std::vector<NodePtr> &Fs, Chunk structName)
{
    if (node == nullptr)
        return;
    for (auto child: node)
    {
        if (child == "function_definition")
        {
            TSNode params = get(child, "name");
            auto func_name = Chunk(file, lo(params), hi(params)).str();
            if (func_name != "__init__") continue;
            TSNode body = get(child, "body");
            if (body == nullptr) continue;
            for (auto stmt : body)
                if (stmt == "expression_statement")
                {
                    auto first = begin(stmt);
                    if (*first == "assignment")
                    {
                        // try to find first self.xxx = yyy
                        auto id = get(*first, "left");
                        auto right = get(*first, "right");
                        if (id != "attribute") continue;
                        auto obj = get(id, "object");
                        if (Chunk(file, lo(obj), hi(obj)).str() != "self") continue;
                        auto attr = get(id, "attribute");
                        auto name = Chunk(file, lo(attr), hi(attr)).str();
                        auto func_name = structName.str() + ".__fields__";
                        auto decl = parseFirstDecl(file, stmt, attr, right, func_name, name);
                        if (decl != nullptr)
                        {
                            NodePtr node_ptr(new Decl(file, lo(stmt), hi(stmt)));
                            Fs.push_back(std::move(node_ptr));
                        }
                    }
                }
        }
        else if (child == "comment")
        {
            NodePtr c(new Comment(file, lo(child), hi(child)));
            Fs.push_back(std::move(c));
        }
    }
}

static Struct *parseStruct(const File &file, TSNode _struct)
{
    TSNode child = get(_struct, "name");
    if (child == nullptr || child != "identifier")
        return nullptr;
    Chunk name(file, lo(child), hi(child), NAME);
    std::vector<NodePtr> Fs;
    child = get(_struct, "body");
    NodePtr body(nullptr);
    if (child != nullptr)
    {
        parseFields(file, child, Fs, name);
        body = NodePtr(new StructBody(file, lo(child), hi(child), Fs));
    }
    else
        body = NodePtr(new Empty(file, hi(_struct)));

    info("STRUCT [%s] %s%s%s [%u..%u]\n", file.path, GREEN,
        name.str().c_str(), OFF,
        file.lineno(lo(_struct)), file.lineno(hi(_struct)));

    NodePtr s(new Struct(file, lo(_struct), hi(_struct), name, body));
    for (auto member : child)
        if (member == "function_definition")
            parseFunc(file, member, static_cast<const Struct*>(s.get()));
    auto i = ITEMS.emplace(std::piecewise_construct,
        std::forward_as_tuple(name),
        std::forward_as_tuple(std::move(s)));
    SCOPES.insert(i->second);
    return &dynamic_cast<Struct &>(*i->second);
}

static void parseEnums(const File &file, TSNode node,
    std::vector<NodePtr> &Es)
{
    if (node == nullptr)
        return;
    Node *prev = nullptr;
    for (auto child: node)
    {
        if (child == "enumerator")
        {
            TSNode node = child;
            child = get(node, "name");
            Chunk name(file, lo(child), hi(child), NAME);
            NodePtr val = parseExpr(file, get(node, "value"));
            if (val == nullptr)
                val = NodePtr(new Empty(file, hi(child)));
            NodePtr item(new EnumItem(file, lo(child), hi(child), name, val));
            prev = item.get();
            Es.push_back(std::move(item));
            continue;
        }
        else if (child == "comment")
        {
            NodePtr c(new Comment(file, lo(child), hi(child)));
            Es.push_back(std::move(c));
        }
        else if (child == "," && prev != nullptr)
            prev->hi = hi(child);
        prev = nullptr;
    }
}

static Enum *parseEnum(const File &file, TSNode _enum)
{
    TSNode child = get(_enum, "name");
    Chunk name;
    if (child != nullptr)
        name = Chunk(file, lo(child), hi(child), NAME);
    std::vector<NodePtr> Es;
    child = get(_enum, "body");
    NodePtr body(nullptr);
    if (child != nullptr)
    {
        parseEnums(file, child, Es);
        body = NodePtr(new EnumBody(file, lo(child), hi(child), Es));
    }
    else
        body = NodePtr(new Empty(file, hi(_enum)));

    info("ENUM [%s] %s%s%s [%u..%u]\n", file.path, GREEN,
        (name.empty()? "": name.str().c_str()), OFF,
        file.lineno(lo(_enum)), file.lineno(hi(_enum)));

    NodePtr e(new Enum(file, lo(_enum), hi(_enum), name, body));
    auto i = ITEMS.emplace(std::piecewise_construct,
        std::forward_as_tuple(name.empty()? *e: name),
        std::forward_as_tuple(std::move(e)));
    SCOPES.insert(i->second);
    return &dynamic_cast<Enum &>(*i->second);
}

static void parseTypedefNames(const File &file, TSNode node, std::vector<Chunk> &names)
{
    if (node == nullptr || node == "type_definition_type")
        return;
    if (node == "type_identifier")
    {
        Chunk t(file, lo(node), hi(node), NAME);
        names.push_back(t);
    }
    else if (node == "array_declarator" || node == "function_declarator")
        parseTypedefNames(file, get(node, "declarator"), names);
    else
    {
        for (auto child: node)
            parseTypedefNames(file, child, names);
    }
}

static void parseTypedef(const File &file, TSNode _typedef)
{
    TSNode child = get(_typedef, "type");
    NodePtr type(nullptr);
    if (child == nullptr)
        return;
    else if ((child == "class_definition" || child == "union_specifier") &&
                get(child, "body") != nullptr)
    {
        TSNode _struct = child;
        child = get(_struct, "name");
        Chunk name;
        if (child != nullptr)
            name = Chunk(file, lo(child), hi(child), NAME);
        std::vector<NodePtr> Fs;
        child = get(_struct, "body");
 	    parseFields(file, child, Fs, name);
    	NodePtr body(new StructBody(file, lo(child), hi(child), Fs));
        type = NodePtr(new Struct(file, lo(_struct), hi(_struct), name, body));
    }
    else if (child == "enum_specifier" && get(child, "body") != nullptr)
    {
        TSNode _enum = child;
        child = get(_enum, "name");
        Chunk name;
        if (child != nullptr)
            name = Chunk(file, lo(child), hi(child), NAME);
        std::vector<NodePtr> Es;
        child = get(_enum, "body");
 	    parseEnums(file, child, Es);
    	NodePtr body(new EnumBody(file, lo(child), hi(child), Es));
        type = NodePtr(new Enum(file, lo(_enum), hi(_enum), name, body));
    }
    else
        type = NodePtr(new Type(file, lo(child), hi(child)));

    std::vector<Chunk> names;
    parseTypedefNames(file, get(_typedef, "declarator"), names);
    if (names.size() == 0)
        return;

    NodePtr t(new Typedef(file, lo(_typedef), hi(_typedef), type, names));
    Typedef &tdef = dynamic_cast<Typedef &>(*t);
    auto i = ITEMS.emplace(Chunk(tdef), std::move(t));
    SCOPES.insert(i->second);
 
    info("TYPEDEF [%s] %s%s%s [%u..%u]\n", file.path, GREEN,
        i->second->str().c_str(), OFF,
        file.lineno(lo(_typedef)), file.lineno(hi(_typedef)));
}

Chunk getIncludePath(Chunk path);

static void parseItems(const File &file, TSNode root, int depth,
    std::vector<Chunk> &includes)
{
    Node *prev = nullptr;
    info("%s%s%s:\n", GREEN, file.path, OFF);

    for (auto child: root)
    {
        info("\t%u: %s%s%s\n", file.lineno(lo(child)), MAGENTA,
            ts_node_type(child), OFF);
        if (child == "function_definition")
            parseFunc(file, child);
        else if (child == "class_definition")
        {
            prev = parseStruct(file, child);
            continue;
        }
        else if (child == "union_specifier")
        {
            prev = parseStruct(file, child);
            continue;
        }
        else if (child == "enum_specifier")
        {
            prev = parseEnum(file, child);
            continue;
        }
        else if (child == "type_definition")
            parseTypedef(file, child);
        else if (child == "declaration")
        {
            NodePtr d(new Decl(file, lo(child), hi(child)));
            Decl &decl = dynamic_cast<Decl &>(*d);
            parseDecl(file, child, decl);
            auto i = ITEMS.emplace(Chunk(decl), std::move(d));
            SCOPES.insert(i->second);
        }
        else if (child == "comment")
        {
            NodePtr c(new Comment(file, lo(child), hi(child)));
            auto i = ITEMS.emplace(Chunk(*c), std::move(c));
            SCOPES.insert(i->second);
        }
        else if (child == "preproc_def" || child == "preproc_function_def")
        {
            Chunk name  = parseChunk(file, get(child, "name"), NAME); 
            Chunk value = parseChunk(file, get(child, "value"), VALUE);
            if (!name.empty() && !value.empty())
            {
                NodePtr d(new Define(file, lo(child), hi(child), name, value));
                auto i = ITEMS.emplace(name, std::move(d));
                SCOPES.insert(i->second);
            }
        }
        else if (child == "preproc_include")
        {
            Chunk path = parseChunk(file, get(child, "path"), STRING);
            if (!path.empty())
            {
                NodePtr i(new Include(file, lo(child), hi(child), path));
                Include &inc = dynamic_cast<Include &>(*i);
                auto j = ITEMS.emplace(Chunk(inc), std::move(i));
                SCOPES.insert(j->second);
                path = getIncludePath(path);
                if (!path.empty())
                    includes.push_back(path);
            }
        }
        else if (child == "preproc_if" || child == "preproc_ifdef" ||
                 child == "preproc_elif" || child == "preproc_elifdef" ||
                 child == "preproc_else" || child == "linkage_specification" ||
                 child == "ERROR")
        {
            TSNode node = child;
            parseItems(file, node, depth, includes);
        }
        else if (child == "ERROR")
        {
            TSNode node = child;
            for (auto child: node)
                info("%s%s%s\n", RED, str(child).c_str(), OFF);
        }
        else if (child == ";" && prev != nullptr)
            prev->hi = hi(child);
        prev = nullptr;
    }
}

#if 0
/*
 * Works arounds tree-sitter-c parsing limitations/bugs (?) by dropping a
 * token and attempting to re-parse.  Seems to work well in a lot of cases.
 */
static void parseItemsAgain(const File &file, TSNode block, int depth,
    int skip, std::vector<Chunk> &includes)
{
    if (depth < 0)
        return;     // Give up
    depth--;

    auto i = begin(block);
    if (i == end(block))
        return;
    for (int j = 0; j < skip && i != end(block); j++)
        ++i;
    if (i == end(block))
        return;
    uint32_t lb = lo(*i), ub = hi(block);
    if (lb >= ub)
        return;

    const char *src = &file[0];
    TSTree *tree = ts_parser_parse_string(parser, nullptr, src + lb, ub - lb);
    ts_parser_reset(parser);
    TSPoint extent = {file.lineno(lb), 1};
    TSNode root = ts_tree_root_node_with_offset(tree, lb, extent);
    parseItems(file, root, depth, includes);
    ts_tree_delete(tree);
}
#endif

static void parseModule(const File &file, std::vector<Chunk> &includes)
{
    initParser();

    const char *src = &file[0];
    TSTree *tree = ts_parser_parse_string(parser, nullptr, src, file.size());
    ts_parser_reset(parser);
    TSNode root = ts_tree_root_node(tree);
    info("PARSE %s%s%s (%u) [%s]\n", YELLOW, file.path, OFF,
        file.size(), ts_node_type(root));

    info("Syntax tree: %s\n", ts_node_string(root));
    info("Manual Printing:\n");
    printTree(file, root);
    info("\n");

    parseItems(file, root, /*depth=*/16, includes);
    ts_tree_delete(tree);
}


/*
 * Get the basename of a file.
 */
const char *getBasename(const char *path)
{
    char *tmp = xstrdup(path);
    char *base = basename(tmp);
    base = xstrdup(base);
    xfree((void *)tmp);
    return base;
}

/*
 * Check for suffix.
 */
bool suffix(const char *path, const char *ext)
{
    size_t elen = strlen(ext), plen = strlen(path);
    if (elen > plen) return false;
    return (strcmp(path + plen - elen, ext) == 0);
}

/*
 * Read a file into memory.  The memory should never be free'ed.
 */
static const char *readFile(const char *path, size_t size)
{
    int fd = open(path, O_RDONLY);
    if (fd < 0)
    {
        warning("failed to open file \"%s\": %s", path, strerror(errno));
        return nullptr;
    }
    char *data = nullptr;
    const size_t MAP_MIN = 4 * 4096;
    if (size >= MAP_MIN)
    {
        data = (char *)mmap(nullptr, size, PROT_READ, MAP_SHARED, fd, 0);
        if ((void *)data == MAP_FAILED)
        {
            warning("failed to map file \"%s\": %s", path, strerror(errno));
            data = nullptr;
        }
    }
    if (data == nullptr)
    {
        data = (char *)xmalloc(size);
        for (ssize_t i = 0; i < (ssize_t)size; )
        {
            ssize_t r = read(fd, data + i, size - i);
            if (r <= 0)
            {
                warning("failed to read file \"%s\": %s", path, strerror(errno));
                close(fd);
                xfree(data);
                return nullptr;
            }
            i += r;
        }
    }

    close(fd);
    return data;
}

/*
 * Resolve a filename.
 */
void resolvePath(const char *path, std::vector<const char *> &candidates)
{
    std::vector<std::string> dirs;
    bool needs_free = false;
    const char *ptr = path;
    if (ptr != nullptr && ptr[0] != '/')
    {
        // relative path, try to add current directory
        auto cwd = static_cast<char *>(xmalloc(PATH_MAX * 2 + 1));
        cwd[PATH_MAX * 2] = '\0';
        if (getcwd(cwd, PATH_MAX) == nullptr)
            error("getcwd() error: %s", strerror(errno));
        auto cwd_len = strlen(cwd);
        if (cwd_len == 0)
            error("getcwd() returns empty string!");
        if (cwd[cwd_len - 1] != '/')
        {
            cwd[cwd_len] = '/';
            ++cwd_len;
        }
        strncpy(cwd + cwd_len, ptr, PATH_MAX);
        if (cwd[PATH_MAX * 2] != '\0')
            error("Given path bhave more than PATH_MAX (%d) characters!", PATH_MAX);
        ptr = cwd;
        needs_free = true;
    }
    while (ptr != nullptr)
    {
        std::string dir;
        const char *next = strchr(ptr, '/');
        if (next == nullptr)
            dir += ptr;
        else
            dir.append(ptr, next-ptr+1);
        ptr = (next == nullptr? nullptr: next+1);
        if (dir == "/" || dir == "")
        {
            if (dirs.size() == 0)
                dirs.push_back(dir);
        }
        else if (dir == "./" || dir == ".")
            ;
        else if (dir == "../" || dir == "..")
        {
            if (dirs.size() > 0)
                dirs.pop_back();
        }
        else
            dirs.push_back(dir);
    }
    std::string normal;
    for (const auto &dir: dirs)
        normal += dir;
    path = normal.c_str();
    const char *base = getBasename(path);
    for (auto i = INDEX.find(base);
         i != INDEX.end() && strcmp(i->first, base) == 0; ++i)
    {
        if (suffix(i->second, path))
            candidates.push_back(i->second);
    }
    xfree((void *)base);
    if (needs_free) xfree((void *)ptr);
}

/*
 * Load a file.
 */
static const File &loadFile(const char *path, std::vector<Chunk> &includes)
{
    auto j = FILES.find(path);
    if (j != FILES.end())
        return j->second;

    struct stat buf;
    if (stat(path, &buf) < 0)
        error("failed to stat file \"%s\": %s", path, strerror(errno));
    size_t size = buf.st_size;

    const char *data = readFile(path, size);

    std::vector<uint32_t> linev;
    linev.push_back(0);
    linev.push_back(0);
    for (size_t i = 0; i < size; i++)
    {
        if (data[i] == '\n')
            linev.push_back(i+1);
    }
    linev.push_back(size);
    size_t linesz = linev.size() * sizeof(uint32_t);
    uint32_t *lines = (uint32_t *)xmalloc(linesz);
    memcpy(lines, linev.data(), linesz);

    auto i = FILES.emplace(std::piecewise_construct,
                           std::forward_as_tuple(path),
                           std::forward_as_tuple(path, data, size, lines, linev.size()));
    info("LOAD %s%s%s\n", YELLOW, path, OFF);

    const File &file = i.first->second;
    parseModule(file, includes);

    return file;
}

/*
 * Load a file.
 */
const File *loadFile(const char *path, size_t max)
{
    const File *file = nullptr;
    std::vector<const char *> candidates;
    resolvePath(path, candidates);
    if (candidates.size() == 0)
        warning("failed to find any candidate for path \"%s\"", path);
    if (candidates.size() > max)
    {
        std::string paths;
        for (const auto *candidate: candidates)
        {
            paths += "\n\t";
            paths += candidate;
        }
        error("filename \"%s\" is ambigious; could be:%s%s%s",
              path, YELLOW, paths.c_str(), OFF);
    }
    for (const auto *candidate: candidates)
    {
        std::vector<Chunk> includes;
        file = &loadFile(candidate, includes);
        for (const auto &path: includes)
            (void)loadFile(path.str().c_str(), SIZE_MAX);
    }
    return file;
}

/*
 * Get the absolute path of a file.
 */
static const char *getAbsolutePath(const char *file)
{
    const char *path = realpath(file, nullptr);
    return path;
}

bool addIndex(const char *path)
{
    static std::set<std::string> PATHS;
    path = getAbsolutePath(path);
    if (path == nullptr || PATHS.find(path) != PATHS.end())
        return false;
    PATHS.insert(path);

    const char *base = getBasename(path);
    INDEX.insert({base, path});
    return true;
}

/*
 * File tree walk callback.
 */
static int treeWalkCallback(const char *path, const struct stat *buf, int type,
                            struct FTW *ftwbuf)
{
    if (type != FTW_F)
        return 0;
    if (!suffix(path, ".c") && !suffix(path, ".h") &&
        !suffix(path, ".cpp") && !suffix(path, ".hpp") &&
        !suffix(path, ".java") && !suffix(path, ".py"))
        return 0;
    size_t size = buf->st_size;
    if (size > UINT32_MAX)
        return 0;
    if (!addIndex(path))
        return 0;
    return 0;
}

/*
 * Build the file index.
 */
void buildIndex(const char *path)
{
    if (nftw(path, treeWalkCallback, /*nopenfd=*/64, FTW_PHYS) < 0)
        error("failed to walk \"%s\": %s", path, strerror(errno));
}
