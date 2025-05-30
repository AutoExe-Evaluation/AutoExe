// Universal AST-related classes

#ifndef AUTOBUG_SYMBOLIC_AST_H
#define AUTOBUG_SYMBOLIC_AST_H

#include <cstdint>
#include <cstring>

#include <any>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "itree.h"
#include "common.h"

struct CStrCmp
{
    bool operator()(const char *a, const char *b) const
    {
        return (strcmp(a, b) < 0);
    }
};

struct File;
struct Loc
{
    uint32_t id;            // File ID
    uint32_t pos;           // File offset

    Loc(void) : id(0), pos(0)
    {
        ;
    }
    Loc(uint32_t id, uint32_t pos) : id(id), pos(pos)
    {
        ;
    }

    const File &file(void) const;
    unsigned lineno(void) const;

    static Loc min(void)
    {
        return Loc(0, 0);
    }
    static Loc max(void)
    {
        return Loc(UINT32_MAX, UINT32_MAX);
    }

    Loc &operator=(uint32_t npos)
    {
        pos = npos;
        return *this;
    }
    bool operator==(const Loc &loc) const
    {
        return (id == loc.id && pos == loc.pos);
    }
    bool operator!=(const Loc &loc) const
    {
        return !(*this == loc);
    }
    bool operator<(const Loc &loc) const
    {
        return (id != loc.id? id < loc.id: pos < loc.pos);
    }
    bool operator>=(const Loc &loc) const
    {
        return !(*this < loc);
    }
    bool operator>(const Loc &loc) const
    {
        return (id != loc.id? id > loc.id: pos > loc.pos);
    }
    bool operator<=(const Loc &loc) const
    {
        return !(*this > loc);
    }
    Loc operator+(uint32_t i) const
    {
        return Loc(id, pos+i);
    }
    Loc operator-(uint32_t i) const
    {
        return Loc(id, pos-i);
    }
    Loc &operator--(int x)
    {
        pos = (pos == 0? 0: pos-1);
        return *this;
    }
    Loc &operator++(int x)
    {
        pos++;
        return *this;
    }

    std::string str(void) const;
};

/*
 * Get the ID from a bound.
 */
static inline uint32_t id(size_t b)
{
    return ((uint32_t)(b >> 32));
}

/*
 * Get the pos from a bound.
 */
static inline uint32_t pos(size_t b)
{
    return (uint32_t)b;
}

template <class T>
struct Data
{
    const size_t datasz;                // Data size
    const T *const data;                // Data
    const T oob;                        // OOB-element

    Data(const T *data, size_t size, T oob = 0) :
        datasz(size), data(data), oob(oob)
    {
        ;
    }

    const T &operator[](size_t i) const
    {
        if (i >= datasz)
            return oob;
        return data[i];
    }

    size_t size(void) const
    {
        return datasz;
    }

    const T* begin(void) const
    {
        return data;
    }

    const T* end(void) const
    {
        return data + datasz;
    }
};

struct File
{
    // Each file is assigned an id, which can be used to retrieve the file.
    // This allows for a more compact storage of files, rather than
    // pointers or references.
    static std::vector<const File *> files;

    const char *path;                   // File path
    Data<char> file;                    // File contents
    Data<uint32_t> lines;               // File lines
    uint32_t id;                        // File ID

    File(const char *path, const char *data, size_t size,
         const uint32_t *lines, size_t linesz) :
        path(path), file(data, size, EOF),
        lines(lines, linesz), id(files.size()+1)
    {
        files.push_back(this);
    }

    Loc lb(void) const
    {
        return Loc(id, 0);
    }
    Loc ub(void) const
    {
        return Loc(id, file.size());
    }

    /*
     * Map a position to a line number.
     */
    uint32_t lineno(uint32_t pos) const
    {
        ssize_t lo = 1, hi = lines.size()-2;
        while (lo <= hi)
        {
            ssize_t mid = (lo + hi) / 2;
            if (pos < lines[mid])
                hi = mid-1;
            else if (pos > lines[mid+1]-1)
                lo = mid+1;
            else
                return mid;
        }
        return lo;
    }

    /*
     * Map a lineno to a position.
     */
    Loc pos(uint32_t lineno) const
    {
        return Loc(id, lines[lineno]);
    }

    /*
     * Get the file size.
     */
    uint32_t size(void) const
    {
        return (uint32_t)file.size();
    }

    /*
     * Lookup the ith character of a file.
     */
    const char &operator[](size_t i) const
    {
        return file[i];
    }

    /*
     * Get the file object associated with the ID.
     */
    static PURE const File &get(uint32_t id)
    {
        return *files.at(id-1);
    }
};

typedef std::map<const char *, File, CStrCmp> Files;
typedef std::multimap<const char *, const char *, CStrCmp> Index;

extern Files FILES;        // All loaded source files
extern Index INDEX;        // Filename -> absolute path

/********************************************************************/
/* TOKENS                                                           */
/********************************************************************/

enum Kind : int
{
    END = 0,
    NAME = 1000,
    FUNC_DECL,
    STRUCT_DECL,
    ENUM_DECL,
    TYPEDEF,
    TYPE,
    COMMENT,
    STRING,
    GAP,
    LINE,
    DECL,
    EXPR,
    IDENTIFIER,
    LITERAL,
    ASSIGN,
    UNARY_EXPR,
    BINARY_EXPR,
    TERNARY_EXPR,
    SUBSCRIPT_EXPR,
    CALL,
    ASSERT,
    SIZEOF,
    BLOCK,
    PREPROC_IF,
    STRUCT_BODY,
    ENUM_BODY,
    ENUM_ITEM,
    DEFINE,
    VALUE,
    INCLUDE,
    EMPTY,
    STMT,
    IF_STMT,
    ELIF_STMT,
    FOR_STMT,
    WHILE_STMT,
    DO_WHILE_STMT,
    SWITCH_STMT,
    RETURN_STMT,
    CASE_STMT,
    DEFAULT_STMT,
    BREAK_STMT,
    CONTINUE_STMT,
    GOTO_STMT,
    LABEL_STMT,
};

/*
 * A "chunk" represents some part of a file.  Pass-by-copy.
 */
struct Chunk
{
    uint32_t id;                // Chunk file ID
    uint32_t lo;                // Chunk lo
    uint32_t hi;                // Chunk hi
    int kind;                   // Chunk kind (abstraction)

    Chunk(void) : id(0), lo(0), hi(0), kind(END)
    {
        ;
    }
    Chunk(const File &file, uint32_t lo, uint32_t hi, int kind = END) :
        id(file.id), lo(lo), hi(hi), kind(kind)
    {
        ;
    }
    Chunk(const Chunk &c, uint32_t lo, uint32_t hi) :
        id(c.id), lo(lo), hi(hi), kind(c.kind)
    {
        ;
    }
    Chunk(const Chunk &c, uint32_t lo, uint32_t hi, int kind) :
        id(c.id), lo(lo), hi(hi), kind(kind)
    {
        ;
    }

    Loc lb(void) const
    {
        return Loc(id, lo);
    }
    Loc ub(void) const
    {
        return Loc(id, hi);
    }

    ssize_t size(void) const
    {
        return hi - lo;
    }
    bool empty(void) const
    {
        return (lo >= hi);
    }
    const char *data(size_t pos = 0) const
    {
        const File &file = File::get(id);
        return &file[pos];
    }
    void clear(void)
    {
        kind = END;
        id = lo = hi = 0;
    }

    bool operator==(const Chunk &c) const
    {
        if (kind != c.kind || size() != c.size())
            return false;
        return (memcmp(data(lo), c.data(c.lo), size()) == 0);
    }
    bool operator!=(const Chunk &c) const
    {
        return !(*this == c);
    }
    bool operator<(const Chunk &c) const
    {
        if (kind != c.kind)
            return (kind < c.kind);
        int cmp = memcmp(data(lo), c.data(c.lo), std::min(size(), c.size()));
        if (cmp != 0)
            return (cmp < 0);
        return (size() < c.size());
    }
    bool operator<=(const Chunk &c) const
    {
        if (kind != c.kind)
            return (kind < c.kind);
        int cmp = memcmp(data(lo), c.data(c.lo), std::min(size(), c.size()));
        if (cmp != 0)
            return (cmp < 0);
        return (size() <= c.size());
    }
    bool operator>(const Chunk &c) const
    {
        return !(*this <= c);
    }
    bool operator>=(const Chunk &c) const
    {
        return !(*this < c);
    }

    bool operator==(const char *s) const
    {
        if (size() != (uint32_t)strnlen(s, UINT32_MAX))
            return false;
        return (memcmp(data(lo), s, size()) == 0);
    }
    bool operator!=(const char *s) const
    {
        return !(*this == s);
    }

    const File &file(void) const
    {
        return File::get(id);
    }
    char operator[](size_t i) const
    {
        if (i >= size())
            return '\0';
        const File &file = File::get(id);
        return file[i + lo];
    }

    void print(FILE *stream) const
    {
        fwrite(data(lo), sizeof(char), size(), stream);
    }

    std::string str(void) const
    {
        return std::string(data(lo), hi - lo);
    }
};

/********************************************************************/
/* TOKENIZER                                                        */
/********************************************************************/

static void tokenize(Chunk x, std::vector<Chunk> &names)
{
    const char *data = x.data();
    for (uint32_t i = x.lo; i < x.hi; i++)
    {
        if (isalpha(data[i]) || data[i] == '_')
        {
            uint32_t lo = i;
            for (i++; i < x.hi && (isalnum(data[i]) || data[i] == '_'); i++)
                ;
            Chunk name(x, lo, i, NAME);
            names.push_back(name);
        }
    }
    names.shrink_to_fit();
}

/********************************************************************/
/* PARSER                                                           */
/********************************************************************/

// All possible resulting types
class ValueType;
using ProgramValue = std::variant<std::monostate, bool, std::int64_t, double, char, std::string, const void*, std::vector<ValueType>>;

enum class TypeIndex : std::size_t
{
    VOID = 0, BOOL, INT, DOUBLE, CHAR, STRING, POINTER, LIST, MAX
};

[[nodiscard]] static std::string get_type_name(TypeIndex index)
{
    static const std::array<std::string, static_cast<int>(TypeIndex::MAX)> names = {
        "void", "bool", "int", "double", "char", "string", "pointer", "list"
    };
    if (index >= TypeIndex::MAX) error("Invalid type index: %d\n", static_cast<int>(index));
    return names[static_cast<int>(index)];
}

/**
 * ValueType: Representing all the types that an expression in a program can result in
 */
class ValueType
{
private:
    ProgramValue value;

public:
    template<typename T>
    [[nodiscard]] explicit ValueType(const T& value) : value(value) {}

    [[nodiscard]] const ProgramValue& get_value() const
    {
        return value;
    }

    [[nodiscard]] bool operator==(const ValueType& other) const
    {
        return value == other.value;
    }
    [[nodiscard]] bool operator!=(const ValueType& other) const
    {
        return !(*this == other);
    }

    [[nodiscard]] std::string to_string() const
    {
        return get_type_name(static_cast<TypeIndex>(value.index())) + "(" + ::to_string(value) + ")";
    }

    friend std::ostream& operator<<(std::ostream& out, const ValueType& value)
    {
        out << value.to_string();
        return out;
    }
};

// Helper enum class
enum class EvalResultType : std::size_t
{
    VALUE = 0, EXCEPTION, MAX
};

[[nodiscard]] static std::string get_type_name(EvalResultType index)
{
    static const std::array<std::string, static_cast<int>(EvalResultType::MAX)> names = {
        "Value", "Exception"
    };
    if (index >= EvalResultType::MAX) error("Invalid eval result type index: %d\n", static_cast<int>(index));
    return names[static_cast<int>(index)];
}

class Trace;

// Visitor pattern
struct Empty;
struct Comment;
struct IfThenElse;
struct Elif;
struct For;
struct While;
struct DoWhile;
struct Switch;
struct Case;
struct Default;
struct Label;
struct Continue;
struct Break;
struct Goto;
struct Return;
struct Stmt;
struct Block;
struct PreprocIf;
struct Decl;
struct Call;
struct Assert;
struct Sizeof;
struct Identifier;
struct Attribute;
struct NumberLiteral;
struct BooleanLiteral;
struct StringLiteral;
struct NullLiteral;
struct CharLiteral;
struct ListLiteral;
struct SliceLiteral;
struct Assignment;
struct Unary;
struct Binary;
struct Ternary;
struct Subscript;
struct Expr;
struct Func;
struct StructBody;
struct Struct;
struct EnumItem;
struct EnumBody;
struct Enum;
struct Type;
struct Typedef;
struct Define;
struct Include;
template<typename R>
struct NodeVisitor
{
    virtual ~NodeVisitor() = 0;
    virtual R default_value(const Trace&) { return {}; }
    virtual R apply(const Empty&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Comment&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const IfThenElse&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Elif&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const For&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const While&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const DoWhile&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Switch&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Case&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Default&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Label&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Continue&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Break&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Goto&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Return&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Stmt&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Block&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const PreprocIf&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Decl&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Call&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Assert&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Sizeof&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Identifier&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Attribute&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const NumberLiteral&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const BooleanLiteral&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const StringLiteral&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const NullLiteral&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const CharLiteral&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const ListLiteral&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const SliceLiteral&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Assignment&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Unary&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Binary&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Ternary&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Subscript&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Expr&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Func&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const StructBody&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Struct&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const EnumItem&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const EnumBody&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Enum&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Type&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Typedef&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Define&, const Trace& trace) { return default_value(trace); }
    virtual R apply(const Include&, const Trace& trace) { return default_value(trace); }
};

template<typename R>
NodeVisitor<R>::~NodeVisitor() = default;

/*
 * Abstract Syntax Tree (AST).
 */
struct Node;
typedef std::unique_ptr<Node> NodePtr;
struct Node : public Chunk
{
    Node(const File &file, uint32_t lo, uint32_t hi, int kind) :
        Chunk(file, lo, hi, kind)
    {
        ;
    }

    virtual ~Node()
    {
        ;
    }

    struct iterator
    {
        const NodePtr *ptr;

        iterator(const NodePtr *ptr) : ptr(ptr)
        {
            ;
        }
        iterator(const iterator &i) : ptr(i.ptr)
        {
            ;
        }
        iterator& operator=(const iterator &i)
        {
            ptr = i.ptr;
            return *this;
        }

        const NodePtr &operator*(void)
        {
            return *ptr;
        }
        const NodePtr *operator->(void)
        {
            return ptr;
        }
        const NodePtr &operator[](ssize_t i) const
        {
            return ptr[i];
        }
        void operator++(void)
        {
            ptr++;
        }
        void operator--(void)
        {
            ptr--;
        }
        bool operator==(const iterator &i)
        {
            return (ptr == i.ptr);
        }
        bool operator!=(const iterator &i)
        {
            return (ptr != i.ptr);
        }
    };

    virtual iterator begin(void) const
    {
        return iterator(nullptr);
    };
    virtual iterator end(void) const
    {
        return iterator(nullptr);
    }
    virtual ssize_t size(void) const
    {
        return 0;
    }
    virtual std::string repr(void) const = 0;
    bool empty(void) const
    {
        return size() == 0;
    }
    virtual ProgramValue accept(NodeVisitor<ProgramValue>*, const Trace&) const = 0;
    virtual std::any accept(NodeVisitor<std::any>*, const Trace&) const = 0;

    virtual iterator getCond(void) const
    {
        return iterator(nullptr);
    }
    virtual iterator getBody(void) const
    {
        return iterator(nullptr);
    }
};

// Debug functions
void printNodeTree(const Node& root, int level = 0);

#define BEGIN()                                                             \
    iterator begin(void) const                                              \
    {                                                                       \
        return iterator(child);                                             \
    }
#define END()                                                               \
    iterator end(void) const                                                \
    {                                                                       \
        return iterator(child + sizeof(child) / sizeof(child[0]));          \
    }
#define SIZE()                                                              \
    ssize_t size(void) const                                                \
    {                                                                       \
        return sizeof(child) / sizeof(child[0]);                            \
    }
#define DEFAULT(name)                                                       \
    iterator name(void) const                                               \
    {                                                                       \
        return end();                                                       \
    }
#define REPR(str)                                                           \
    std::string repr(void) const                                            \
    {                                                                       \
        return std::string{"<"} + #str + ">";                               \
    }
#define ACCEPT()                                                            \
    ProgramValue accept(NodeVisitor<ProgramValue>* visitor, const Trace& current_trace) const           \
    {                                                                       \
        return visitor->apply(*this, current_trace);                                       \
    }                                                                       \
    std::any accept(NodeVisitor<std::any>* visitor, const Trace& current_trace) const                   \
    {                                                                       \
        return visitor->apply(*this, current_trace);                                       \
    }

struct Empty : public Node
{
    Empty(const File &file, uint32_t pos) :
        Node(file, pos, pos, EMPTY)
    {
        ;
    }
    Empty(const NodePtr &node) :
        Node(node->file(), node->hi, node->hi, EMPTY)
    {
        ;
    }
    Empty(const NodePtr &node, uint32_t pos) :
        Node(node->file(), pos, pos, EMPTY)
    {
        ;
    }
    REPR(Empty Node);
    ACCEPT();
};
struct Comment : public Node
{
    Comment(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, COMMENT)
    {
        ;
    }
    REPR(Comment);
    ACCEPT();
};
struct IfThenElse : public Node
{
    std::vector<NodePtr> child;

    IfThenElse(const File &file, uint32_t lo, uint32_t hi,
               NodePtr &if_, std::vector<NodePtr> &children) :
        Node(file, lo, hi, IF_STMT)
    {
        child.push_back(std::move(if_));
        for (auto &ast: children)
            child.push_back(std::move(ast));
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    iterator getCond(void) const
    {
        return iterator(&child[0]);
    }
    DEFAULT(getBody);
    REPR(if);
    ACCEPT();
};
struct Elif : public Node
{
    NodePtr child[2];

    Elif(const File &file, uint32_t lo, uint32_t hi,
         NodePtr &cond_, NodePtr &body_) :
        Node(file, lo, hi, ELIF_STMT),
        child{std::move(cond_), std::move(body_)}
    {}

    BEGIN();
    END();
    SIZE();
    iterator getCond(void) const
    {
        return iterator(&child[0]);
    }
    iterator getBody(void) const
    {
        return iterator(&child[1]);
    }
    REPR(elif);
    ACCEPT();
};
struct For : public Node
{
    NodePtr child[4];

    For(const File &file, uint32_t lo, uint32_t hi,
        NodePtr &init_, NodePtr &cond_, NodePtr &inc_, NodePtr &body_) :
        Node(file, lo, hi, FOR_STMT),
        child{std::move(init_), std::move(cond_), std::move(inc_),
              std::move(body_)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    iterator getCond(void) const
    {
        return iterator(&child[1]);
    }
    iterator getBody(void) const
    {
        return iterator(&child[3]);
    }
    REPR(for);
    ACCEPT();
};
struct While : public Node
{
    NodePtr child[2];

    While(const File &file, uint32_t lo, uint32_t hi,
          NodePtr &cond_, NodePtr &body_) :
        Node(file, lo, hi, WHILE_STMT),
        child{std::move(cond_), std::move(body_)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    iterator getCond(void) const
    {
        return iterator(&child[0]);
    }
    iterator getBody(void) const
    {
        return iterator(&child[1]);
    }
    REPR(while);
    ACCEPT();
};
struct DoWhile : public Node
{
    NodePtr child[2];

    DoWhile(const File &file, uint32_t lo, uint32_t hi,
            NodePtr &body_, NodePtr &cond_) :
        Node(file, lo, hi, DO_WHILE_STMT),
        child{std::move(body_), std::move(cond_)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    iterator getCond(void) const
    {
        return iterator(&child[1]);
    }
    iterator getBody(void) const
    {
        return iterator(&child[0]);
    }
    REPR(do-while);
    ACCEPT();
};
struct Switch : public Node
{
    NodePtr child[2];

    Switch(const File &file, uint32_t lo, uint32_t hi,
           NodePtr &expr_, NodePtr &body_) :
        Node(file, lo, hi, SWITCH_STMT),
        child{std::move(expr_), std::move(body_)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    iterator getCond(void) const
    {
        return iterator(&child[0]);
    }
    iterator getBody(void) const
    {
        return iterator(&child[1]);
    }
    REPR(switch);
    ACCEPT();
};
struct Case : public Node
{
    std::vector<NodePtr> child;

    Case(const File &file, uint32_t lo, uint32_t hi,
         NodePtr &e, std::vector<NodePtr> &children) :
        Node(file, lo, hi, CASE_STMT)
    {
        child.push_back(std::move(e));
        for (auto &ast: children)
            child.push_back(std::move(ast));
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    iterator getCond(void) const
    {
        return iterator(&child[0]);
    }
    DEFAULT(getBody);
    REPR(case);
    ACCEPT();
};
struct Default : public Node
{
    std::vector<NodePtr> child;

    Default(const File &file, uint32_t lo, uint32_t hi,
            std::vector<NodePtr> &children) :
        Node(file, lo, hi, DEFAULT_STMT), child(std::move(children))
    {
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(Default);
    ACCEPT();
};
struct Label : public Node
{
    NodePtr child[2];

    Label(const File &file, uint32_t lo, uint32_t hi, NodePtr &l, NodePtr &s) :
        Node(file, lo, hi, LABEL_STMT), child{std::move(l), std::move(s)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    iterator getCond(void) const
    {
        return iterator(&child[0]);
    }
    iterator getBody(void) const
    {
        return iterator(&child[1]);
    }
    REPR(label);
    ACCEPT();
};
struct Continue : public Node
{
    Continue(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, CONTINUE_STMT)
    {
        ;
    }
    REPR(continue);
    ACCEPT();
};
struct Break : public Node
{
    Break(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, BREAK_STMT)
    {
        ;
    }
    REPR(break);
    ACCEPT();
};
struct Goto : public Node
{
    NodePtr child[1];

    Goto(const File &file, uint32_t lo, uint32_t hi, NodePtr &l) :
        Node(file, lo, hi, GOTO_STMT), child{std::move(l)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(goto);
    ACCEPT();
};
struct Return : public Node
{
    NodePtr child[1];

    Return(const File &file, uint32_t lo, uint32_t hi, NodePtr &v) :
        Node(file, lo, hi, RETURN_STMT), child{std::move(v)}
    {
        ;
    }
    Return(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, RETURN_STMT), child{NodePtr(new Empty(file, hi))}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(return);
    ACCEPT();
};
struct Stmt : public Node
{
    NodePtr child[1];

    Stmt(const File &file, uint32_t lo, uint32_t hi, NodePtr &e) :
        Node(file, lo, hi, STMT), child{std::move(e)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(Stmt);
    ACCEPT();
};
struct Block : public Node
{
    std::vector<NodePtr> child;

    Block(const File &file, uint32_t lo, uint32_t hi,
          std::vector<NodePtr> &children) :
        Node(file, lo, hi, BLOCK), child(std::move(children))
    {
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(Block);

    const NodePtr &back(void) const
    {
        return child.back();
    }
    ACCEPT();
};
struct PreprocIf : public Node
{
    std::vector<NodePtr> child;

    PreprocIf(const File &file, uint32_t lo, uint32_t hi,
              std::vector<NodePtr> &children) :
        Node(file, lo, hi, PREPROC_IF), child(std::move(children))
    {
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);

    std::string repr(void) const
    {
        return "<#if>";
    }
    ACCEPT();
};
struct Decl : public Node
{
    std::vector<NodePtr> child;
    std::map<Chunk, size_t> vars;
    std::set<Chunk> types;              // Type names
    bool is_python_for_range;

    Decl(const File &file, uint32_t lo, uint32_t hi, bool is_python_for_range = false) :
        Node(file, lo, hi, DECL), is_python_for_range(is_python_for_range)
    {
        ;
    }

    void insertVar(Chunk x, NodePtr &val)
    {
        vars.insert({x, child.size()});
        child.push_back(std::move(val));
    }

    void insertType(Chunk t)
    {
        types.insert(t);
    }

    ssize_t size(void) const
    {
        return vars.size();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(Decl);
    ACCEPT();
};
typedef std::unique_ptr<Decl> DeclPtr;

struct Call : public Node
{
    Chunk func;
    std::vector<NodePtr> child;

    Call(const File &file, uint32_t lo, uint32_t hi,
         Chunk func, std::vector<NodePtr> &args) :
        Node(file, lo, hi, CALL), func(func), child(std::move(args))
    {
        ;
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(Call);
    ACCEPT();
};

struct Assert : public Node
{
    std::vector<NodePtr> child;

    Assert(const File &file, uint32_t lo, uint32_t hi, std::vector<NodePtr> &args) :
        Node(file, lo, hi, ASSERT), child(std::move(args))
    {}

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(assert);
    ACCEPT();
};

struct Sizeof : public Node
{
    std::vector<NodePtr> child;

    Sizeof(const File &file, uint32_t lo, uint32_t hi, std::vector<NodePtr> &args) :
        Node(file, lo, hi, SIZEOF), child(std::move(args))
    {}

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(sizeof);
    ACCEPT();
};

struct Identifier : public Node
{
    Identifier(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, IDENTIFIER)
    {}
    REPR(Identifier);
    ACCEPT();
};

struct Attribute : public Node
{
    std::string op;
    NodePtr child[2];

    Attribute(const File &file, uint32_t lo, uint32_t hi, NodePtr object, const std::string &op, NodePtr attr) :
        Node(file, lo, hi, ASSIGN), op(op), child{std::move(object), std::move(attr)}
    {}

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    std::string repr(void) const
    {
        return "<Attribute \"" + op + "\">";
    }
    ACCEPT();
};

struct NumberLiteral : public Node
{
    NumberLiteral(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, LITERAL)
    {}
    REPR(NumberLiteral);
    ACCEPT();
};

struct BooleanLiteral : public Node
{
    BooleanLiteral(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, LITERAL)
    {}
    REPR(BooleanLiteral);
    ACCEPT();
};

struct NullLiteral : public Node
{
    NullLiteral(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, LITERAL)
    {}
    REPR(NullLiteral);
    ACCEPT();
};

struct CharLiteral : public Node
{
    CharLiteral(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, LITERAL)
    {}
    REPR(CharLiteral);
    ACCEPT();
};

struct StringLiteral : public Node
{
    StringLiteral(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, LITERAL)
    {}
    REPR(StringLiteral);
    ACCEPT();
};

struct ListLiteral : public Node
{
    std::vector<NodePtr> child;

    ListLiteral(const File &file, uint32_t lo, uint32_t hi, std::vector<NodePtr> &elems) :
        Node(file, lo, hi, LITERAL), child(std::move(elems))
    {}

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(ListLiteral);
    ACCEPT();
};

struct SliceLiteral : public Node
{
    NodePtr child[3]; // start, end, step

    SliceLiteral(const File &file, uint32_t lo, uint32_t hi, NodePtr start, NodePtr end = nullptr, NodePtr step = nullptr) :
        Node(file, lo, hi, LITERAL), child{std::move(start), std::move(end), std::move(step)}
    {}

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(SliceLiteral);
    ACCEPT();
};

struct Assignment : public Node
{
    std::string op;
    NodePtr child[2];

    Assignment(const File &file, uint32_t lo, uint32_t hi, NodePtr left, const std::string &op, NodePtr right) :
        Node(file, lo, hi, ASSIGN), op(op), child{std::move(left), std::move(right)}
    {}

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    std::string repr(void) const
    {
        return "<Assignment \"" + op + "\">";
    }
    ACCEPT();
};

struct Unary : public Node
{
    std::string op;
    NodePtr child[1];

    Unary(const File &file, uint32_t lo, uint32_t hi, NodePtr arg, const std::string &op) :
        Node(file, lo, hi, UNARY_EXPR), op(op), child{std::move(arg)}
    {}

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    std::string repr(void) const
    {
        return "<Unary \"" + op + "\">";
    }
    ACCEPT();
};

struct Binary : public Node
{
    std::string op;
    NodePtr child[2];

    Binary(const File &file, uint32_t lo, uint32_t hi, NodePtr arg1, const std::string &op, NodePtr arg2) :
        Node(file, lo, hi, BINARY_EXPR), op(op), child{std::move(arg1), std::move(arg2)}
    {}

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    std::string repr(void) const
    {
        return "<Binary \"" + op + "\">";
    }
    ACCEPT();
};

struct Ternary : public Node
{
    NodePtr child[3];

    Ternary(const File &file, uint32_t lo, uint32_t hi, NodePtr cond, NodePtr then_node, NodePtr else_node) :
        Node(file, lo, hi, TERNARY_EXPR), child{std::move(cond), std::move(then_node), std::move(else_node)}
    {}

    BEGIN();
    END();
    SIZE();
    iterator getCond(void) const
    {
        return iterator(&child[0]);
    }
    DEFAULT(getBody);
    REPR(Ternary);
    ACCEPT();
};

struct Subscript : public Node
{
    NodePtr child[2];

    Subscript(const File &file, uint32_t lo, uint32_t hi, NodePtr arg, NodePtr index) :
        Node(file, lo, hi, SUBSCRIPT_EXPR), child{std::move(arg), std::move(index)}
    {}

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(Subscript);
    ACCEPT();
};

struct Expr : public Node
{
    const std::vector<Chunk> names;
    const std::vector<Chunk> types;
    const std::vector<Chunk> fields;
    const std::vector<Chunk> lvals;

    const std::vector<NodePtr> child;
    NodePtr detail;

    Expr(const File &file, uint32_t lo, uint32_t hi,
         const std::set<Chunk> &names,
         const std::set<Chunk> &types,
         const std::set<Chunk> &fields,
         const std::set<Chunk> &lvals,
         std::vector<NodePtr> &calls, NodePtr detailExpr) :
        Node(file, lo, hi, EXPR),
        names(names.begin(), names.end()),
        types(types.begin(), types.end()),
        fields(fields.begin(), fields.end()),
        lvals(lvals.begin(), lvals.end()),
        child(std::move(calls)),
        detail(std::move(detailExpr))
    {
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(Expr);
    ACCEPT();
};

/*
 * Variable information, including declaration & users
 */
struct Var
{
    Chunk x;                        // Variable name
    const Decl &decl;               // Variable declaration
    uint32_t lo, hi;                // Variable scope
    uint32_t real_hi;               // Real physical scope (useful for Python only)

    Var(Chunk x, const Decl &decl, uint32_t lo, uint32_t hi) :
        Var(x, decl, lo, hi, hi)
    {}

    Var(Chunk x, const Decl &decl, uint32_t lo, uint32_t hi, uint32_t real_hi) :
        x(x), decl(decl), lo(lo), hi(hi), real_hi(real_hi)
    {}

    bool contains(const NodePtr &ast) const
    {
        return (ast->lo >= lo && ast->hi <= hi);
    }
    bool contains(const Var &v) const
    {
        return (v.lo >= lo && v.hi <= hi);
    }

    size_t lb(void)
    {
        return (size_t)lo;
    }
    size_t ub(void)
    {
        return (size_t)hi;
    }
};

void expandMacros(const Chunk &n, std::set<Chunk> &names);
struct Vars
{
    std::multimap<Chunk, Var> vars;
    std::vector<const Var *>  empty;
    std::map<const Expr *, std::vector<const Var *>>  e2v;      // Usage
    std::map<const Var *,  std::vector<const Expr *>> v2e;      // Users
    std::map<const Decl *, std::vector<const Var *>>  d2v;      // Decl -> var

    const std::vector<const Var *> &get(const Expr &e) const
    {
        auto i = e2v.find(&e);
        return (i == e2v.end()? empty: i->second);
    }
    const std::vector<const Var *> &get(const Decl &d) const
    {
        auto i = d2v.find(&d);
        return (i == d2v.end()? empty: i->second);
    }

    void insert(Chunk x, const Decl &decl, uint32_t lo, uint32_t hi, uint32_t real_hi)
    {
        auto i = vars.emplace(std::piecewise_construct,
                              std::forward_as_tuple(x),
                              std::forward_as_tuple(x, decl, std::max(lo, decl.lo), hi, real_hi));
        std::vector<const Var *> tmp;
        auto j = d2v.insert({&decl, tmp});
        j.first->second.push_back(&i->second);
    }

    void insert(Chunk x, const Decl &decl, uint32_t lo, uint32_t hi)
    {
        insert(x, decl, lo, hi, hi);
    }

    void addUse(const NodePtr &ast)
    {
        const Expr &expr = dynamic_cast<Expr &>(*ast);
        info("%s%s%s: ", RED, expr.str().c_str(), OFF);
        for (const auto &n: expr.names)
        {
            std::set<Chunk> names;
            expandMacros(n, names);
            for (const auto &m: names)
            {
                auto j = vars.find(m), jend = vars.end(), k = vars.end();
                for (; j != jend; ++j)
                {
                    if (j->second.x != m)
                        break;
                    if (!j->second.contains(ast))
                        continue;
                    if (k == jend)
                        k = j;
                    else if (k->second.contains(j->second))
                        k = j;
                }
                if (k == vars.end())
                    continue;
                auto &var = k->second;
                info("%s ", m.str().c_str());
                std::vector<const Var *> ex;
                auto x = e2v.insert({&expr, ex});
                x.first->second.push_back(&var);
                std::vector<const Expr *> ey;
                auto y = v2e.insert({&var, ey});
                y.first->second.push_back(&expr);
            }
        }
        info("\n");
    }

    void shrink(void)
    {
        for (auto &entry: e2v)
            entry.second.shrink_to_fit();
        for (auto &entry: v2e)
            entry.second.shrink_to_fit();
        for (auto &entry: d2v)
            entry.second.shrink_to_fit();
//        for (auto &entry: e2v)
//        {
//            info("%s%s%s: ", RED, entry.first->str().c_str(),
//                OFF);
//            const auto &vs = entry.second;
//            for (const auto *v: vs)
//                info("%s ", v->x.str().c_str());
//            info("\n");
//        }
    }
};

void buildVars(const NodePtr &ast, uint32_t lo, uint32_t hi, uint32_t body_hi, Vars &Vs);

/*
 * An object of interest (function, type, etc.)
 */
struct Scope
{
    const NodePtr &node;

    Scope(const NodePtr &node) : node(node)
    {
        ;
    }

    Loc lb(void) const
    {
        return node->lb();
    }
    Loc ub(void) const
    {
        return node->ub();
    }
};
typedef interval::Tree<Scope, Loc> Scopes;

/*
 * A function definition.
 */
struct Func : public Node
{
    const Struct* parent;
    const Chunk name;
    std::vector<NodePtr> child;

    Func(const File &file, uint32_t lo, uint32_t hi,
         Chunk name, std::vector<NodePtr> &params, NodePtr &body, const Struct* parent = nullptr) :
        Node(file, lo, hi, FUNC_DECL), parent(parent), name(name), child(std::move(params))
    {
        child.push_back(std::move(body));
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    iterator getBody(void) const
    {
        return iterator(&child.back());
    }
    REPR(Func);
    ACCEPT();

    /*
     * Analyze this function.
     */
    mutable Vars vars;
    mutable bool analyzed = false;
    void analyze(void) const
    {
        if (analyzed)
            return;

        auto iend = getBody();
        const NodePtr &body = *iend;
        for (auto i = begin(); i != iend; ++i)
        {
            const NodePtr &param = *i;
            const Decl &decl = dynamic_cast<Decl &>(*param);
            for (auto &entry: decl.vars)
            {
                Chunk x = entry.first;
                vars.insert(x, decl, body->lo, body->hi);
            }
        }

        buildVars(body, 0, INT32_MAX, body->hi, vars);
        vars.shrink();

        analyzed = true;
    }
};

/*
 * A struct definition.
 */
struct StructBody : public Node
{
    std::vector<NodePtr> child;

    StructBody(const File &file, uint32_t lo, uint32_t hi,
               std::vector<NodePtr> &fields) :
        Node(file, lo, hi, STRUCT_BODY), child(std::move(fields))
    {
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(StructBody);
    ACCEPT();
};
struct Struct : public Node
{
    const Chunk name;
    NodePtr child[1];

    Struct(const File &file, uint32_t lo, uint32_t hi,
           Chunk name, NodePtr &body) :
        Node(file, lo, hi, STRUCT_DECL), name(name), child{std::move(body)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    REPR(Struct);
    iterator getBody(void) const
    {
        return iterator(&child[0]);
    }
    ACCEPT();
};
typedef std::multimap<Chunk, const Node *> Fields;

/*
 * An enum definition.
 */
struct EnumItem : public Node
{
    Chunk name;
    NodePtr val;

    EnumItem(const File &file, uint32_t lo, uint32_t hi,
             Chunk name, NodePtr &val) :
        Node(file, lo, hi, ENUM_ITEM), name(name), val(std::move(val))
    {
        ;
    }
    REPR(EnumItem);
    ACCEPT();
};
struct EnumBody : public Node
{
    std::vector<NodePtr> child;

    EnumBody(const File &file, uint32_t lo, uint32_t hi,
             std::vector<NodePtr> &items) :
        Node(file, lo, hi, ENUM_BODY), child(std::move(items))
    {
        child.shrink_to_fit();
    }

    iterator begin(void) const
    {
        return iterator(child.data());
    }
    iterator end(void) const
    {
        return iterator(child.data() + child.size());
    }
    ssize_t size(void) const
    {
        return child.size();
    }
    DEFAULT(getCond);
    DEFAULT(getBody);
    REPR(EnumBody);
    ACCEPT();
};
struct Enum : public Node
{
    const Chunk name;
    NodePtr child[1];

    Enum(const File &file, uint32_t lo, uint32_t hi,
         Chunk name, NodePtr &body) :
        Node(file, lo, hi, ENUM_DECL), name(name), child{std::move(body)}
    {
        ;
    }

    BEGIN();
    END();
    SIZE();
    DEFAULT(getCond);
    REPR(Enum);
    iterator getBody(void) const
    {
        return iterator(&child[0]);
    }
    ACCEPT();
};
typedef std::multimap<Chunk, const Node *> Enums;

/*
 * A (generic) type.
 */
struct Type : public Node
{
    Type(const File &file, uint32_t lo, uint32_t hi) :
        Node(file, lo, hi, TYPE)
    {
        ;
    }
    REPR(Type);
    ACCEPT();
};

/*
 * A typedef definition.
 */
struct Typedef : public Node
{
    NodePtr type;
    std::vector<Chunk> names;

    Typedef(const File &file, uint32_t lo, uint32_t hi,
            NodePtr &type, std::vector<Chunk> &names_0) :
        Node(file, lo, hi, TYPEDEF), type(std::move(type)),
        names(std::move(names_0))
    {
        names.shrink_to_fit();
    }
    REPR(Typedef);
    ACCEPT();
};
typedef std::multimap<Chunk, const Typedef *> Types;

/*
 * A #define macro
 */
struct Define : public Node
{
    Chunk name;
    Chunk value;
    std::vector<Chunk> names;

    Define(const File &file, uint32_t lo, uint32_t hi,
           Chunk name, Chunk value) :
        Node(file, lo, hi, DEFINE), name(name), value(value)
    {
        tokenize(value, names);
    }

    std::string repr(void) const
    {
        return "<#define>";
    }
    ACCEPT();
};

/*
 * A #include directive.
 */
struct Include : public Node
{
    Chunk path;

    Include(const File &file, uint32_t lo, uint32_t hi,
            Chunk path) :
        Node(file, lo, hi, INCLUDE), path(path)
    {
        ;
    }

    std::string repr(void) const
    {
        return "<#include>";
    }
    ACCEPT();
};

#undef BEGIN
#undef END
#undef DEFAULT
#undef SIZE
#undef REPR
#undef ACCEPT

typedef std::multimap<Chunk, NodePtr> Items;
typedef std::multimap<Chunk, const Decl *> Globals;

extern Items  ITEMS;
extern Scopes SCOPES;

extern Fields  FIELDS;
extern Enums   ENUMS;
extern Types   TYPES;
extern Globals GLOBALS;

Chunk getIncludePath(Chunk path);
const Func* getFunc(const Chunk& line);

#endif //AUTOBUG_SYMBOLIC_AST_H
