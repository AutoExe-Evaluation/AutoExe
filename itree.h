#ifndef AUTOBUG_SYMBOLIC_ITREE_H
#define AUTOBUG_SYMBOLIC_ITREE_H

// XXX
#define PAIR(x)     ((uint32_t)((x).id)), ((uint32_t)((x).pos))

namespace interval
{

#define RB_BLACK                    1
#define RB_RED                      0
#define RB_PARENT(N)                (N)->entry.parent
#define RB_LEFT(N)                  (N)->entry.left
#define RB_RIGHT(N)                 (N)->entry.right
#define RB_COLOR(N)                 (N)->color

template <class T, class L> struct Node;
template <class T, class L> struct Tree;

template <class T, class L> static Node<T, L> *insert(Tree<T, L> *t, Node<T, L> *k);
template <class T, class L> static Node<T, L> *remove(Tree<T, L> *t, Node<T, L> *n);
template <class T, class L> static Node<T, L> *search(const Tree<T, L> *t,
    const Node<T, L> *k);
template <class T, class L> static void destroy(Node<T, L> *n);
template <class T, class L> static void dump(FILE *stream, Node<T, L> *n);

/*
 * Interval tree node.
 */
template <class T, class L>
struct Node
{
    T obj;                      // Object
    struct Entry
    {
        Node *parent = nullptr; // RB-tree parent
        Node *left   = nullptr; // RB-tree left
        Node *right  = nullptr; // RB-tree right
    } entry;
    L lb;                       // tree lower bound
    L ub;                       // tree upper bound
    int color;                  // RB-tree color

    Node(const T &obj) : obj(obj),
        lb(obj.lb()), ub(obj.ub()),
        color(RB_RED)
    {
        ;
    }
    template <typename... Args>
    Node(Args&&... args) :
        obj(std::forward<Args>(args)...),
        color(RB_RED)
    {
        lb = obj.lb();
        ub = obj.ub();
    }

    int compare(const Node &n) const
    {
        L lb1 = n.obj.lb(), lb2 = obj.lb();
        if (lb1 != lb2)
            return (lb1 < lb2? -1: 1);
        L ub1 = n.obj.ub(), ub2 = obj.ub();
        if (ub1 != ub2)
            return (ub1 < ub2? -1: 1);
        return 0;
    }

    void fix(void)
    {
        lb = obj.lb();
        ub = obj.ub();
        Node *l = entry.left, *r = entry.right;
        if (l != nullptr)
        {
            lb = std::min(lb, l->lb);
            ub = std::max(ub, l->ub);
        }
        if (r != nullptr)
        {
            lb = std::min(lb, r->lb);
            ub = std::max(ub, r->ub);
        }
    }
};

/*
 * Interval tree.
 */
template <class T, class L>
struct Tree
{
    Node<T, L> *root = nullptr; // Interval tree root
    size_t count  = 0;          // Tree size (# nodes)

    ~Tree(void)
    {
        interval::destroy(root);
    }

    void dump(FILE *stream) const
    {
        interval::dump(stream, root);
    }

    void insert(const T &obj)
    {
        Node<T, L> *n = new Node<T, L>(obj);
        interval::insert(this, n);
        count++;
    }

    template <typename... Args>
    void emplace(Args&&... args)
    {
        Node<T, L> *n = new Node<T, L>(std::forward<Args>(args)...);
        interval::insert(this, n);
        count++;
    }

    L lb(void) const
    {
        return (root == nullptr? L::max(): root->lb);
    }
    L ub(void) const
    {
        return (root == nullptr? L::min(): root->ub);
    }
    bool empty(void) const
    {
        return (root == nullptr);
    }
    size_t size(void) const
    {
        return count;
    }
    /*
     * Iterators.
     */
    struct iterator
    {
        Node<T, L> *node = nullptr;
        L lb;
        L ub;

        iterator() : lb(L::min()), ub(L::max())
        {
            ;
        }
        iterator(const iterator &i) : node(i.node), lb(i.lb), ub(i.ub)
        {
            ;
        }
        iterator(Node<T, L> *node) : node(node), lb(L::min()), ub(L::max())
        {
            ;
        }
        iterator(Node<T, L> *node, L lb, L ub) :
            node(node), lb(lb), ub(ub)
        {
            ;
        }

        const T &operator*(void)
        {
            return node->obj;
        }
        const T* operator->() { return &node->obj; }
        void operator++(void);
        bool operator!=(const iterator &i)
        {
            return (node != i.node);
        }
        bool operator==(const iterator &i)
        {
            return (node == i.node);
        }
        void operator=(const iterator &i)
        {
            node = i.node;
            lb   = i.lb;
            ub   = i.ub;
        }
    };

    iterator find(const T &k) const
    {
        Node<T, L> key(k);
        Node<T, L> *n = interval::search(this, &key);
        return iterator(n);
    }
    iterator scan(L lb, L ub) const;

    iterator begin(void) const;
    iterator end(void) const
    {
        return iterator();
    }

    void erase(iterator i)
    {
        Node<T, L> *n = i.node;
        if (interval::remove(this, n) != nullptr)
            count--;
    }
    void erase(const T &k)
    {
        auto i = find(k);
        erase(i);
    }
    bool intersects(L lb, L ub) const
    {
        return (scan(lb, ub) != end());
    }
    bool contains(L lb, L ub) const
    {
        L lo = lb;
        for (auto i = scan(lb, ub), iend = end(); i != iend; ++i)
        {
            const T &x = *i;
            if (x.lb() > lo)
                return false;
            lo = x.ub();
        }
        return (lo >= ub);
    }
};

/*
 * The implementation uses code that is distantly derived from Niels
 * Provos' red-black tree implementation.  Any element substantial similar
 * to the original protectable expression remains under the BSD license:
 *
 * Copyright 2002 Niels Provos <provos@citi.umich.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

template <class T, class L>
static void fix(Node<T, L> *n)
{
    if (n == nullptr)
        return;
    n->fix();
}

template <class T, class L>
static void rotateLeft(Tree<T, L> *t, Node<T, L> *n)
{
    Node<T, L> *tmp = RB_RIGHT(n);
    if ((RB_RIGHT(n) = RB_LEFT(tmp)) != nullptr)
        RB_PARENT(RB_LEFT(tmp)) = n;
    fix(n);
    if ((RB_PARENT(tmp) = RB_PARENT(n)) != nullptr)
    {
        if (n == RB_LEFT(RB_PARENT(n)))
            RB_LEFT(RB_PARENT(n)) = tmp;
        else
            RB_RIGHT(RB_PARENT(n)) = tmp;
    }
    else
        t->root = tmp;
    RB_LEFT(tmp) = n;
    RB_PARENT(n) = tmp;
    fix(tmp);
    if (RB_PARENT(tmp) != nullptr)
        fix(RB_PARENT(tmp));
}

template <class T, class L>
static void rotateRight(Tree<T, L> *t, Node<T, L> *n)
{
    Node<T, L> *tmp = RB_LEFT(n);
    if ((RB_LEFT(n) = RB_RIGHT(tmp)) != nullptr)
        RB_PARENT(RB_RIGHT(tmp)) = n;
    fix(n);
    if ((RB_PARENT(tmp) = RB_PARENT(n)) != nullptr)
    {
        if (n == RB_LEFT(RB_PARENT(n)))
            RB_LEFT(RB_PARENT(n)) = tmp;
        else
            RB_RIGHT(RB_PARENT(n)) = tmp;
    } else
        t->root = tmp;
    RB_RIGHT(tmp) = n;
    RB_PARENT(n) = tmp;
    fix(tmp);
    if (RB_PARENT(tmp) != nullptr)
        fix(RB_PARENT(tmp));
}

template <class T, class L>
static void rebalanceInsert(Tree<T, L> *t, Node<T, L> *n)
{
    Node<T, L> *parent, *gparent, *tmp;
    for (Node<T, L> *m = n; m != nullptr; m = RB_PARENT(m))
        fix(m);
    while ((parent = RB_PARENT(n)) != nullptr &&
                RB_COLOR(parent) == RB_RED)
    {
        gparent = RB_PARENT(parent);
        if (parent == RB_LEFT(gparent))
        {
            tmp = RB_RIGHT(gparent);
            if (tmp != nullptr && RB_COLOR(tmp) == RB_RED)
            {
                RB_COLOR(tmp)     = RB_BLACK;
                RB_COLOR(parent)  = RB_BLACK;
                RB_COLOR(gparent) = RB_RED;
                n = gparent;
                continue;
            }
            if (RB_RIGHT(parent) == n)
            {
                rotateLeft(t, parent);
                tmp = parent;
                parent = n;
                n = tmp;
            }
            RB_COLOR(parent)  = RB_BLACK;
            RB_COLOR(gparent) = RB_RED;
            rotateRight(t, gparent);
        }
        else
        {
            tmp = RB_LEFT(gparent);
            if (tmp != nullptr && RB_COLOR(tmp) == RB_RED)
            {
                RB_COLOR(tmp)     = RB_BLACK;
                RB_COLOR(parent)  = RB_BLACK;
                RB_COLOR(gparent) = RB_RED;
                n = gparent;
                continue;
            }
            if (RB_LEFT(parent) == n)
            {
                rotateRight(t, parent);
                tmp = parent;
                parent = n;
                n = tmp;
            }
            RB_COLOR(parent)  = RB_BLACK;
            RB_COLOR(gparent) = RB_RED;
            rotateLeft(t, gparent);
        }
    }
    RB_COLOR(t->root) = RB_BLACK;
}

template <class T, class L>
static void rebalanceRemove(Tree<T, L> *t, Node<T, L> *parent, Node<T, L> *n)
{
    Node<T, L> *tmp;
    while ((n == nullptr || RB_COLOR(n) == RB_BLACK) && n != t->root)
    {
        if (RB_LEFT(parent) == n)
        {
            tmp = RB_RIGHT(parent);
            if (RB_COLOR(tmp) == RB_RED)
            {
                RB_COLOR(tmp) = RB_BLACK;
                RB_COLOR(parent) = RB_RED;
                rotateLeft(t, parent);
                tmp = RB_RIGHT(parent);
            }
            if ((RB_LEFT(tmp) == nullptr ||
                    RB_COLOR(RB_LEFT(tmp)) == RB_BLACK) &&
                (RB_RIGHT(tmp) == nullptr ||
                    RB_COLOR(RB_RIGHT(tmp)) == RB_BLACK))
            {
                RB_COLOR(tmp) = RB_RED;
                n = parent;
                parent = RB_PARENT(n);
            }
            else
            {
                if (RB_RIGHT(tmp) == nullptr ||
                    RB_COLOR(RB_RIGHT(tmp)) == RB_BLACK)
                {
                    Node<T, L> *oleft;
                    if ((oleft = RB_LEFT(tmp)) != nullptr)
                        RB_COLOR(oleft) = RB_BLACK;
                    RB_COLOR(tmp) = RB_RED;
                    rotateRight(t, tmp);
                    tmp = RB_RIGHT(parent);
                }
                RB_COLOR(tmp) = RB_COLOR(parent);
                RB_COLOR(parent) = RB_BLACK;
                if (RB_RIGHT(tmp))
                    RB_COLOR(RB_RIGHT(tmp)) = RB_BLACK;
                rotateLeft(t, parent);
                n = t->root;
                break;
            }
        }
        else
        {
            tmp = RB_LEFT(parent);
            if (RB_COLOR(tmp) == RB_RED)
            {
                RB_COLOR(tmp) = RB_BLACK;
                RB_COLOR(parent) = RB_RED;
                rotateRight(t, parent);
                tmp = RB_LEFT(parent);
            }
            if ((RB_LEFT(tmp) == nullptr ||
                    RB_COLOR(RB_LEFT(tmp)) == RB_BLACK) &&
                (RB_RIGHT(tmp) == nullptr ||
                    RB_COLOR(RB_RIGHT(tmp)) == RB_BLACK))
            {
                RB_COLOR(tmp) = RB_RED;
                n = parent;
                parent = RB_PARENT(n);
            }
            else
            {
                if (RB_LEFT(tmp) == nullptr ||
                    RB_COLOR(RB_LEFT(tmp)) == RB_BLACK)
                {
                    Node<T, L> *oright;
                    if ((oright = RB_RIGHT(tmp)) != nullptr)
                        RB_COLOR(oright) = RB_BLACK;
                    RB_COLOR(tmp) = RB_RED;
                    rotateLeft(t, tmp);
                    tmp = RB_LEFT(parent);
                }
                RB_COLOR(tmp) = RB_COLOR(parent);
                RB_COLOR(parent) = RB_BLACK;
                if (RB_LEFT(tmp))
                    RB_COLOR(RB_LEFT(tmp)) = RB_BLACK;
                rotateRight(t, parent);
                n = t->root;
                break;
            }
        }
    }
    if (n != nullptr)
        RB_COLOR(n) = RB_BLACK;
}       

template <class T, class L>
static Node<T, L> *remove(Tree<T, L> *t, Node<T, L> *n)
{
    if (n == nullptr)
        return nullptr;
    Node<T, L> *child, *parent, *old = n;
    int color;
    if (RB_LEFT(n) == nullptr)
        child = RB_RIGHT(n);
    else if (RB_RIGHT(n) == nullptr)
        child = RB_LEFT(n);
    else
    {
        Node<T, L> *left;
        n = RB_RIGHT(n);
        while ((left = RB_LEFT(n)) != nullptr)
            n = left;
        child = RB_RIGHT(n);
        parent = RB_PARENT(n);
        color = RB_COLOR(n);
        if (child != nullptr)
            RB_PARENT(child) = parent;
        if (parent != nullptr)
        {
            if (RB_LEFT(parent) == n)
                RB_LEFT(parent) = child;
            else
                RB_RIGHT(parent) = child;
            fix(parent);
        }
        else
            t->root = child;
        if (RB_PARENT(n) == old)
            parent = n;
        RB_PARENT(n) = RB_PARENT(old);
        RB_LEFT(n)   = RB_LEFT(old);
        RB_RIGHT(n)  = RB_RIGHT(old);
        RB_COLOR(n)  = RB_COLOR(old);
        if (RB_PARENT(old) != nullptr)
        {
            if (RB_LEFT(RB_PARENT(old)) == old)
                RB_LEFT(RB_PARENT(old)) = n;
            else
                RB_RIGHT(RB_PARENT(old)) = n;
            fix(RB_PARENT(old));
        }
        else
            t->root = n;
        RB_PARENT(RB_LEFT(old)) = n;
        if (RB_RIGHT(old) != nullptr)
            RB_PARENT(RB_RIGHT(old)) = n;
        if (parent)
        {
            left = parent;
            do
            {
                fix(left);
            }
            while ((left = RB_PARENT(left)) != nullptr);
        }
        goto color;
    }
    parent = RB_PARENT(n);
    color = RB_COLOR(n);
    if (child != nullptr)
        RB_PARENT(child) = parent;
    if (parent)
    {
        if (RB_LEFT(parent) == n)
            RB_LEFT(parent) = child;
        else
            RB_RIGHT(parent) = child;
        n = parent;
        do
        {
            fix(n);
        }
        while ((n = RB_PARENT(n)) != nullptr);
    }
    else
        t->root = child;
color:
    if (color == RB_BLACK)
        rebalanceRemove(t, parent, child);
    return old;
}

template <class T, class L>
static Node<T, L> *insert(Tree<T, L> *t, Node<T, L> *k)
{
    Node<T, L> *n = t->root, *parent = nullptr;
    int cmp = 0;
    while (n != nullptr)
    {
        parent = n;
        cmp = n->compare(*k);
        if (cmp <= 0)
            n = RB_LEFT(n);
        else
            n = RB_RIGHT(n);
    }
    n = k;
    RB_PARENT(n) = parent;
    if (parent != nullptr)
    {
        if (cmp <= 0)
            RB_LEFT(parent) = n;
        else
            RB_RIGHT(parent) = n;
    }
    else
        t->root = n;
    rebalanceInsert(t, n);
    return n;
}

template <class T, class L>
static Node<T, L> *search(const Tree<T, L> *t, const Node<T, L> *k)
{
    Node<T, L> *n = t->root;
    while (n != nullptr)
    {
        int cmp = n->compare(*k);
        if (cmp == 0)
            return n;
        else if (cmp < 0)
            n = RB_LEFT(n);
        else
            n = RB_RIGHT(n);
    }
    return nullptr;
}

template <class T, class L>
static void destroy(Node<T, L> *n)
{
    if (n == nullptr)
        return;
    destroy(RB_LEFT(n));
    destroy(RB_RIGHT(n));
    delete n;
}

template <class T, class L>
static void dump(FILE *stream, Node<T, L> *n)
{
    if (n == nullptr)
        return;
    dump(stream, RB_LEFT(n));
    fprintf(stream, "\t<%u,%u>..<%u,%u>:\n", PAIR(n->obj.lb()), PAIR(n->obj.ub()));
    dump(stream, RB_RIGHT(n));
}

template <class L>
static bool intersects(L lb1, L ub1, L lb2, L ub2)
{
    L lb = std::max(lb1, lb2);
    L ub = std::min(ub1, ub2);
    return (lb < ub);
}

template <class L>
static bool contains(L lb1, L ub1, L lb2, L ub2)
{
    return (lb1 >= lb2 && ub1 <= ub2);
}

template <class T, class L>
Node<T, L> *find(Node<T, L> *n, L lb, L ub)
{
    if (n == nullptr)
        return nullptr;
    if (!intersects(lb, ub, n->lb, n->ub))
    {
//        fprintf(stderr, "\t%sPRUNE  <%u,%u>..<%u,%u>%s:\n", RED, PAIR(n->lb), PAIR(n->ub), OFF);
        return nullptr;
    }
//    fprintf(stderr, "\tSEARCH <%u,%u>..<%u,%u>:\n", PAIR(n->lb), PAIR(n->ub));
    Node<T, L> *r = nullptr;
    r = (r == nullptr? find(RB_LEFT(n), lb, ub): r);
    r = (r == nullptr && intersects(lb, ub, n->obj.lb(), n->obj.ub())? n: r);
    r = (r == nullptr? find(RB_RIGHT(n), lb, ub): r);
    return r;
}

template <class T, class L>
typename Tree<T, L>::iterator Tree<T, L>::scan(L lb, L ub) const
{
    if (ub < lb)
        return Tree<T, L>::iterator();
//    dump(stderr);
//    fprintf(stderr, "\t%sSEARCH <%u,%u>..<%u,%u>%s:\n", CYAN, PAIR(lb), PAIR(ub), OFF);
    Node<T, L> *r = interval::find(root, lb, ub);
//    if (r != nullptr)
//        fprintf(stderr, "%sscan%s(<%u,%u>..<%u,%u>) = <%u,%u>..<%u,%u>\n\n",
//            YELLOW, OFF, PAIR(lb), PAIR(ub), PAIR(r->obj.lb()), PAIR(r->obj.ub()));
//    else
//        fprintf(stderr, "nothing found\n");
    return Tree<T, L>::iterator(r, lb, ub);
}

template <class T, class L>
typename Tree<T, L>::iterator Tree<T, L>::begin(void) const
{
    Node<T, L> *n = root, *r = nullptr;
    while (n != nullptr)
    {
        r = n;
        n = RB_LEFT(n);
    }
    return Tree<T, L>::iterator(r);
}

template <class T, class L>
static Node<T, L> *next(Node<T, L> *n, L lb, L ub)
{
	if (n == nullptr)
        return n;
    Node<T, L> *r = find(RB_RIGHT(n), lb, ub);
    if (r != nullptr)
        return r;
    while (true)
    {
        Node<T, L> *parent = RB_PARENT(n);
        if (parent == nullptr)
            return nullptr;
        if (RB_LEFT(parent) == n)
        {
            if (intersects(lb, ub, parent->obj.lb(), parent->obj.ub()))
                return parent;
            r = find(RB_RIGHT(parent), lb, ub);
            if (r != nullptr)
                return r;
        }
        n = parent;
    }
}

template <class T, class L>
void Tree<T, L>::iterator::operator++(void)
{
    node = next(node, lb, ub);
}

}   // Namespace interval

#endif //AUTOBUG_SYMBOLIC_ITREE_H
