#ifndef AUTOBUG_SYMBOLIC_C_PARSER_H
#define AUTOBUG_SYMBOLIC_C_PARSER_H

#include "AST.h"

// File utilities
const char *getBasename(const char *path);
bool suffix(const char *path, const char *ext);

// File parsing related functions
bool addIndex(const char *path);
void buildIndex(const char *path);
void resolvePath(const char *path, std::vector<const char *> &candidates);
const File *loadFile(const char *path, size_t max = 1);

#endif //AUTOBUG_SYMBOLIC_C_PARSER_H
