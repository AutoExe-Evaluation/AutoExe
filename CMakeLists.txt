cmake_minimum_required(VERSION 3.16)
project(AutoBug_symbolic)
include(FetchContent)
include(FindPkgConfig)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()
set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -Wno-unused-parameter -Wno-missing-field-initializers -Wno-unused-function -Wno-sign-compare -Wno-reorder-ctor -Wno-invalid-utf8 -Wno-unused-variable -Wno-unused-but-set-variable")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set(CMAKE_CXX_FLAGS_RELEASE "-O2")

find_package(CURL CONFIG REQUIRED)
find_package(nlohmann_json CONFIG REQUIRED)
find_package(Z3 CONFIG REQUIRED)
find_package(argparse CONFIG REQUIRED)

add_subdirectory(liboai)

add_custom_target(
    parser
    COMMAND make
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/parsers
)
add_custom_target(
    tree-sitter
    COMMAND make
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/parsers/lib/src
)

add_executable(autobug AutoBug-main.cpp AutoBug.cpp c-parser.cpp AST.cpp common.cpp)
target_include_directories(autobug PRIVATE ${CMAKE_SOURCE_DIR}/parsers/lib/include)
target_link_libraries(autobug PRIVATE
    ${CMAKE_SOURCE_DIR}/parsers/lib/src/libtree-sitter.a
    ${CMAKE_SOURCE_DIR}/parsers/c-parser.o
)
add_dependencies(autobug parser tree-sitter)

add_executable(executor main.cpp simple-executor.cpp constraint-solver.cpp interpreter.cpp AutoBug.cpp c-parser.cpp coverage_map.cpp AST.cpp common.cpp)
target_include_directories(executor PRIVATE
    ${CMAKE_SOURCE_DIR}/parsers/lib/include
    oai
)
target_link_libraries(executor PRIVATE
    ${CMAKE_SOURCE_DIR}/parsers/lib/src/libtree-sitter.a
    ${CMAKE_SOURCE_DIR}/parsers/c-parser.o
    z3::libz3
    CURL::libcurl
    nlohmann_json::nlohmann_json
    oai
    argparse::argparse
)
add_dependencies(executor parser tree-sitter)

add_executable(executor-py main.cpp simple-executor.cpp constraint-solver.cpp interpreter.cpp AutoBug.cpp python-parser.cpp coverage_map.cpp AST.cpp common.cpp)
target_compile_options(executor-py PRIVATE -DPYTHON_PARSER)
target_include_directories(executor-py PRIVATE
    ${CMAKE_SOURCE_DIR}/parsers/lib/include
    oai
)
target_link_libraries(executor-py PRIVATE
    ${CMAKE_SOURCE_DIR}/parsers/lib/src/libtree-sitter.a
    ${CMAKE_SOURCE_DIR}/parsers/python-parser.o
    ${CMAKE_SOURCE_DIR}/parsers/python-scanner.o
    z3::libz3
    CURL::libcurl
    nlohmann_json::nlohmann_json
    oai
    argparse::argparse
)
add_dependencies(executor-py parser tree-sitter)

add_executable(executor-java main.cpp simple-executor.cpp constraint-solver.cpp interpreter.cpp AutoBug.cpp java-parser.cpp coverage_map.cpp AST.cpp common.cpp)
target_compile_options(executor-java PRIVATE -DJAVA_PARSER)
target_include_directories(executor-java PRIVATE
    ${CMAKE_SOURCE_DIR}/parsers/lib/include
    oai
)
target_link_libraries(executor-java PRIVATE
    ${CMAKE_SOURCE_DIR}/parsers/lib/src/libtree-sitter.a
    ${CMAKE_SOURCE_DIR}/parsers/java-parser.o
    z3::libz3
    CURL::libcurl
    nlohmann_json::nlohmann_json
    oai
    argparse::argparse
)
add_dependencies(executor-java parser tree-sitter)
