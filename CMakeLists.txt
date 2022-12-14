cmake_minimum_required (VERSION 3.20)
set(CMAKE_CXX_STANDARD 20)
project (cppfront-exp)

enable_testing()

include_directories(${cppfront-exp_SOURCE_DIR}/src)

add_executable(packcc thirdparty/packcc/packcc.c)

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/cpp2parser.cpp ${CMAKE_CURRENT_BINARY_DIR}/cpp2parser.h
                   COMMAND packcc -l -o ${CMAKE_CURRENT_BINARY_DIR}/cpp2parser ${CMAKE_CURRENT_SOURCE_DIR}/src/parser/cpp2.peg
                   DEPENDS packcc src/parser/cpp2.peg VERBATIM)

add_executable(cppfront-exp
	src/cppfront.cpp
	src/codegen/CppOut.cpp
	src/infra/Hash.cpp
	src/parser/AST.cpp
	src/parser/Parser.cpp
	src/parser/Range.cpp
	src/parser/Source.cpp
	src/program/Declaration.cpp
	src/program/Expression.cpp
	src/program/FunctionType.cpp
	src/program/Module.cpp
	src/program/Namespace.cpp
	src/program/Program.cpp
	src/program/Statement.cpp
	src/program/Type.cpp
	src/semana/Scope.cpp
	src/semana/SemanticAnalysis.cpp
	src/stdlib/Stdlib.cpp
	${CMAKE_CURRENT_BINARY_DIR}/cpp2parser.cpp
)

target_compile_options(cppfront-exp PRIVATE -Werror -Wall -Wextra)

file(GLOB testfiles "${CMAKE_CURRENT_SOURCE_DIR}/tests/*.cpp2")
foreach(filename ${testfiles})
    cmake_path(GET filename FILENAME n)
    add_test(NAME "test_${n}" COMMAND cppfront-exp --test "-I${CMAKE_CURRENT_SOURCE_DIR}/include" ${filename})
endforeach()
