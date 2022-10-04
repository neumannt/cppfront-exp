cmake_minimum_required (VERSION 3.1.1)
set(CMAKE_CXX_STANDARD 20)
project (cppfront-exp)

find_package(BISON)
BISON_TARGET(Cpp2Parser src/parser/cpp2.ypp ${CMAKE_CURRENT_BINARY_DIR}/cpp2parser.cpp)

include_directories(${cppfront-exp_SOURCE_DIR}/src)

add_executable(cppfront-exp
	src/cppfront.cpp
	src/parser/AST.cpp
	src/parser/Lexer.cpp
	src/parser/Parser.cpp
	${BISON_Cpp2Parser_OUTPUTS}
)

