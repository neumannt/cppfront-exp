cmake_minimum_required (VERSION 3.1.1)
set(CMAKE_CXX_STANDARD 20)
project (cppfront-exp)

include_directories(${cppfront-exp_SOURCE_DIR}/src)

add_executable(packcc thirdparty/packcc/packcc.c)

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/cpp2parser.cpp ${CMAKE_CURRENT_BINARY_DIR}/cpp2parser.h
                   COMMAND packcc -l -o ${CMAKE_CURRENT_BINARY_DIR}/cpp2parser ${CMAKE_CURRENT_SOURCE_DIR}/src/parser/cpp2.peg
                   DEPENDS packcc src/parser/cpp2.peg VERBATIM)

add_executable(cppfront-exp
	src/cppfront.cpp
	src/infra/Hash.cpp
	src/parser/AST.cpp
	src/parser/Parser.cpp
	src/parser/Range.cpp
	src/parser/Source.cpp
	src/program/Declaration.cpp
	src/program/FunctionType.cpp
	src/program/Module.cpp
	src/program/Namespace.cpp
	src/program/Program.cpp
	src/program/Type.cpp
	src/semana/SemanticAnalysis.cpp
	${CMAKE_CURRENT_BINARY_DIR}/cpp2parser.cpp
)
