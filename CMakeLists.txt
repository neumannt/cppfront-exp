cmake_minimum_required (VERSION 3.1.1)
set(CMAKE_CXX_STANDARD 20)
project (cppfront-exp)

add_executable(cppfront-exp
	src/cppfront.cpp
	src/parser/Lexer.cpp
)

