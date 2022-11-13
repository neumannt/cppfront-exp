#include "stdlib/Stdlib.hpp"
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
namespace cpp2exp {
//---------------------------------------------------------------------------
// Note: In an ideal world this file would not exists. Ideally, we simply
// want to expose the existing C++1 standard library as-is in C++2. Doing so
// would be possible using, e.g., libclang.
// But using that would bring a very large dependency and a lot of complexity
// to this experiment. Thus, we instead mock the existing C++1 library in
// C++2 for now.
//---------------------------------------------------------------------------
const char* Stdlib::interface = R"stdlib(

namespace std {

using nullptr_t = decltype(nullptr);
using size_t = decltype(0uz);
using ssize_t = decltype(0z);

// A simple iostream emulation for the test programs

class endl_type {}
endl : endl_type;

class ostream {
   operator <<: (v:char) -> ostream& inout;
   operator <<: (v:short) -> ostream& inout;
   operator <<: (v:unsigned short) -> ostream& inout;
   operator <<: (v:int) -> ostream& inout;
   operator <<: (v:unsigned int) -> ostream& inout;
   operator <<: (v:long) -> ostream& inout;
   operator <<: (v:unsigned long) -> ostream& inout;
   operator <<: (v:long long) -> ostream& inout;
   operator <<: (v:unsigned long long) -> ostream& inout;
   operator <<: (v:double) -> ostream& inout;
   operator <<: (v:*const char) -> ostream& inout;
   operator <<: (v:endl_type) -> ostream& inout;
}

cout : ostream;
cerr : ostream;

// A simplified std::string

class string {
   using size_type = std::size_t;
   using difference_type = std::ssize_t;
   using pointer = *char;
   using const_pointer = *const char;

   npos: static const size_type; // = -1;

   string : ();
   string : (count:size_type, ch: char);
   string : (other:string, pos:size_type);
   string : (other:string, pos:size_type, count:size_type);
   string : (s:const_pointer, count:size_type);
   string : (s:const_pointer);
   string : (first:const_pointer, last:const_pointer);
   string : (other:string);
   string : (move other:string);
   string : (n:nullptr_t) = delete;

   operator =: (other:string) -> string& inout;
   operator =: (move other:string) -> string& inout;
   operator =: (other:const_pointer) -> string& inout;
   operator =: (ch:char) -> string& inout;
   operator =: (n:nullptr_t) -> string& = delete inout;

   assign: (count:size_type, ch: char) -> string& inout;
   assign: (other:string) -> string& inout;
   assign: (str:string, pos:size_type, count:size_type) inout;
   assign: (move other:string) -> string& inout;
   assign: (s:const_pointer, count:size_type) -> string& inout;
   assign: (s:const_pointer) -> string& inout;

   at: (pos:size_type) -> char& inout;
   at: (pos:size_type) -> char;

   operator[]: (pos:size_type) -> char& inout;
   operator[]: (pos:size_type) -> char;

   front: () -> char& inout;
   front: () -> char;

   back: () -> char& inout;
   back: () -> char;

   data: () -> pointer inout;
   data: () -> const_pointer;

   c_str: () -> pointer inout;
   c_str: () -> const_pointer;

   class iterator {
      operator !=: (other: iterator) -> bool;
      operator *: () -> char&;
      operator ++: () -> iterator& inout;
   }
   class const_iterator {
      operator !=: (other: iterator) -> bool;
      operator *: () -> char;
      operator ++: () -> iterator& inout;
   }

   begin: () -> iterator inout;
   begin: () -> const_iterator;
   cbegin: () -> const_iterator;
   end: () -> iterator inout;
   end: () -> const_iterator;
   cend: () -> const_iterator;

   empty: () -> bool;
   size: () -> size_type;
   length: () -> size_type;
   max_size: () -> size_type;
   reserve: (new_cap: size_type) inout;
   capacity: () -> size_type;
   shrink_to_fit: () inout;

   clear: () inout;
   // TODO insert
   erase: (index:size_type = 0, count:size_type = npos) -> string& inout;
   erase: (position:const_iterator) -> iterator inout;
   erase: (first:const_iterator, last:const_iterator) -> iterator inout;
   push_back: (ch:char) inout;
   pop_back: () inout;

   append: (count:size_type, ch:char) -> string& inout;
   append: (str:string) -> string& inout;
   append: (str:string, pos:size_type, count:size_type = npos) -> string& inout;
   append: (str:const_pointer) -> string& inout;
   append: (from:const_iterator, to:const_iterator) -> string& inout;
   operator +=: (str:string) -> string& inout;
   operator +=: (str:const_pointer) -> string& inout;
   operator +=: (ch:char) -> string& inout;
   // TODO compare, starts_with, ends_with, contains, replace
   substr: (pos:size_type = 0, count: size_type = npos) -> string;
   copy: (dest: pointer, count:size_type, pos:size_type = 0) -> size_type;
   resize: (count: size_type) inout;
   resize: (count: size_type, ch:char) inout;
   // TODO resize_and_overwrite, find, rfind, find_first_of, find_first_not_of, find_last_of, find_last_not_of
}

}

)stdlib";
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
