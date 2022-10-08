# cppfront-exp

Copyright (c) Thomas Neumann

cppfront-exp is an even more experimental front end for the experimental [cppfront](https://github.com/hsutter/cppfront) compiler by Herb Sutter.

This is only an experiment to understand the proposal better and to try out potential suggestions for upstream cppfront. Do not use for anything serious.

## Deviations from cppfront

While upstream uses an ad-hoc hand-written parser, cppfront-exp uses bison with a [context free grammar](src/parser/cpp2.ypp). The good thing is
that this guarantees that the cppfront-exp grammar is unambiguous (as bison will complain otherwise), the bad thing is that, unfortunately,
the upstream grammar is currently ambiguous. For now we fix this by deviating from the upstream grammar, in the long run the upstream
grammar will hopefully be unambiguous, too. The deviations are discussed below.

### Template syntax

By far the most serious deviation is on the syntax for template expressions and types. The problem there is that C++ (and implicitly cppfront)
is contact sensitive, it is impossible to parse ```{ a<b,c>d; }``` without knowing wheter ```a``` is a template or not. There is no easy solution to this problem
if we want parse without type analysis. One option would be to forbid the comma operator and use an LR(inf) grammar, but that is also unattractive.
For now, we solve this problem by using ```a![T]``` as template syntax, i.e.:

```
vector<int> -> vector![int]
map<int,vector<string>> -> map![int,vector![string]]
```

That solves the ambiguity problem, at the cost of a new syntax for templates.

