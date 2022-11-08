#ifndef H_cpp2util
#define H_cpp2util
//---------------------------------------------------------------------------
// cppfront-exp
// (c) 2022 Thomas Neumann
// SPDX-License-Identifier: MIT
//---------------------------------------------------------------------------
// Simulate import std;
#include <iostream>
#include <new>
#include <vector>
//---------------------------------------------------------------------------
namespace cpp2 {
//---------------------------------------------------------------------------
/// Calling convention for in parameters
template <typename T>
using in = std::conditional_t<sizeof(T) <= 2 * sizeof(void*) && std::is_trivially_copy_constructible_v<T>, const T, const T&>;
//---------------------------------------------------------------------------
/// Wrapper class for deferred init
template <typename T>
class deferred_init {
    private:
    /// The real value
    alignas(T) std::byte data[sizeof(T)];
    /// Did we initialize already?
    bool init = false;

    /// Raw access to the value
    T& raw() noexcept { return *std::launder(reinterpret_cast<T*>(data)); }

    template <typename U>
    friend class out;

    /// Forbid copying and moving the wrapper. We can copy/move the value itself
    deferred_init(const deferred_init&) = delete;
    deferred_init(deferred_init&&) = delete;
    void operator=(const deferred_init&) = delete;
    void operator=(deferred_init&&) = delete;

    public:
    /// Constructor for lazy initialization
    deferred_init() noexcept = default;
    /// Destructor
    ~deferred_init() noexcept {
        if (init) raw().T::~T();
    }

    /// Value access
    T& value() noexcept {
        // assert(init); // This is enforced by the cppfront compiler
        return raw();
    }

    /// Late initialization
    void construct(auto&&... args) {
        // assert(!init); // This is enforced by the cppfront compiler
        new (data) T(std::forward<decltype(args)>(args)...);
        init = true;
    }
};
//---------------------------------------------------------------------------
/// Calling convention for out parameters
template <typename T>
class out {
    private:
    /// The target
    union {
        /// A real variable
        T* var;
        /// A deferred variable
        deferred_init<T>* defvar;
    } target;
    /// The target mode
    bool hasVar;

    public:
    /// Construct from a target variable
    out(T* v) noexcept : hasVar(true) { target.var = v; }
    /// Construct from a target deferred variable
    out(deferred_init<T>* dv) noexcept : hasVar(false) { target.defvar = dv; }

    // Access the value itself
    T& value() { return hasVar ? *target.var : target.defvar->value(); }

    /// Construction
    void construct(auto&&... args) {
        if (hasVar || target.defvar->init) {
            value() = T(std::forward<decltype(args)>(args)...);
        } else {
            target.defvar->construct(std::forward<decltype(args)>(args)...);
        }
    }
};
//---------------------------------------------------------------------------
}
//---------------------------------------------------------------------------
#endif
