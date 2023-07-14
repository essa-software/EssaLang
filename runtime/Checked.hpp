#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <limits>

namespace ___Esl {

template<std::integral T>
inline T checked_cast(std::integral auto in) {
    using Lim = std::numeric_limits<T>;
    if (in > Lim::max()) {
        panic("Integer overflow: {} is greater than target type maximum {}", in, Lim::max());
    }
    if (in < Lim::min()) {
        panic("Integer overflow: {} is less than target type minimum {}", in, Lim::min());
    }
    return static_cast<T>(in);
}

constexpr bool greater(auto l, auto r) { return l > r; }
template<class L, class R>
using Bigger =  std::conditional_t<greater(sizeof(L), sizeof(R)), L, R>;

template<std::integral L, std::integral R>
inline Bigger<L, R> checked_add(L lhs, R rhs) {
    decltype(lhs) result;
    if (__builtin_add_overflow(lhs, rhs, &result)) {
        panic("Integer overflow when calculating {} + {}", lhs, rhs);
    }
    return result;
}
template<std::integral L, std::integral R>
inline Bigger<L, R> checked_sub(L lhs, R rhs) {
    decltype(lhs) result;
    if (__builtin_sub_overflow(lhs, rhs, &result)) {
        panic("Integer overflow when calculating {} - {}", lhs, rhs);
    }
    return result;
}
template<std::integral L, std::integral R>
inline Bigger<L, R> checked_mul(L lhs, R rhs) {
    decltype(lhs) result;
    if (__builtin_mul_overflow(lhs, rhs, &result)) {
        panic("Integer overflow when calculating {} * {}", lhs, rhs);
    }
    return result;
}
template<std::integral L, std::integral R>
inline Bigger<L, R> checked_div(L lhs, R rhs) {
    if (rhs == 0) {
        panic("Integer division by 0");
    }
    return lhs / rhs;
}
template<std::integral L, std::integral R>
inline Bigger<L, R> checked_mod(L lhs, R rhs) {
    if (rhs == 0) {
        panic("Integer modulo by 0");
    }
    return lhs % rhs;
}

}
