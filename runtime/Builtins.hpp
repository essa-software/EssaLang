#include <algorithm>
#include <bits/ranges_algobase.h>
#include <fmt/format.h>
#include <runtime/UString.hpp>

template<class... Args>
void print(fmt::format_string<Args...>&& fmtstr, Args&&... args) { fmt::print(std::move(fmtstr), std::forward<Args>(args)...); }

namespace ___Esl {

class RangeIterator {
public:
    explicit RangeIterator(uint64_t value)
        : m_value(value) { }

    auto operator==(RangeIterator const& other) const { return m_value == other.m_value; }
    auto operator*() const { return m_value; }
    auto operator++() { return ++m_value; }

private:
    uint64_t m_value;
};

class Range {
public:
    Range(uint64_t begin, uint64_t end)
        : m_begin(begin)
        , m_end(end) { }

    auto begin() const { return RangeIterator { m_begin }; }
    auto end() const { return RangeIterator { m_end }; }

private:
    uint64_t m_begin;
    uint64_t m_end;
};

struct EmptyArray {
    template<class T, size_t S>
    operator std::array<T, S>() const {
        std::array<T, S> a;
        std::ranges::fill(a, T());
        return a;
    }
};

}

template<>
struct fmt::formatter<___Esl::Range> : public fmt::formatter<std::string_view> {
    template<typename FormatContext>
    constexpr auto format(___Esl::Range const& p, FormatContext& ctx) const {
        fmt::format_to(ctx.out(), "{}..{}", *p.begin(), *p.end());
        return ctx.out();
    }
};
