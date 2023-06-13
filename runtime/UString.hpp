#pragma once

#include "Error.hpp"
#include <compare>
#include <cstdint>
#include <optional>
#include <ostream>
#include <span>
#include <string>
#include <type_traits>
#include <vector>

namespace Util {

class Buffer;

// A string that properly supports Unicode. It stores data as UTF-32
// internally, like SFML strings. This string is NOT null-terminated
// (because why is should be, you have size and this is sufficient
// in MODERN languages like C++ :^))
// It is immutable like in JS or Python.
class UString {
public:
    UString() = default;
    ~UString();
    UString(UString const& other);
    UString& operator=(UString const& other);
    UString(UString&& other);
    UString& operator=(UString&& other);

    static UString take_ownership(std::span<uint32_t const>);

    explicit UString(uint32_t codepoint);
    explicit UString(std::span<uint32_t const>);

    explicit UString(uint8_t ch)
        : UString(static_cast<uint32_t>(ch)) { }

    UString(char ch)
        : UString(static_cast<uint32_t>(ch)) { }

    UString(wchar_t ch)
        : UString(static_cast<uint32_t>(ch)) { }

    enum class Encoding { ASCII, Utf8 };

    UString(std::span<uint8_t const>, Encoding = Encoding::Utf8, uint32_t replacement = 0xfffd);
    UString(std::string_view, Encoding = Encoding::Utf8, uint32_t replacement = 0xfffd);
    enum DecodingErrorTag { DecodingError };
    static ErrorOr<UString, DecodingErrorTag> decode(std::span<uint8_t const>, Encoding = Encoding::Utf8);

    static auto decoding_error_to_os_error(UString::DecodingErrorTag) { return OsError { .error = 0, .function = "Decoding failed" }; }

    template<size_t S>
    UString(char const (&string)[S], Encoding encoding = Encoding::Utf8, uint32_t replacement = 0xfffd)
        : UString({ string, S - 1 }, encoding, replacement) { }

    template<class... Args> static UString format(fmt::format_string<Args...> fmtstr, Args&&... args) {
        return Util::UString(fmt::format(fmtstr, std::forward<Args>(args)...));
    }

    [[nodiscard]] std::string encode(Encoding = Encoding::Utf8) const;
    [[nodiscard]] Buffer encode_buffer(Encoding = Encoding::Utf8) const;

    [[nodiscard]] uint32_t at(size_t) const;
    [[nodiscard]] size_t size() const { return m_size; }
    [[nodiscard]] bool is_empty() const { return m_size == 0; }

    [[nodiscard]] auto begin() const { return span().begin(); }
    [[nodiscard]] auto end() const { return span().end(); }

    // If you really want... there is a footgun for you.
    [[nodiscard]] uint32_t const* storage() const { return m_storage; }

    [[nodiscard]] std::span<uint32_t const> span() const { return { m_storage, m_size }; }

    // Substring from `start` to end of string
    [[nodiscard]] UString substring(size_t start) const;

    [[nodiscard]] UString substring(size_t start, size_t size) const;
    [[nodiscard]] std::optional<size_t> find(UString const& needle, size_t start = 0) const;
    [[nodiscard]] std::optional<size_t> find_one_of(std::initializer_list<uint32_t>, size_t start = 0) const;
    [[nodiscard]] UString erase(size_t start, size_t size = 1) const;
    [[nodiscard]] UString insert(UString other, size_t where) const;
    [[nodiscard]] bool starts_with(UString other) const;

    [[nodiscard]] size_t indent() const;

    template<class Callback> void for_each_split(UString const& splitter, Callback&& callback) const {
        size_t index = 0;
        while (true) {
            auto next = find(splitter, index);
            if (!next.has_value()) {
                next = size();
            }
            if (index > size() - 1)
                break;
            callback({ m_storage + index, *next - index });
            if (next == size())
                break;
            index = *next + 1;
        }
    }

    template<class Callback> void for_each_line(Callback&& callback) const { for_each_split("\n", std::forward<Callback>(callback)); }

    std::strong_ordering operator<=>(UString const& other) const;
    bool operator==(UString const& other) const;

    friend std::ostream& operator<<(std::ostream& out, UString const& str) { return out << str.encode(); }

    template<std::integral I> OsErrorOr<I> parse(int base = 10) const;
    template<std::floating_point I> OsErrorOr<I> parse() const;

private:
    friend UString operator+(UString const& lhs, UString const& rhs);

    void reallocate(size_t);
    std::string dump() const;

    uint32_t* m_storage {};
    size_t m_size {};
};

template<typename T> UString to_ustring(const T& to_convert) { return UString { std::to_string(to_convert) }; }

// For some reason, there is no std::stou for that. :(
template<> OsErrorOr<unsigned int> UString::parse<unsigned int>(int base) const = delete;

}
