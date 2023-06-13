#include "UString.hpp"

#include "Buffer.hpp"
#include "Utf8.hpp"

#include <algorithm>
#include <bit>
#include <cassert>
#include <compare>
#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace Util {

UString::UString(UString const& other) {
    // std::cout << __PRETTY_FUNCTION__ << std::endl;
    reallocate(other.m_size);
    std::copy(other.m_storage, other.m_storage + other.m_size, m_storage);
}

UString::UString(std::span<uint32_t const> codepoints) {
    reallocate(codepoints.size());
    std::copy(codepoints.begin(), codepoints.end(), m_storage);
}

UString::~UString() {
    // std::cout << __PRETTY_FUNCTION__ << ": " << dump() << std::endl;
    delete[] m_storage;
}

UString& UString::operator=(UString const& other) {
    // std::cout << __PRETTY_FUNCTION__ << std::endl;
    if (this == &other)
        return *this;
    reallocate(other.m_size);
    std::copy(other.m_storage, other.m_storage + other.m_size, m_storage);
    return *this;
}

UString::UString(UString&& other) {
    // std::cout << __PRETTY_FUNCTION__ << ": " << dump() << " << " << other.dump() << std::endl;
    m_size = std::exchange(other.m_size, 0);
    m_storage = std::exchange(other.m_storage, nullptr);
}

UString& UString::operator=(UString&& other) {
    // std::cout << __PRETTY_FUNCTION__ << ": " << dump() << " << " << other.dump() << std::endl;
    if (this == &other)
        return *this;
    delete[] m_storage;
    m_size = std::exchange(other.m_size, 0);
    m_storage = std::exchange(other.m_storage, nullptr);
    return *this;
}

UString UString::take_ownership(std::span<uint32_t const> codepoints) {
    UString str;
    // NOTE: This assumes that the storage isn't modified by any of
    //       the methods. We can't enforce m_storage being const
    //       because it is written by constructors (e.g uint32 or UTF-8).
    // FIXME: Find a way to enforce m_storage being const to avoid this
    //        const_cast.
    str.m_storage = const_cast<uint32_t*>(codepoints.data());
    str.m_size = codepoints.size();
    return str;
}

UString::UString(uint32_t codepoint) {
    // std::cout << __PRETTY_FUNCTION__ << std::endl;
    reallocate(1);
    m_storage[0] = codepoint;
}

UString::UString(std::string_view string, Encoding encoding, uint32_t replacement) {
    switch (encoding) {
    case Encoding::ASCII:
        reallocate(string.size());
        std::copy(string.begin(), string.end(), m_storage);
        break;
    case Encoding::Utf8: {
        auto size = Utf8::codepoint_count_if_valid(string);
        if (!size) {
            size = 0;
        }
        reallocate(*size);
        Utf8::decode({ m_storage, m_size }, string, replacement);
        break;
    }
    }
    // std::cout << __PRETTY_FUNCTION__ << ": " << dump() << std::endl;
}

UString::UString(std::span<uint8_t const> data, Encoding encoding, uint32_t replacement)
    : UString { std::string_view { reinterpret_cast<char const*>(data.data()), data.size() }, encoding, replacement } { }

ErrorOr<UString, UString::DecodingErrorTag> UString::decode(std::span<uint8_t const> data, Encoding) {
    std::string_view data_sv { reinterpret_cast<char const*>(data.data()), data.size() };
    UString string;
    auto size = Utf8::codepoint_count_if_valid(data_sv);
    if (!size) {
        return UString::DecodingError;
    }
    string.reallocate(*size);
    [[maybe_unused]] bool r = Utf8::decode({ string.m_storage, string.m_size }, data_sv, 0xfffd);
    assert(r);
    return string;
}

Buffer UString::encode_buffer(Encoding encoding) const {
    switch (encoding) {
    case Encoding::ASCII: {
        Buffer str;
        for (auto cp : span()) {
            if (cp > 0x7f)
                continue;
            str.append(cp);
        }
        return str;
    }
    case Encoding::Utf8: {
        return Utf8::encode(span());
    }
    }
    return Buffer();
}

std::string UString::encode(Encoding e) const {
    Buffer encoded = encode_buffer(e);
    return std::string { reinterpret_cast<char const*>(encoded.span().data()), encoded.size() };
}

uint32_t UString::at(size_t p) const {
    assert(p < m_size);
    return m_storage[p];
}

UString UString::substring(size_t start) const { return substring(start, size() - start); }

UString UString::substring(size_t start, size_t size) const {
    assert(start + size <= m_size);
    UString result;
    result.reallocate(size);
    if (m_storage)
        std::copy(m_storage + start, m_storage + start + size, result.m_storage);
    return result;
}

std::optional<size_t> UString::find(UString const& needle, size_t start) const {
    assert(start <= m_size);
    for (size_t s = start; s < m_size; s++) {
        if (m_storage[s] == needle.at(0)) {
            bool found = true;
            for (size_t t = s; t - s < needle.m_size; t++) {
                if (t == m_size) {
                    found = false;
                    break;
                }
                if (m_storage[t] != needle.at(t - s)) {
                    found = false;
                    break;
                }
            }
            if (found) {
                return s;
            }
        }
    }
    return {};
}

std::optional<size_t> UString::find_one_of(std::initializer_list<uint32_t> codepoints, size_t start) const {
    assert(start < m_size);
    for (size_t s = start; s < m_size; s++) {
        if (std::any_of(codepoints.begin(), codepoints.end(), [&](uint32_t c) { return m_storage[s] == c; })) {
            return s;
        }
    }
    return {};
}

UString UString::erase(size_t start, size_t size) const {
    if (start + size > m_size) {
        size = m_size - start;
    }
    // std::cout << "erase " << start << " +" << size << " from US[" << m_size << "]" << std::endl;
    UString result;
    result.reallocate(m_size - size);
    if (m_storage) {
        std::copy(m_storage, m_storage + start, result.m_storage);
        std::copy(m_storage + start + size, m_storage + m_size, result.m_storage + start);
    }
    return result;
}

UString UString::insert(UString other, size_t where) const {
    // std::cout << "insert US[" << other.m_size << "] at " << where << " into US[" << m_size << "]" << std::endl;
    assert(where <= m_size);
    UString result;
    result.reallocate(m_size + other.m_size);
    if (m_storage) {
        std::copy(m_storage, m_storage + where, result.m_storage);
        std::copy(m_storage + where, m_storage + m_size, result.m_storage + where + other.m_size);
    }
    if (other.m_storage) {
        std::copy(other.m_storage, other.m_storage + other.m_size, result.m_storage + where);
    }
    return result;
}

bool UString::starts_with(UString other) const {
    if (other.size() > size()) {
        return false;
    }
    for (size_t s = 0; s < other.size(); s++) {
        if (m_storage[s] != other.m_storage[s]) {
            return false;
        }
    }
    return true;
}

[[nodiscard]] size_t UString::indent() const {
    if (is_empty())
        return 0;
    for (size_t s = 0; s < size(); s++) {
        if (!isspace(m_storage[s])) {
            return s;
        }
    }
    return size();
}

void UString::reallocate(size_t size) {
    auto old_storage = m_storage;
    if (size > 0) {
        auto old_size = m_size;

        m_storage = new uint32_t[size];
        if (old_storage) {
            std::copy(old_storage, old_storage + std::min(size, old_size), m_storage);
        }
    }
    else {
        m_storage = nullptr;
    }
    delete[] old_storage;
    m_size = size;
    // std::cout << __PRETTY_FUNCTION__ << " with size = " << size << " result = " << dump() << std::endl;
}

std::string UString::dump() const {
    std::ostringstream oss;
    oss << "US[" << m_storage << " +" << m_size << "] ";
    for (auto cp : *this) {
        oss << "U+" << std::hex << std::setfill('0') << std::setw(4) << cp << std::dec << " ";
    }
    oss << " (encoded to ";
    auto encoded = Utf8::encode(span());
    for (auto ch : encoded) {
        oss << std::hex << ((uint16_t)ch & 0xff) << std::dec << " ";
    }
    oss << encode() << ")";
    return oss.str();
}

template<class T> using StoiFunction = T(const std::string& __str, size_t* __idx, int __base);

template<class T> using StofFunction = T(const std::string& __str, size_t* __idx);

template<class T> OsErrorOr<T> parse_impl(StoiFunction<T>&& stot, Util::UString const& str, int base) {
    try {
        return stot(str.encode(), nullptr, base);
    } catch (...) {
        return OsError { .error = 0, .function = "Failed to parse int" };
    }
}

template<class T> OsErrorOr<T> parse_impl(StofFunction<T>&& stot, Util::UString const& str) {
    try {
        return stot(str.encode(), nullptr);
    } catch (...) {
        return OsError { .error = 0, .function = "Failed to parse int" };
    }
}

template<> OsErrorOr<int> UString::parse<int>(int base) const { return parse_impl(std::stoi, *this, base); }
template<> OsErrorOr<long> UString::parse<long>(int base) const { return parse_impl(std::stol, *this, base); }
template<> OsErrorOr<long long> UString::parse<long long>(int base) const { return parse_impl(std::stoll, *this, base); }
template<> OsErrorOr<unsigned long> UString::parse<unsigned long>(int base) const { return parse_impl(std::stoul, *this, base); }
template<> OsErrorOr<unsigned long long> UString::parse<unsigned long long>(int base) const { return parse_impl(std::stoull, *this, base); }
template<> OsErrorOr<float> UString::parse<float>() const { return parse_impl(std::stof, *this); }
template<> OsErrorOr<double> UString::parse<double>() const { return parse_impl(std::stod, *this); }
template<> OsErrorOr<long double> UString::parse<long double>() const { return parse_impl(std::stold, *this); }

std::strong_ordering UString::operator<=>(UString const& other) const {
    return std::lexicographical_compare_three_way(m_storage, m_storage + m_size, other.m_storage, other.m_storage + other.m_size);
}

bool UString::operator==(UString const& other) const { return (*this <=> other) == std::strong_ordering::equal; }

UString operator+(UString const& lhs, UString const& rhs) {
    UString result;
    result.reallocate(lhs.m_size + rhs.m_size);
    std::copy(lhs.m_storage, lhs.m_storage + lhs.m_size, result.m_storage);
    std::copy(rhs.m_storage, rhs.m_storage + rhs.m_size, result.m_storage + lhs.m_size);
    return result;
}

}
