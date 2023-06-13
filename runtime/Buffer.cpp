#include "Buffer.hpp"
#include <algorithm>
#include <fmt/core.h>
#include <unistd.h>
#include <utility>

namespace Util {

Buffer::~Buffer() { delete[] m_data; }

Buffer::Buffer(Buffer const& other) {
    resize_uninitialized(other.m_size);
    std::copy(other.m_data, other.m_data + other.m_size, m_data);
}

Buffer& Buffer::operator=(Buffer const& other) {
    if (this == &other)
        return *this;
    resize_uninitialized(other.m_size);
    std::copy(other.m_data, other.m_data + other.m_size, m_data);
    return *this;
}

Buffer::Buffer(Buffer&& other) {
    delete[] m_data;
    m_data = std::exchange(other.m_data, nullptr);
    m_size = std::exchange(other.m_size, 0);
}

Buffer& Buffer::operator=(Buffer&& other) {
    if (this == &other)
        return *this;
    delete[] m_data;
    m_data = std::exchange(other.m_data, nullptr);
    m_size = std::exchange(other.m_size, 0);
    return *this;
}

Buffer::Buffer(std::span<uint8_t const> data) {
    resize_uninitialized(data.size());
    std::copy(data.begin(), data.end(), m_data);
}

Buffer::Buffer(std::initializer_list<uint8_t> data) {
    resize_uninitialized(data.size());
    std::copy(data.begin(), data.end(), m_data);
}

Buffer Buffer::uninitialized(size_t size) { return Buffer(size); }

Buffer Buffer::filled(size_t size, uint8_t byte) {
    Buffer buffer(size);
    std::fill(buffer.m_data, buffer.m_data + buffer.m_size, byte);
    return buffer;
}

void Buffer::clear() { resize_uninitialized(0); }

void Buffer::append(uint8_t byte) {
    if (m_size == 0) {
        ensure_capacity(1);
    }
    else if (m_size + 1 > m_capacity) {
        ensure_capacity(m_capacity * 2);
    }
    assert(m_capacity >= m_size + 1);
    m_size++;
    m_data[m_size - 1] = byte;
}

void Buffer::append(std::span<uint8_t const> data) {
    resize_uninitialized(m_size + data.size());
    std::copy(data.begin(), data.end(), &m_data[m_size - data.size()]);
}

UString Buffer::decode_infallible(UString::Encoding encoding, uint32_t replacement) const {
    return UString { std::string_view { reinterpret_cast<char const*>(m_data), m_size }, encoding, replacement };
}

ErrorOr<UString, UString::DecodingErrorTag> Buffer::decode(UString::Encoding encoding) const { return UString::decode(span(), encoding); }

void Buffer::insert(size_t position, uint8_t byte) { insert(position, { &byte, 1 }); }

void Buffer::insert(size_t position, std::span<uint8_t const> data) {
    // FIXME: This makes an unnecessary copy.
    resize_uninitialized(m_size + data.size());
    std::copy(begin() + position, end() - data.size(), begin() + position + data.size());
    std::copy(data.begin(), data.end(), begin() + position);
}

void Buffer::take_from_back(size_t s) { resize_uninitialized(m_size - s); }

void Buffer::resize_uninitialized(size_t size) {
    reallocate(size);
    m_size = size;
}

void Buffer::reallocate(size_t capacity) {

    auto old_storage = m_data;
    if (capacity > 0) {
        auto old_capacity = m_capacity;

        m_data = new uint8_t[capacity];
        if (old_storage) {
            std::copy(old_storage, old_storage + std::min(capacity, old_capacity), m_data);
        }
    }
    else {
        m_data = nullptr;
    }
    delete[] old_storage;
    m_capacity = capacity;
}

void Buffer::ensure_capacity(size_t capacity) {
    if (capacity <= m_capacity) {
        return;
    }
    reallocate(capacity);
}

bool Buffer::operator==(Buffer const& other) const {
    if (m_size != other.m_size)
        return false;
    return std::equal(begin(), end(), other.begin());
}

}
