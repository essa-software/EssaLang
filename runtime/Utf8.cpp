#include "Utf8.hpp"

namespace Util::Utf8 {

int bytes_required_to_store_codepoint(uint32_t codepoint) {
    if (codepoint < 0x80)
        return 1;
    if (codepoint < 0x800)
        return 2;
    if (codepoint < 0x10000)
        return 3;
    if (codepoint < 0x200000)
        return 4;
    if (codepoint < 0x4000000)
        return 5;
    return 6;
}

std::optional<size_t> codepoint_count_if_valid(std::string_view string) {
    size_t size = 0;
    if (!decode_to_callback(string, 0x0, [&size](auto) { size++; })) {
        return {};
    }
    return size;
}

bool decode(std::span<uint32_t> storage, std::string_view string, uint32_t replacement) {
    size_t offset = 0;
    return decode_to_callback(string, replacement, [&offset, &storage](auto cp) {
        assert(offset < storage.size());
        storage[offset] = cp;
        offset++;
    });
}

Buffer encode(std::span<uint32_t const> codepoints) {
    Buffer result;
    // Some random heuristic to make less allocations
    result.ensure_capacity(codepoints.size() * 1.1);
    encode_to_callback(codepoints, [&](uint8_t byte) { result.append(byte); });
    return result;
}

}
