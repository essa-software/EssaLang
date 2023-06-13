#pragma once

#include "Buffer.hpp"
#include <bit>
#include <cassert>
#include <optional>
#include <span>
#include <string>

namespace Util::Utf8 {

int bytes_required_to_store_codepoint(uint32_t codepoint);
std::optional<size_t> codepoint_count_if_valid(std::string_view string);

template<class Callback> static bool decode_to_callback(std::string_view string, uint32_t replacement, Callback callback) {
    bool error = false;
    for (size_t s = 0; s < string.size(); s++) {
        auto byte = std::bit_cast<uint8_t>(string[s]);
        uint32_t codepoint = 0;
        int additional_bytes_to_expect = 0;

        if (!(byte & 0x80)) {
            codepoint = byte & 0x7f;
        }
        else if (!(byte & 0x20)) {
            additional_bytes_to_expect = 1;
            codepoint = byte & 0b11111;
        }
        else if (!(byte & 0x10)) {
            additional_bytes_to_expect = 2;
            codepoint = byte & 0b1111;
        }
        else if (!(byte & 0x08)) {
            additional_bytes_to_expect = 3;
            codepoint = byte & 0b111;
        }
        else if (!(byte & 0x04)) {
            additional_bytes_to_expect = 4;
            codepoint = byte & 0b11;
        }
        else if (!(byte & 0x02)) {
            additional_bytes_to_expect = 5;
            codepoint = byte & 0b1;
        }
        else {
            error = true;
            codepoint = replacement;
        }

        for (int i = 0; i < additional_bytes_to_expect; i++) {
            s++;
            if (s >= string.size()) {
                // std::cout << "unfinished utf8 sequence" << std::endl;
                return false;
            }
            codepoint <<= 6;
            codepoint |= (string[s] & 0b111111);
        }

        // Check if codepoint was stored optimally
        auto required_bytes = bytes_required_to_store_codepoint(codepoint);
        auto got_bytes = additional_bytes_to_expect + 1;
        if (required_bytes > got_bytes) {
            // std::cout << "got more bytes than required to encode codepoint "
            // << codepoint << " (" << got_bytes << " > " << required_bytes <<
            // ")\n";
        }

        callback(codepoint);
    }
    return !error;
}

// Decode a UTF-8 string `string` to `storage`. This function
// asserts if storage is not big enough; codepoint_count_if_valid()
// should be used first to check how much space is needed.
bool decode(std::span<uint32_t> storage, std::string_view string, uint32_t replacement);

template<class Callback> void encode_to_callback(std::span<uint32_t const> codepoints, Callback callback) {
    for (auto codepoint : codepoints) {
        if (codepoint < 0x80) {
            callback(codepoint);
        }
        else if (codepoint < 0x800) {
            callback(0b1100'0000 | ((codepoint & 0b11111'000000) >> 6));
            callback(0b1000'0000 | ((codepoint & 0b111111)));
        }
        else if (codepoint < 0x10000) {
            callback(0b1110'0000 | ((codepoint & 0b1111'000000'000000) >> 12));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000) >> 6));
            callback(0b1000'0000 | ((codepoint & 0b111111)));
        }
        else if (codepoint < 0x200000) {
            callback(0b1111'0000 | ((codepoint & 0b1110'000000'000000'000000) >> 18));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000'000000) >> 12));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000) >> 6));
            callback(0b1000'0000 | ((codepoint & 0b111111)));
        }
        else if (codepoint < 0x4000000) {
            callback(0b1111'1000 | ((codepoint & 0b1100'000000'000000'000000'000000) >> 24));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000'000000'000000) >> 18));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000'000000) >> 12));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000) >> 6));
            callback(0b1000'0000 | ((codepoint & 0b111111)));
        }
        else {
            callback(0b1111'1100 | ((codepoint & 0b1000'000000'000000'000000'000000'000000) >> 30));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000'000000'000000'000000) >> 24));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000'000000'000000) >> 18));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000'000000) >> 12));
            callback(0b1000'0000 | ((codepoint & 0b111111'000000) >> 6));
            callback(0b1000'0000 | ((codepoint & 0b111111)));
        }
    }
}

// UTF-8-encode a span of `codepoints` to Buffer.
Buffer encode(std::span<uint32_t const> codepoints);

}
