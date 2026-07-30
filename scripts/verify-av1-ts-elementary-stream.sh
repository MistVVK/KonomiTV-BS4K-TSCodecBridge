#!/usr/bin/env bash
# SPDX-License-Identifier: 0BSD

set -euo pipefail

if [[ "$#" -ne 1 ]]; then
    printf 'Usage: %s AV1_TS_ELEMENTARY_STREAM\n' "$0" >&2
    exit 2
fi

elementary_stream="$1"
test -s "${elementary_stream}"

LC_ALL=C od -An -v -tu1 -- "${elementary_stream}" |
    awk '
        function fail(message) {
            print "AV1 TS elementary stream verification failed: " message \
                > "/dev/stderr"
            exit 1
        }

        {
            for (field = 1; field <= NF; field++) {
                byte_count++
                byte[byte_count] = $field + 0
            }
        }

        END {
            if (byte_count < 4) {
                fail("stream is too short")
            }

            start_code_count = 0
            for (position = 1;
                 position + 2 <= byte_count;
                 position++) {
                if (byte[position] == 0 &&
                    byte[position + 1] == 0 &&
                    byte[position + 2] == 1) {
                    start_code_count++
                    start_code[start_code_count] = position
                    position += 2
                }
            }
            if (start_code_count == 0 || start_code[1] != 1) {
                fail("first OBU does not start with 0x000001")
            }

            sequence_header_count = 0
            frame_header_count = 0
            for (obu = 1; obu <= start_code_count; obu++) {
                payload_start = start_code[obu] + 3
                payload_end = (obu < start_code_count) \
                    ? start_code[obu + 1] - 1 \
                    : byte_count
                if (payload_start > payload_end) {
                    fail("empty tsOBU after start code " obu)
                }

                header = byte[payload_start]
                forbidden = int(header / 128)
                type = int(header / 8) % 16
                extension = int(header / 4) % 2
                has_size = int(header / 2) % 2
                reserved = header % 2
                if (forbidden != 0 || reserved != 0) {
                    fail("invalid OBU header at tsOBU " obu)
                }
                if (has_size != 0) {
                    fail("obu_has_size_field was not normalized at tsOBU " obu)
                }
                if (type == 0 || type == 2 || type == 8 ||
                    (type >= 9 && type <= 14)) {
                    fail("unsupported OBU type " type " at tsOBU " obu)
                }
                if (extension != 0) {
                    if (payload_start + 1 > payload_end) {
                        fail("truncated OBU extension at tsOBU " obu)
                    }
                    if (byte[payload_start + 1] % 8 != 0) {
                        fail("nonzero OBU extension reserved bits at tsOBU " obu)
                    }
                }

                if (type == 1) {
                    sequence_header_count++
                }
                if (type == 3 || type == 6) {
                    frame_header_count++
                }

                for (position = payload_start;
                     position + 2 <= payload_end;
                     position++) {
                    if (byte[position] == 0 &&
                        byte[position + 1] == 0) {
                        following = byte[position + 2]
                        if (following <= 2) {
                            fail("forbidden 0x00000" following \
                                 " sequence in tsOBU " obu)
                        }
                        if (following == 3 &&
                            (position + 3 > payload_end ||
                             byte[position + 3] > 3)) {
                            fail("invalid emulation prevention sequence in tsOBU " \
                                 obu)
                        }
                    }
                }
            }

            if (sequence_header_count == 0) {
                fail("sequence header OBU is absent")
            }
            if (frame_header_count == 0) {
                fail("frame OBU or frame header OBU is absent")
            }
            printf "Verified AV1 TS elementary stream: %d bytes, %d tsOBUs\n", \
                byte_count, start_code_count
        }
    '
