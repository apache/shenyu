/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.ai.token.limiter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.function.Predicate;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

/**
 * Streaming gzip decoder for handling cross-buffer gzip decompression.
 * Package-visible for testing.
 */
class GzipStreamDecoder {

    private static final Logger LOG = LoggerFactory.getLogger(GzipStreamDecoder.class);

    private final Inflater inflater = new Inflater(true);

    private final byte[] decompressBuffer = new byte[4096];

    private final GzipHeaderState headerState = new GzipHeaderState();

    private boolean abandoned;

    private boolean closed;

    /**
     * Decode a chunk of gzip data, collecting the whole expanded chunk.
     * Only for callers that can afford the full expansion, such as tests; the
     * response observation path must use {@link #decode(byte[], Predicate)} so
     * the inspection budget bounds the decompression work itself.
     * @param inBytes compressed input bytes
     * @return decompressed bytes, or empty array if header incomplete or abandoned
     */
    byte[] decode(final byte[] inBytes) {
        ByteArrayOutputStream collected = new ByteArrayOutputStream();
        decode(inBytes, slice -> {
            collected.writeBytes(slice);
            return true;
        });
        return collected.toByteArray();
    }

    /**
     * Decode a chunk of gzip data, pushing each decompressed slice straight into
     * the sink without ever materializing the whole expanded chunk. When the sink
     * reports exhaustion (returns {@code false}) inflation stops immediately and
     * the decoder abandons further input, so a highly compressible chunk cannot
     * allocate or inflate beyond the observer's budget. The compressed input is
     * only observed here; the forwarded response bytes are untouched.
     * @param inBytes compressed input bytes
     * @param sink receives a copy of each decompressed slice; returns whether observation should continue
     */
    void decode(final byte[] inBytes, final Predicate<byte[]> sink) {
        if (abandoned || closed) {
            return;
        }

        int offset = 0;
        if (!headerState.isComplete()) {
            offset = headerState.process(inBytes);
            if (headerState.isCapacityExceeded()) {
                abandoned = true;
                return;
            }
            if (!headerState.isComplete()) {
                return;
            }
        }

        inflater.setInput(inBytes, offset, inBytes.length - offset);
        try {
            int cnt;
            while (!inflater.needsInput() && (cnt = inflater.inflate(decompressBuffer)) > 0) {
                if (!sink.test(Arrays.copyOf(decompressBuffer, cnt))) {
                    // The observer budget is exhausted; leave the remaining input
                    // un-inflated instead of expanding it into a discarded buffer.
                    abandoned = true;
                    return;
                }
            }
        } catch (DataFormatException ex) {
            LOG.error("Inflater decompression failed", ex);
            abandoned = true;
        }
    }

    /**
     * Release the inflater. Idempotent so success, error and cancellation paths
     * can all close the decoder without a double end.
     */
    void close() {
        if (closed) {
            return;
        }
        closed = true;
        inflater.end();
    }

    /**
     * Track gzip header parsing state across buffers.
     */
    static class GzipHeaderState {

        private static final int MAX_HEADER_SIZE = 10 + 256;

        private final byte[] accumulatedHeader = new byte[MAX_HEADER_SIZE];

        private int accumulatedLength;

        private boolean complete;

        private boolean capacityExceeded;

        boolean isComplete() {
            return complete;
        }

        boolean isCapacityExceeded() {
            return capacityExceeded;
        }

        /**
         * Process gzip header bytes, potentially spanning multiple buffers.
         *
         * @param inBytes input bytes
         * @return offset where compressed data starts (0 if header is still incomplete)
         */
        int process(final byte[] inBytes) {
            if (complete || capacityExceeded) {
                return 0;
            }

            final int prev = accumulatedLength;
            final int toCopy = Math.min(inBytes.length, accumulatedHeader.length - prev);
            System.arraycopy(inBytes, 0, accumulatedHeader, prev, toCopy);
            accumulatedLength += toCopy;

            if (accumulatedLength < 10) {
                return 0;
            }

            try {
                int pos = 10;
                int flg = accumulatedHeader[3] & 0xFF;

                if ((flg & 0x04) != 0) {
                    if (accumulatedLength < pos + 2) {
                        return headerIncomplete();
                    }
                    int xlen = (accumulatedHeader[pos] & 0xFF) | ((accumulatedHeader[pos + 1] & 0xFF) << 8);
                    pos += 2 + xlen;
                    if (accumulatedLength < pos) {
                        return headerIncomplete();
                    }
                }

                if ((flg & 0x08) != 0) {
                    while (pos < accumulatedLength && accumulatedHeader[pos] != 0) {
                        pos++;
                    }
                    if (pos >= accumulatedLength) {
                        return headerIncomplete();
                    }
                    pos++;
                }

                if ((flg & 0x10) != 0) {
                    while (pos < accumulatedLength && accumulatedHeader[pos] != 0) {
                        pos++;
                    }
                    if (pos >= accumulatedLength) {
                        return headerIncomplete();
                    }
                    pos++;
                }

                if ((flg & 0x02) != 0) {
                    if (accumulatedLength < pos + 2) {
                        return headerIncomplete();
                    }
                    pos += 2;
                }

                complete = true;
                return pos - prev;

            } catch (ArrayIndexOutOfBoundsException e) {
                // Defensive: the bounds checks above should make this unreachable.
                // Abandon decompression instead of letting the error propagate into the
                // reactive pipeline, where it would abort the response for the client.
                capacityExceeded = true;
                LOG.warn("Unexpected gzip header parse error, decompression abandoned", e);
                return 0;
            }
        }

        private int headerIncomplete() {
            if (accumulatedLength >= accumulatedHeader.length) {
                capacityExceeded = true;
                LOG.warn("Gzip header exceeds maximum size of {} bytes, decompression abandoned. "
                        + "This may occur with long FNAME or FCOMMENT fields.", MAX_HEADER_SIZE);
            }
            return 0;
        }
    }
}
