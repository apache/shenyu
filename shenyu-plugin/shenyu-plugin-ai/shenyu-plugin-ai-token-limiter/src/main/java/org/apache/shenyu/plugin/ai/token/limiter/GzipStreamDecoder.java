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

import java.util.Arrays;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

/**
 * Streaming gzip decoder for handling cross-buffer gzip decompression.
 * Package-visible for testing.
 *
 * <p>Decoding is sink-driven and bounded: inflated output is handed to the observer
 * in chunks of at most {@link #CHUNK_SIZE} bytes as it is produced, never accumulated
 * into a whole-chunk expansion first. The observer returns {@code false} once its
 * inspection budget is exhausted, and the decoder then stops inflating for the rest
 * of the response instead of spending work on output nobody will read. Response
 * forwarding is unaffected: observation only reads a copy of the bytes.</p>
 */
class GzipStreamDecoder {

    private static final Logger LOG = LoggerFactory.getLogger(GzipStreamDecoder.class);

    /**
     * Upper bound of one inflated chunk handed to the sink; also the only temporary
     * output buffer, so a highly compressible input cannot materialize its full
     * expansion before the inspection budget is checked.
     */
    private static final int CHUNK_SIZE = 4096;

    private final Inflater inflater = new Inflater(true);

    private final byte[] decompressBuffer = new byte[CHUNK_SIZE];

    private final GzipHeaderState headerState = new GzipHeaderState();

    private boolean abandoned;

    private boolean closed;

    /**
     * Decode a chunk of gzip data, feeding bounded inflated chunks to the sink.
     *
     * @param inBytes compressed input bytes
     * @param sink the observer of inflated chunks; may stop the observation
     */
    void decode(final byte[] inBytes, final ChunkSink sink) {
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
                byte[] chunk = cnt == decompressBuffer.length ? decompressBuffer : Arrays.copyOf(decompressBuffer, cnt);
                if (!sink.accept(chunk)) {
                    // the inspection budget is exhausted; stop inflating for the rest
                    // of the response instead of expanding output nobody will read
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
     * Release the inflater exactly once across success, error and cancellation paths.
     */
    void close() {
        if (!closed) {
            closed = true;
            inflater.end();
        }
    }

    /**
     * Observer of inflated chunks. Returning {@code false} stops the observation
     * permanently, e.g. when the usage-inspection budget is exhausted.
     */
    @FunctionalInterface
    interface ChunkSink {

        /**
         * Consume one inflated chunk.
         *
         * @param chunk the inflated bytes, owned by the sink for the call's duration
         * @return true to keep observing, false to stop the observation
         */
        boolean accept(byte[] chunk);
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
