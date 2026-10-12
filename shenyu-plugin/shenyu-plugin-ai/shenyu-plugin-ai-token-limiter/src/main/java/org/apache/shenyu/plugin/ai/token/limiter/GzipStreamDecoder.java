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

    /**
     * Decode a chunk of gzip data.
     * @param inBytes compressed input bytes
     * @return decompressed bytes, or empty array if header incomplete or abandoned
     */
    byte[] decode(final byte[] inBytes) {
        if (abandoned) {
            return new byte[0];
        }

        int offset = 0;
        if (!headerState.isComplete()) {
            offset = headerState.process(inBytes);
            if (headerState.isCapacityExceeded()) {
                abandoned = true;
                return new byte[0];
            }
            if (!headerState.isComplete()) {
                return new byte[0];
            }
        }

        inflater.setInput(inBytes, offset, inBytes.length - offset);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            int cnt;
            while (!inflater.needsInput() && (cnt = inflater.inflate(decompressBuffer)) > 0) {
                baos.write(decompressBuffer, 0, cnt);
            }
        } catch (DataFormatException ex) {
            LOG.error("Inflater decompression failed", ex);
            abandoned = true;
            return new byte[0];
        }
        return baos.toByteArray();
    }

    void close() {
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
