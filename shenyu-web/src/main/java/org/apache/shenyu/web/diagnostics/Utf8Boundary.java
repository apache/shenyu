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

package org.apache.shenyu.web.diagnostics;

/**
 * Byte-limit arithmetic for UTF-8 section content.
 *
 * <p>A size cap is a byte count, but a diagnostic section is text. Cutting at an arbitrary byte
 * offset can split a multi-byte character and leave an entry that no longer decodes as UTF-8,
 * which defeats the purpose of a bundle meant to be read. Every cap in the diagnostics package -
 * the per-section cap while collecting and again while archiving - therefore goes through
 * {@link #limit(byte[], int)}, which only ever returns a prefix that ends on a character
 * boundary.</p>
 */
final class Utf8Boundary {

    private static final int MAX_CONTINUATION_BYTES = 3;

    private static final int CONTINUATION_MASK = 0xC0;

    private static final int CONTINUATION_BITS = 0x80;

    private Utf8Boundary() {
    }

    /**
     * Lower a byte limit to the nearest character boundary at or below it.
     *
     * @param content  the UTF-8 content
     * @param maxBytes the requested byte limit
     * @return a limit no greater than {@code maxBytes} that never splits a sequence, possibly 0
     */
    static int limit(final byte[] content, final int maxBytes) {
        if (maxBytes >= content.length) {
            return content.length;
        }
        if (maxBytes <= 0) {
            return 0;
        }
        int index = maxBytes;
        int continuation = 0;
        while (index > 0 && continuation < MAX_CONTINUATION_BYTES
                && (content[index - 1] & CONTINUATION_MASK) == CONTINUATION_BITS) {
            index--;
            continuation++;
        }
        if (index == 0) {
            return 0;
        }
        // index now sits just past a lead byte. Keep the limit only when the whole sequence that
        // lead byte starts fits; otherwise the limit has to move back before the lead byte.
        int end = index - 1 + sequenceLength(content[index - 1] & 0xFF);
        return end <= maxBytes ? maxBytes : index - 1;
    }

    private static int sequenceLength(final int lead) {
        if ((lead & 0xE0) == 0xC0) {
            return 2;
        }
        if ((lead & 0xF0) == 0xE0) {
            return 3;
        }
        if ((lead & 0xF8) == 0xF0) {
            return 4;
        }
        return 1;
    }
}
