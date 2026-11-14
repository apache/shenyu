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

package org.apache.shenyu.plugin.mcp.server.contract;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.StreamReadFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * RFC 8785 (JSON Canonicalization Scheme) canonicalizer for the #7935 regression fixtures: a test utility, not production code.
 *
 * <p>The input is parsed with Jackson (duplicate member names are rejected) and the canonical form is written here, not by a
 * Jackson serializer: strings are escaped as RFC 8785 section 3.2.2.2 prescribes, numbers are written as the ECMA-262
 * Number::toString form of their IEEE 754 double value (section 3.2.2.3), and object members are sorted by their UTF-16 code
 * units (section 3.2.3). Every JSON number is taken as a double, so an integer that a double cannot hold is rounded the way the
 * RFC requires: 9007199254740993 (2^53 + 1) is written as 9007199254740992. A lone surrogate is rejected (section 3.2.2.2).</p>
 *
 * <p>The shortest-form search by BigDecimal rounding matches every measured case (the Appendix B rows and these fixtures), but it
 * is not proven for every double; the tests therefore compare against the expected literals in the fixtures, not against this
 * writer.</p>
 */
public final class Rfc8785TestCanonicalizer {

    private static final ObjectMapper MAPPER = new ObjectMapper(JsonFactory.builder()
            .enable(StreamReadFeature.STRICT_DUPLICATE_DETECTION).build());

    private static final int MAX_SIGNIFICANT_DIGITS = 17;

    private static final int MAX_FIXED_EXPONENT = 21;

    private static final int MIN_FIXED_EXPONENT = -6;

    private static final int FIRST_UNESCAPED_CHAR = 0x20;

    private static final int UNICODE_ESCAPE_WIDTH = 4;

    private static final int HEX_RADIX = 16;

    private Rfc8785TestCanonicalizer() {
    }

    /**
     * Canonicalizes one JSON document.
     *
     * @param json the JSON text, UTF-8
     * @return the RFC 8785 canonical bytes, UTF-8
     * @throws IOException if the text is not JSON or repeats a member name
     */
    public static byte[] canonicalize(final byte[] json) throws IOException {
        final StringBuilder out = new StringBuilder();
        write(MAPPER.readTree(json), out);
        return out.toString().getBytes(StandardCharsets.UTF_8);
    }

    private static void write(final JsonNode node, final StringBuilder out) {
        if (node.isObject()) {
            writeObject(node, out);
        } else if (node.isArray()) {
            writeArray(node, out);
        } else if (node.isTextual()) {
            writeString(node.textValue(), out);
        } else if (node.isNumber()) {
            out.append(formatNumber(node.doubleValue()));
        } else {
            out.append(node.asText());
        }
    }

    private static void writeObject(final JsonNode node, final StringBuilder out) {
        final List<String> names = new ArrayList<>();
        final Iterator<String> it = node.fieldNames();
        it.forEachRemaining(names::add);
        // String.compareTo compares UTF-16 code units, which is the RFC 8785 section 3.2.3 order
        names.sort(String::compareTo);
        out.append('{');
        for (int i = 0; i < names.size(); i++) {
            if (i > 0) {
                out.append(',');
            }
            writeString(names.get(i), out);
            out.append(':');
            write(node.get(names.get(i)), out);
        }
        out.append('}');
    }

    private static void writeArray(final JsonNode node, final StringBuilder out) {
        out.append('[');
        for (int i = 0; i < node.size(); i++) {
            if (i > 0) {
                out.append(',');
            }
            write(node.get(i), out);
        }
        out.append(']');
    }

    private static void writeString(final String value, final StringBuilder out) {
        out.append('"');
        for (int i = 0; i < value.length(); i++) {
            final char c = value.charAt(i);
            checkSurrogate(value, i);
            out.append(escape(c));
        }
        out.append('"');
    }

    private static void checkSurrogate(final String value, final int index) {
        final char c = value.charAt(index);
        final boolean pairedHigh = Character.isHighSurrogate(c) && index + 1 < value.length()
                && Character.isLowSurrogate(value.charAt(index + 1));
        final boolean pairedLow = Character.isLowSurrogate(c) && index > 0 && Character.isHighSurrogate(value.charAt(index - 1));
        if (Character.isSurrogate(c) && !pairedHigh && !pairedLow) {
            throw new IllegalArgumentException("RFC 8785 section 3.2.2.2: a lone surrogate has no canonical form");
        }
    }

    private static String escape(final char c) {
        switch (c) {
            case '"':
                return "\\\"";
            case '\\':
                return "\\\\";
            case '\b':
                return "\\b";
            case '\t':
                return "\\t";
            case '\n':
                return "\\n";
            case '\f':
                return "\\f";
            case '\r':
                return "\\r";
            default:
                return c < FIRST_UNESCAPED_CHAR ? unicodeEscape(c) : String.valueOf(c);
        }
    }

    private static String unicodeEscape(final char c) {
        final StringBuilder hex = new StringBuilder(Integer.toString(c, HEX_RADIX));
        while (hex.length() < UNICODE_ESCAPE_WIDTH) {
            hex.insert(0, '0');
        }
        return "\\u" + hex;
    }

    /**
     * Writes a double as ECMA-262 Number::toString does (RFC 8785 section 3.2.2.3).
     *
     * @param value the IEEE 754 double
     * @return the canonical number literal
     */
    public static String formatNumber(final double value) {
        if (Double.isNaN(value) || Double.isInfinite(value)) {
            throw new IllegalArgumentException("RFC 8785 section 3.2.2.3: NaN and Infinity have no canonical form");
        }
        if (value == 0) {
            return "0";
        }
        if (value < 0) {
            return "-" + formatNumber(-value);
        }
        final BigDecimal shortest = shortestRoundTrip(value);
        final String digits = shortest.unscaledValue().toString();
        return layout(digits, digits.length() - shortest.scale());
    }

    /**
     * The fewest significant digits that read back as the same double, nearest to the exact value (ECMA-262 7.1.12.1).
     */
    private static BigDecimal shortestRoundTrip(final double value) {
        final BigDecimal exact = new BigDecimal(value);
        for (int precision = 1; precision < MAX_SIGNIFICANT_DIGITS; precision++) {
            final BigDecimal candidate = exact.round(new MathContext(precision, RoundingMode.HALF_EVEN));
            if (Double.parseDouble(candidate.toString()) == value) {
                return candidate.stripTrailingZeros();
            }
        }
        return exact.round(new MathContext(MAX_SIGNIFICANT_DIGITS, RoundingMode.HALF_EVEN)).stripTrailingZeros();
    }

    /**
     * Lays out k significant digits with decimal exponent n (value = digits x 10^(n - k)) as ECMA-262 7.1.12.1 steps 6-10.
     */
    private static String layout(final String digits, final int n) {
        final int k = digits.length();
        if (k <= n && n <= MAX_FIXED_EXPONENT) {
            return digits + "0".repeat(n - k);
        }
        if (0 < n && n <= MAX_FIXED_EXPONENT) {
            return digits.substring(0, n) + "." + digits.substring(n);
        }
        if (MIN_FIXED_EXPONENT < n && n <= 0) {
            return "0." + "0".repeat(-n) + digits;
        }
        final int exponent = n - 1;
        final String mantissa = k == 1 ? digits : digits.charAt(0) + "." + digits.substring(1);
        return mantissa + "e" + (exponent < 0 ? "-" : "+") + Math.abs(exponent);
    }
}
