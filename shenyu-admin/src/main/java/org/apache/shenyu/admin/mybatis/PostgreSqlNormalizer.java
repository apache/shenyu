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

package org.apache.shenyu.admin.mybatis;

import java.util.Locale;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * SQL case and identifier normalization for PostgreSQL-compatible mappers.
 */
public final class PostgreSqlNormalizer {

    private static final String IDENTIFIER_START = "A-Za-z_\\P{ASCII}";

    private static final String IDENTIFIER_PART = IDENTIFIER_START + "0-9";

    private static final String IDENTIFIER_BOUNDARY = "(?<![" + IDENTIFIER_PART + "$])";

    private static final Pattern TOKEN_START = Pattern.compile(
            IDENTIFIER_BOUNDARY + "(?<escape>[Ee]')|"
                    + IDENTIFIER_BOUNDARY + "(?<dollar>\\$(?:[" + IDENTIFIER_START + "][" + IDENTIFIER_PART + "]*)?\\$)|['\"`]|--|/\\*");

    private static final Pattern BLOCK_COMMENT_DELIMITER = Pattern.compile("/\\*|\\*/");

    private static final Pattern ESCAPED_STRING_CONTINUATION = Pattern.compile("[ \\t\\f\\x0B]*+(?:--[^\\r\\n]*+)?[\\r\\n](?:\\s++|--[^\\r\\n]*+[\\r\\n])*+'");

    private PostgreSqlNormalizer() {
    }

    /**
     * Normalize a mapper statement while preserving single- and dollar-quoted literals.
     * Ordinary single-quoted literals follow standard-conforming string syntax.
     *
     * @param sql original statement
     * @return normalized statement
     */
    public static String normalize(final String sql) {
        Matcher token = TOKEN_START.matcher(sql);
        StringBuilder result = new StringBuilder(sql.length());
        int normalizedFrom = 0;
        int position = 0;
        while (token.find(position)) {
            switch (token.group()) {
                case "--":
                    position = lineCommentEnd(sql, token.end());
                    break;
                case "/*":
                    position = blockCommentEnd(sql, token.end());
                    break;
                case "\"":
                case "`":
                    position = quotedEnd(sql, token.end(), token.group().charAt(0), false);
                    break;
                default:
                    position = appendLiteral(sql, token, normalizedFrom, result);
                    normalizedFrom = position;
            }
        }
        return result.append(normalizeCode(sql, normalizedFrom, sql.length())).toString();
    }

    private static int appendLiteral(final String sql, final Matcher token, final int normalizedFrom, final StringBuilder result) {
        boolean escape = Objects.nonNull(token.group("escape"));
        int start = escape ? token.end() - 1 : token.start();
        String dollarQuote = token.group("dollar");
        int end = Objects.isNull(dollarQuote) ? quotedEnd(sql, token.end(), '\'', escape) : dollarQuotedEnd(sql, token.end(), dollarQuote);
        result.append(normalizeCode(sql, normalizedFrom, start));
        result.append(sql, start, end);
        if (!escape) {
            return end;
        }
        // A newline can continue an escape string without another E prefix.
        Matcher continuation = ESCAPED_STRING_CONTINUATION.matcher(sql);
        while (continuation.region(end, sql.length()).lookingAt()) {
            int quote = continuation.end() - 1;
            result.append(normalizeCode(sql, end, quote));
            end = quotedEnd(sql, continuation.end(), '\'', true);
            result.append(sql, quote, end);
        }
        return end;
    }

    private static String normalizeCode(final String sql, final int start, final int end) {
        return sql.substring(start, end).replace('`', '"').toLowerCase(Locale.ROOT);
    }

    private static int quotedEnd(final String sql, final int start, final char quote, final boolean escapeBackslashes) {
        int position = start;
        while (position < sql.length()) {
            char character = sql.charAt(position++);
            if (escapeBackslashes && character == '\\') {
                position++;
            } else if (character == quote) {
                if (position == sql.length() || sql.charAt(position) != quote) {
                    return position;
                }
                position++;
            }
        }
        return sql.length();
    }

    private static int dollarQuotedEnd(final String sql, final int start, final String quote) {
        int closing = sql.indexOf(quote, start);
        return closing < 0 ? sql.length() : closing + quote.length();
    }

    private static int lineCommentEnd(final String sql, final int start) {
        int position = start;
        while (position < sql.length() && sql.charAt(position) != '\n' && sql.charAt(position) != '\r') {
            position++;
        }
        return position;
    }

    private static int blockCommentEnd(final String sql, final int start) {
        Matcher delimiter = BLOCK_COMMENT_DELIMITER.matcher(sql).region(start, sql.length());
        int depth = 1;
        while (delimiter.find()) {
            depth += "/*".equals(delimiter.group()) ? 1 : -1;
            if (depth == 0) {
                return delimiter.end();
            }
        }
        return sql.length();
    }
}
