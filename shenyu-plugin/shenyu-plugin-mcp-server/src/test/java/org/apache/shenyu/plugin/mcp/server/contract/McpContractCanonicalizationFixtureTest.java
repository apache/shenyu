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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * RFC 8785 canonicalization regression fixtures for issue #7935 (tools/list descriptors).
 *
 * <p>Each before/after pair differs in raw bytes only by number spelling, string escaping or member order, or, in the negative
 * control, by a real schema change. The expected canonical bytes are fixture files written from the RFC 8785 literals, not output
 * of the canonicalizer under test. Canonical equality is not schema-semantic equivalence; see provenance.json.</p>
 */
class McpContractCanonicalizationFixtureTest {

    private static final Path FIXTURE_ROOT = Paths.get("src/test/resources/mcp-7935");

    private static final String EXPECTED_FILE = "expected/canonical-equivalence.json";

    private static final String SIDECAR_SUFFIX = ".sha256";

    private static final String BEFORE = "before";

    private static final String AFTER = "after";

    private static final String CANONICAL = "canonical";

    private static final String KEY_ORDER_DESCRIPTOR_BEFORE = "rfc8785/key-order/ascii-descriptor.before.json";

    private static final String REQUIRED_ADDED_BEFORE = "rfc8785/negative-control/required-added.before.json";

    private static final String TWO_POW_53_PLUS_ONE = "rfc8785/numbers/two-pow-53-plus-one.before.json";

    private static final String PINNED_TWO_POW_53 = "\"minimum\":9007199254740992";

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void testEveryFixtureCanonicalizesToItsExpectedBytes() throws IOException {
        for (final JsonNode pair : pairs()) {
            assertArrayEquals(read(pair.get(CANONICAL).asText()), canonical(pair.get(BEFORE).asText()), pair.get(BEFORE).asText());
            assertArrayEquals(read(afterCanonicalOf(pair)), canonical(pair.get(AFTER).asText()), pair.get(AFTER).asText());
        }
    }

    @Test
    void testEverySidecarHoldsTheSha256OfTheCanonicalBytes() throws IOException {
        for (final JsonNode pair : pairs()) {
            for (final String side : List.of(BEFORE, AFTER)) {
                final String file = pair.get(side).asText();
                assertEquals(sidecar(file), sha256Hex(canonical(file)), file);
            }
            final String canonicalFile = pair.get(CANONICAL).asText();
            assertEquals(sidecar(canonicalFile), sha256Hex(read(canonicalFile)), canonicalFile);
        }
    }

    @Test
    void testPairEqualityMatchesTheExpectedFile() throws IOException {
        for (final JsonNode pair : pairs()) {
            final String before = pair.get(BEFORE).asText();
            final String after = pair.get(AFTER).asText();
            assertEquals(pair.get("raw_equal").asBoolean(), Arrays.equals(read(before), read(after)), before);
            assertEquals(pair.get("canonical_equal").asBoolean(), Arrays.equals(canonical(before), canonical(after)), before);
            assertFalse(pair.get("semantic_equivalence_proof").asBoolean(), before);
        }
    }

    @Test
    void testAsciiDescriptorKeyOrderIsNoCanonicalDescriptorDrift() throws IOException {
        final JsonNode pair = pairOf(KEY_ORDER_DESCRIPTOR_BEFORE);
        final String after = pair.get(AFTER).asText();
        assertFalse(Arrays.equals(read(KEY_ORDER_DESCRIPTOR_BEFORE), read(after)));
        assertArrayEquals(canonical(KEY_ORDER_DESCRIPTOR_BEFORE), canonical(after));
    }

    @Test
    void testNewRequiredFieldChangesTheCanonicalBytes() throws IOException {
        final JsonNode pair = pairOf(REQUIRED_ADDED_BEFORE);
        assertFalse(pair.get("canonical_equal").asBoolean());
        assertFalse(Arrays.equals(canonical(REQUIRED_ADDED_BEFORE), canonical(pair.get(AFTER).asText())));
    }

    @Test
    void testTwoPowFiftyThreePlusOneIsWrittenAsTheDoubleItRoundsTo() throws IOException {
        final String written = new String(canonical(TWO_POW_53_PLUS_ONE), StandardCharsets.UTF_8);
        assertTrue(written.contains(PINNED_TWO_POW_53), written);
    }

    @Test
    void testExpectedFileStatesThatCanonicalEqualityIsNotSemanticEquivalence() throws IOException {
        final String statement = MAPPER.readTree(read(EXPECTED_FILE)).get("statement").asText();
        assertTrue(statement.contains("not a proof of schema-semantic equivalence"), statement);
    }

    private static List<JsonNode> pairs() throws IOException {
        final List<JsonNode> list = new ArrayList<>();
        MAPPER.readTree(read(EXPECTED_FILE)).get("pairs").forEach(list::add);
        assertFalse(list.isEmpty());
        return list;
    }

    private static JsonNode pairOf(final String before) throws IOException {
        for (final JsonNode pair : pairs()) {
            if (before.equals(pair.get(BEFORE).asText())) {
                return pair;
            }
        }
        throw new IllegalStateException("no fixture pair for " + before);
    }

    private static String afterCanonicalOf(final JsonNode pair) {
        return pair.has("after_canonical") ? pair.get("after_canonical").asText() : pair.get(CANONICAL).asText();
    }

    private static byte[] canonical(final String file) throws IOException {
        return Rfc8785TestCanonicalizer.canonicalize(read(file));
    }

    private static byte[] read(final String file) throws IOException {
        return Files.readAllBytes(FIXTURE_ROOT.resolve(file));
    }

    private static String sidecar(final String file) throws IOException {
        return new String(read(file + SIDECAR_SUFFIX), StandardCharsets.US_ASCII).trim();
    }

    private static String sha256Hex(final byte[] data) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(data));
        } catch (final NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }
}
