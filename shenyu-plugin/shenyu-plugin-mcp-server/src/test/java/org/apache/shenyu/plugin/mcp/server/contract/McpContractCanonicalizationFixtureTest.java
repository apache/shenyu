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
import java.util.HashSet;
import java.util.HexFormat;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * RFC 8785 canonicalization fixtures for MCP tools/list descriptors.
 *
 * <p>Each before/after pair differs in raw bytes only by number spelling, string escaping or member order, or, in the negative
 * control, by a real schema change. Both sides must canonicalize to the expected canonical file, which is written from the RFC 8785
 * literals rather than produced by the canonicalizer under test, and each canonical file has one SHA-256 sidecar. Canonical
 * equality is not schema-semantic equivalence; see provenance.json.</p>
 */
class McpContractCanonicalizationFixtureTest {

    private static final Path FIXTURE_ROOT = Paths.get("src/test/resources/mcp/canonicalization");

    private static final String EXPECTED_FILE = "expected/canonical-equivalence.json";

    private static final String SIDECAR_SUFFIX = ".sha256";

    private static final String BEFORE = "before";

    private static final String AFTER = "after";

    private static final String CANONICAL = "canonical";

    private static final String AFTER_CANONICAL = "after_canonical";

    private static final String CANONICAL_SHA256 = "canonical_sha256";

    private static final String AFTER_CANONICAL_SHA256 = "after_canonical_sha256";

    private static final String TWO_POW_53_PLUS_ONE = "rfc8785/numbers/two-pow-53-plus-one.before.json";

    private static final String TWO_POW_53_PLUS_ONE_CANONICAL = "{\"tools\":[{\"description\":\"Synthetic fixture: only the number spelling differs.\","
            + "\"inputSchema\":{\"properties\":{\"amount\":{\"minimum\":9007199254740992,\"type\":\"number\"}},\"required\":[\"amount\"],"
            + "\"type\":\"object\"},\"name\":\"rfc8785_numbers_two_pow_53_plus_one\"}]}";

    private static final ObjectMapper MAPPER = new ObjectMapper();

    @Test
    void testEveryFixtureCanonicalizesToItsExpectedBytes() throws IOException {
        for (final JsonNode pair : pairs()) {
            final String before = pair.get(BEFORE).asText();
            final String after = pair.get(AFTER).asText();
            assertArrayEquals(read(pair.get(CANONICAL).asText()), canonical(before), before);
            assertArrayEquals(read(afterCanonicalOf(pair)), canonical(after), after);
            assertEquals(pair.get("raw_equal").asBoolean(), Arrays.equals(read(before), read(after)), before);
            assertEquals(pair.get("canonical_equal").asBoolean(), Arrays.equals(canonical(before), canonical(after)), before);
        }
    }

    @Test
    void testEverySidecarHoldsTheSha256OfItsCanonicalFile() throws IOException {
        for (final JsonNode pair : pairs()) {
            assertSidecar(pair.get(CANONICAL).asText(), pair.get(CANONICAL_SHA256).asText());
            if (pair.has(AFTER_CANONICAL)) {
                assertSidecar(pair.get(AFTER_CANONICAL).asText(), pair.get(AFTER_CANONICAL_SHA256).asText());
            }
        }
    }

    @Test
    void testEverySidecarUnderTheFixtureRootIsReferenced() throws IOException {
        final Set<String> referenced = new HashSet<>();
        for (final JsonNode pair : pairs()) {
            referenced.add(pair.get(CANONICAL_SHA256).asText());
            if (pair.has(AFTER_CANONICAL_SHA256)) {
                referenced.add(pair.get(AFTER_CANONICAL_SHA256).asText());
            }
        }
        final Set<String> onDisk;
        try (Stream<Path> files = Files.walk(FIXTURE_ROOT)) {
            onDisk = files.filter(Files::isRegularFile)
                    .map(file -> FIXTURE_ROOT.relativize(file).toString().replace('\\', '/'))
                    .filter(file -> file.endsWith(SIDECAR_SUFFIX))
                    .collect(Collectors.toSet());
        }
        assertFalse(onDisk.isEmpty());
        final Set<String> unreferenced = new HashSet<>(onDisk);
        unreferenced.removeAll(referenced);
        assertTrue(unreferenced.isEmpty(), "sidecars not referenced by " + EXPECTED_FILE + ": " + unreferenced);
        assertTrue(onDisk.containsAll(referenced), "referenced sidecars missing on disk");
    }

    @Test
    void testTwoPowFiftyThreePlusOneIsWrittenAsTheDoubleItRoundsTo() throws IOException {
        assertArrayEquals(TWO_POW_53_PLUS_ONE_CANONICAL.getBytes(StandardCharsets.UTF_8), canonical(TWO_POW_53_PLUS_ONE));
    }

    private static void assertSidecar(final String canonicalFile, final String sidecarFile) throws IOException {
        assertTrue(sidecarFile.endsWith(SIDECAR_SUFFIX), sidecarFile);
        assertEquals(new String(read(sidecarFile), StandardCharsets.US_ASCII).trim(), sha256Hex(read(canonicalFile)), sidecarFile);
    }

    private static List<JsonNode> pairs() throws IOException {
        final List<JsonNode> list = new ArrayList<>();
        MAPPER.readTree(read(EXPECTED_FILE)).get("pairs").forEach(list::add);
        assertFalse(list.isEmpty());
        return list;
    }

    private static String afterCanonicalOf(final JsonNode pair) {
        return pair.has(AFTER_CANONICAL) ? pair.get(AFTER_CANONICAL).asText() : pair.get(CANONICAL).asText();
    }

    private static byte[] canonical(final String file) throws IOException {
        return Rfc8785TestCanonicalizer.canonicalize(read(file));
    }

    private static byte[] read(final String file) throws IOException {
        return Files.readAllBytes(FIXTURE_ROOT.resolve(file));
    }

    private static String sha256Hex(final byte[] data) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(data));
        } catch (final NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }
}
