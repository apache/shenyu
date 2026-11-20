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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

/**
 * Canonical digest unit tests: no database, no Spring context, runs in the default surefire pass.
 * v2 excludes the phone number (PII) and carries an explicit hashVersion marker;
 * the legacy v1 digest keeps the historical field set for replay compatibility with rows stored before v2.
 * Vectors: src/test/resources/app-auth/canonical-request.json (v2) and canonical-request-v1.json (v1).
 */
class AppAuthLifecycleServiceImplCanonicalHashTest {

    private static String sha256Hex(final byte[] data) throws Exception {
        final MessageDigest digest = MessageDigest.getInstance("SHA-256");
        final byte[] hash = digest.digest(data);
        final StringBuilder sb = new StringBuilder(hash.length * 2);
        for (final byte b : hash) {
            sb.append(Character.forDigit((b >> 4) & 0xF, 16));
            sb.append(Character.forDigit(b & 0xF, 16));
        }
        return sb.toString();
    }

    private static AppAuthApplyRequestDTO request() {
        final AppAuthApplyRequestDTO dto = new AppAuthApplyRequestDTO();
        dto.setAppName("demo-app");
        dto.setNamespaceId("ns-demo");
        dto.setUserId("10001");
        // duplicate entry on purpose: the canonical map must de-duplicate and sort pathList
        dto.setPathList(List.of("/api/b", "/api/a", "/api/a"));
        return dto;
    }

    @Test
    void v2DigestMatchesSharedVectorFile() throws Exception {
        final byte[] vector = Files.readAllBytes(
                Paths.get("src/test/resources/app-auth/canonical-request.json"));
        assertEquals(sha256Hex(vector), AppAuthLifecycleServiceImpl.canonicalRequestHash(request()));
    }

    @Test
    void phoneIsExcludedFromV2Digest() {
        final AppAuthApplyRequestDTO withPhone = request();
        withPhone.setPhone("13800138000");
        final AppAuthApplyRequestDTO otherPhone = request();
        otherPhone.setPhone("13900139000");
        assertEquals(AppAuthLifecycleServiceImpl.canonicalRequestHash(withPhone),
                AppAuthLifecycleServiceImpl.canonicalRequestHash(otherPhone));
    }

    @Test
    void legacyDigestMatchesV1VectorAndStillCoversPhone() throws Exception {
        final byte[] vector = Files.readAllBytes(
                Paths.get("src/test/resources/app-auth/canonical-request-v1.json"));
        final AppAuthApplyRequestDTO withPhone = request();
        withPhone.setPhone(null);
        assertEquals(sha256Hex(vector), AppAuthLifecycleServiceImpl.legacyCanonicalRequestHash(withPhone));
        final AppAuthApplyRequestDTO otherPhone = request();
        otherPhone.setPhone("13900139000");
        assertNotEquals(AppAuthLifecycleServiceImpl.legacyCanonicalRequestHash(withPhone),
                AppAuthLifecycleServiceImpl.legacyCanonicalRequestHash(otherPhone));
    }
}
