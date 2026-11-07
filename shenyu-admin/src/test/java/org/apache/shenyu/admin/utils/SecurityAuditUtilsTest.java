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

package org.apache.shenyu.admin.utils;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

/**
 * Test case for {@link SecurityAuditUtils}.
 */
public class SecurityAuditUtilsTest {

    @Test
    public void testFingerprintKnownVector() {
        // SHA-256("abc") = ba7816bf8f01cfea...
        assertThat(SecurityAuditUtils.fingerprint("abc"), is("ba7816bf"));
    }

    @Test
    public void testFingerprintEmptyValue() {
        // SHA-256("") = e3b0c44298fc1c14...
        assertThat(SecurityAuditUtils.fingerprint(""), is("e3b0c442"));
    }

    @Test
    public void testFingerprintNullValue() {
        assertThat(SecurityAuditUtils.fingerprint(null), is(""));
    }

    @Test
    public void testFingerprintIsTruncated() {
        assertThat(SecurityAuditUtils.fingerprint("shenyu8179-sensitive-value").length(), is(8));
    }

    @Test
    public void testFingerprintDiffersForDifferentValues() {
        assertThat(SecurityAuditUtils.fingerprint("value-a"), not(is(SecurityAuditUtils.fingerprint("value-b"))));
    }

    @Test
    public void testSanitizeNullValue() {
        assertThat(SecurityAuditUtils.sanitize(null), is(""));
    }

    @Test
    public void testSanitizeRemovesLineBreaksAndControlCharacters() {
        assertThat(SecurityAuditUtils.sanitize("alice\r\nFAKE_LOG_LINE"), is("aliceFAKE_LOG_LINE"));
        assertThat(SecurityAuditUtils.sanitize("a\tb" + (char) 7 + "c"), is("abc"));
        assertThat(SecurityAuditUtils.sanitize("x" + (char) 0x1D + "y"), is("xy"));
        assertThat(SecurityAuditUtils.sanitize(String.valueOf((char) 0x7F)), is(""));
    }

    @Test
    public void testSanitizeTruncatesLongValues() {
        String longValue = "x".repeat(300);
        assertThat(SecurityAuditUtils.sanitize(longValue).length(), is(128));
    }

    @Test
    public void testSanitizeKeepsOrdinaryTextUntouched() {
        assertThat(SecurityAuditUtils.sanitize("alice_01-./platform/login"), is("alice_01-./platform/login"));
    }
}
