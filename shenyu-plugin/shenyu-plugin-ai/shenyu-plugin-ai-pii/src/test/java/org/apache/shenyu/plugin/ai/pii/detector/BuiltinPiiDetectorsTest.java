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

package org.apache.shenyu.plugin.ai.pii.detector;

import org.apache.shenyu.plugin.ai.pii.PiiMatch;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link BuiltinPiiDetectors} and {@link PiiChecksum}.
 */
public final class BuiltinPiiDetectorsTest {

    private static final long FAR_FUTURE = System.nanoTime() + 60_000_000_000L;

    @Test
    public void testEmailDetection() {
        PiiDetector detector = BuiltinPiiDetectors.all().get(BuiltinPiiDetectors.EMAIL);
        List<PiiMatch> matches = detector.detect("contact me at john.doe@example.com please", FAR_FUTURE);
        assertEquals(1, matches.size());
        assertEquals("john.doe@example.com".length(), matches.get(0).getEnd() - matches.get(0).getStart());
    }

    @Test
    public void testPhoneCnDetection() {
        PiiDetector detector = BuiltinPiiDetectors.all().get(BuiltinPiiDetectors.PHONE_CN);
        assertEquals(1, detector.detect("call 13800138000 now", FAR_FUTURE).size());
        // a phone shaped number inside a longer digit run is not a phone
        assertTrue(detector.detect("id 9138001380001 here", FAR_FUTURE).isEmpty());
        // a landline style number does not match
        assertTrue(detector.detect("call 01012345678 now", FAR_FUTURE).isEmpty());
    }

    @Test
    public void testNationalIdChecksumRejectsInvalidCandidate() {
        PiiDetector detector = BuiltinPiiDetectors.all().get(BuiltinPiiDetectors.NATIONAL_ID_CN);
        // valid resident id with a correct mod 11-2 checksum
        assertEquals(1, detector.detect("number 11010519491231002X end", FAR_FUTURE).size());
        // same shape, broken checksum character
        assertTrue(detector.detect("number 110105194912310021 end", FAR_FUTURE).isEmpty());
    }

    @Test
    public void testNationalIdInsidePhoneDigitsIsNotAPhone() {
        PiiDetector phone = BuiltinPiiDetectors.all().get(BuiltinPiiDetectors.PHONE_CN);
        // an 18 digit resident id contains 11 digit runs, the look around rejects them
        assertTrue(phone.detect("11010519491231002X", FAR_FUTURE).isEmpty());
    }

    @Test
    public void testCreditCardLuhn() {
        PiiDetector detector = BuiltinPiiDetectors.all().get(BuiltinPiiDetectors.CREDIT_CARD);
        // a classic Luhn valid test number
        assertEquals(1, detector.detect("card 4111111111111111 ok", FAR_FUTURE).size());
        // broken Luhn
        assertTrue(detector.detect("card 4111111111111112 ok", FAR_FUTURE).isEmpty());
    }

    @Test
    public void testSelectEnablesSubsetAndSkipsUnknown() {
        Map<String, PiiDetector> selected = BuiltinPiiDetectors.select(
                List.of(BuiltinPiiDetectors.EMAIL, "NOT_A_TYPE"));
        assertEquals(1, selected.size());
        assertTrue(selected.containsKey(BuiltinPiiDetectors.EMAIL));
        assertEquals(4, BuiltinPiiDetectors.select(List.of()).size());
    }

    @Test
    public void testDeadlineAbortsLongScan() {
        PiiDetector detector = new RegexPiiDetector("BACKTRACK",
                java.util.regex.Pattern.compile("(a+)+b"));
        StringBuilder text = new StringBuilder();
        for (int i = 0; i < 5000; i++) {
            text.append('a');
        }
        long expired = System.nanoTime() - 1L;
        assertThrows(PiiDetectorTimeoutException.class, () -> detector.detect(text, expired));
    }

    @Test
    public void testChecksumHelpers() {
        assertTrue(PiiChecksum.isValidMod11("11010519491231002X"));
        assertFalse(PiiChecksum.isValidMod11("110105194912310021"));
        assertTrue(PiiChecksum.isValidLuhn("4111111111111111"));
        assertFalse(PiiChecksum.isValidLuhn("4111111111111112"));
        assertFalse(PiiChecksum.isValidLuhn(null));
        assertFalse(PiiChecksum.isValidMod11("123"));
    }
}
