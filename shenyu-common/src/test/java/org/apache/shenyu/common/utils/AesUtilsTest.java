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

package org.apache.shenyu.common.utils;

import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AesUtilsTest {

    @Test
    void testCbcEncryptAndDecrypt() {

        // 16-byte key
        String secretKey = "1234567890123456";
        // 16-byte IV
        String iv = "1234567890123456";
        String data = "Test data for AES";

        // Encrypt the data
        String encryptedData = AesUtils.cbcEncrypt(secretKey, iv, data);
        assertNotNull(encryptedData, "Encrypted data should not be null");

        // Decrypt the data
        String decryptedData = AesUtils.cbcDecrypt(secretKey, iv, encryptedData);
        Assertions.assertNotNull(decryptedData, "Decrypted data should not be null");
        assertEquals(data, decryptedData, "Decrypted data should match the original");
    }

    @Test
    void testCbcEncryptWithInvalidKey() {

        // Invalid key length
        String invalidKey = "shortkey";
        // 16-byte IV
        String iv = "1234567890123456";
        String data = "Test data for AES";

        Exception exception = assertThrows(ShenyuException.class, () -> {
            AesUtils.cbcEncrypt(invalidKey, iv, data);
        });

        // Update the assertion to match the actual exception message
        assertTrue(exception.getMessage().contains("Invalid AES key length")
                || exception.getMessage().contains("Given final block not properly padded")
                || exception.getMessage().contains("Key length not 128/192/256 bits"));
    }

    @Test
    void testCbcDecryptWithInvalidData() {

        // 16-byte key
        String secretKey = "1234567890123456";
        // 16-byte IV
        String iv = "1234567890123456";
        String invalidData = "InvalidBase64Data";

        Exception exception = assertThrows(ShenyuException.class, () -> {
            AesUtils.cbcDecrypt(secretKey, iv, invalidData);
        });

        // Update the assertion to match the actual exception message
        assertTrue(exception.getMessage().contains("Illegal base64 character")
                || exception.getMessage().contains("Input byte array has wrong 4-byte ending unit")
                || exception.getMessage().contains("Last unit does not have enough valid bits"));
    }

}
