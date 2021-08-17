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

import java.nio.charset.StandardCharsets;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Optional;
import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.apache.shenyu.common.exception.ShenyuException;

/**
 *  Cipher Tools.
 */
public class CipherUtils {

    public static final String AES_CBC_PKCS_5_PADDING = "AES/CBC/PKCS5Padding";

    /**
     * cipherTool.
     *
     * @param content source
     * @param mode    encryption/decryption
     * @param aesKey  key
     * @return the bytes
     */
    private static byte[] cipherTool(final byte[] content, final int mode, final String aesKey,
        final String iv) {
        byte[] plainText;
        try {
            SecretKeySpec keySpec = new SecretKeySpec(aesKey.getBytes(StandardCharsets.UTF_8),
                "AES");
            final Cipher cipher = Cipher.getInstance(AES_CBC_PKCS_5_PADDING);
            IvParameterSpec ivSpec = new IvParameterSpec(iv.getBytes(StandardCharsets.UTF_8));
            cipher.init(mode, keySpec, ivSpec);
            plainText = content;
            return cipher.doFinal(plainText);
        } catch (NoSuchAlgorithmException | InvalidKeyException | NoSuchPaddingException | BadPaddingException
            | IllegalBlockSizeException | InvalidAlgorithmParameterException e) {
            throw new ShenyuException(e);
        }
    }

    /**
     * Aes encryption.
     *
     * @param src    source
     * @param aesKey key
     * @param iv     iv
     * @return the string
     */
    public static String encryptHex(final String src, final String aesKey, final String iv) {
        return Optional.ofNullable(src).map(item -> {
            try {
                byte[] byteContent = item.getBytes(StandardCharsets.UTF_8);
                byte[] result = cipherTool(byteContent, Cipher.ENCRYPT_MODE, aesKey, iv);
                return Base64.getEncoder().encodeToString(result);
            } catch (Exception ex) {
                throw new ShenyuException(ex);
            }
        }).orElse(null);
    }

    /**
     * decryptStr.
     *
     * @param src    source
     * @param aesKey key
     * @param iv     iv
     * @return the string
     */
    public static String decryptStr(final String src, final String aesKey, final String iv) {
        return Optional.ofNullable(src).map(item -> {
            try {
                byte[] byteContent = Base64.getDecoder().decode(item);
                byte[] result = cipherTool(byteContent, Cipher.DECRYPT_MODE, aesKey, iv);
                return new String(result);
            } catch (Exception ex) {
                throw new ShenyuException(ex);
            }
        }).orElse(null);
    }
}
