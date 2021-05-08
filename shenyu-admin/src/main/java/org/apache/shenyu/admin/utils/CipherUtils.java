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

import org.apache.shenyu.common.exception.SoulException;
import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Optional;

/**
 *  Cipher Tools.
 *
 * @author nuo-promise
 */
public class CipherUtils {

    /**
     *  Check if the packet is a multiple of 16.
     *
     * @param srcBytes the bytes
     * @param blockSize block size
     * @return the bytes
     */
    private static byte[] addZeroPadding(final byte[] srcBytes, final int blockSize) {
        byte[] plainText;
        int length = srcBytes.length;
        if (length % blockSize != 0) {
            length = length + (blockSize - (length % blockSize));
        }
        plainText = new byte[length];
        System.arraycopy(srcBytes, 0, plainText, 0, srcBytes.length);
        for (int i = srcBytes.length; i < length; i++) {
            plainText[i] = '\0';
        }
        return plainText;
    }

    /**
     *  delete zero padding.
     *
     * @param src the deleteZeroPadding source
     * @return the bytes
     */
    private static byte[] deleteZeroPadding(final byte[] src) {
        int length = 0;
        byte[] plainText = null;
        if (src.length > 0) {
            for (int i = src.length - 1; i > 0; i--) {
                if (src[i] != '\0') {
                    length = i;
                    break;
                }
            }
            plainText = new byte[length + 1];
            System.arraycopy(src, 0, plainText, 0, length + 1);
        }
        return plainText;
    }

    /**
     * cipherTool.
     *
     * @param content source
     * @param mode encryption/decryption
     * @param aesKey key
     * @return the bytes
     */
    private static byte[] cipherTool(final byte[] content, final int mode, final String aesKey) {
        byte[] plainText;
        try {
            SecretKeySpec keySpec = new SecretKeySpec(aesKey.getBytes(StandardCharsets.UTF_8), "AES");
            Cipher cipher = Cipher.getInstance("AES/ECB/NoPadding");
            cipher.init(mode, keySpec);
            if (mode == Cipher.ENCRYPT_MODE) {
                plainText = addZeroPadding(content, cipher.getBlockSize());
            } else {
                plainText = content;
            }
            return cipher.doFinal(plainText);
        } catch (NoSuchAlgorithmException | InvalidKeyException | NoSuchPaddingException | BadPaddingException | IllegalBlockSizeException e) {
            throw new SoulException(e);
        }
    }

    /**
     * Aes encryption.
     *
     * @param src  source
     * @param aesKey key
     * @return the string
     */
    public static String encryptHex(final String src, final String aesKey) {
        return Optional.ofNullable(src).map(item -> {
            try {
                byte[] byteContent = item.getBytes(StandardCharsets.UTF_8);
                byte[] result = cipherTool(byteContent, Cipher.ENCRYPT_MODE, aesKey);
                return Base64.getEncoder().encodeToString(result);
            } catch (Exception ex) {
                throw new SoulException(ex);
            }
        }).orElse(null);
    }

    /**
     * decryptStr.
     *
     * @param src source
     * @param aesKey key
     * @return the string
     */
    public static String decryptStr(final String src, final String aesKey) {
        return Optional.ofNullable(src).map(item -> {
            try {
                byte[] byteContent = Base64.getDecoder().decode(item);
                byte[] result = cipherTool(byteContent, Cipher.DECRYPT_MODE, aesKey);
                return new String(deleteZeroPadding(result));
            } catch (Exception ex) {
                throw new SoulException(ex);
            }
        }).orElse(null);
    }
}
