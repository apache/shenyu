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

import java.nio.charset.StandardCharsets;
import java.security.Security;
import java.util.Base64;
import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.apache.shenyu.common.exception.ShenyuException;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * AesUtils.
 */
public class AesUtils {

    private static final Logger LOG = LoggerFactory.getLogger(AesUtils.class);

    /**
     * AES/CBC/Pkcs7Padding encrypt data.
     *
     * @param secretKeyStr secretKeyStr
     * @param ivStr        ivStr
     * @param data         data
     * @return cbcEncrypt str.
     */
    public static String cbcEncrypt(final String secretKeyStr, final String ivStr, final String data) {
        Security.addProvider(new BouncyCastleProvider());
        String encryptStr;
        byte[] secretKeyBytes = secretKeyStr.getBytes(StandardCharsets.UTF_8);
        byte[] ivBytes = ivStr.getBytes(StandardCharsets.UTF_8);
        try {
            SecretKey secretKey = new SecretKeySpec(secretKeyBytes, "AES");
            Cipher cipher = Cipher.getInstance("AES/CBC/Pkcs7Padding");
            IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);
            cipher.init(Cipher.ENCRYPT_MODE, secretKey, ivParameterSpec);
            byte[] encryptedBytes = cipher.doFinal(data.getBytes());
            encryptStr = Base64.getEncoder().encodeToString(encryptedBytes);
        } catch (Exception e) {
            LOG.error("aes encrypt fail. cause:{}", e.getMessage());
            throw new ShenyuException(e);
        }
        return encryptStr;
    }

    /**
     * AES/CBC/Pkcs7Padding decrypt data.
     *
     * @param secretKeyStr secretKeyStr
     * @param ivStr        ivStr
     * @param data         data
     * @return cbcDecrypt str.
     */
    public static String cbcDecrypt(final String secretKeyStr, final String ivStr, final String data) {
        Security.addProvider(new BouncyCastleProvider());
        String decryptStr;
        byte[] secretKeyBytes = secretKeyStr.getBytes(StandardCharsets.UTF_8);
        byte[] ivBytes = ivStr.getBytes(StandardCharsets.UTF_8);
        try {
            SecretKey secretKey = new SecretKeySpec(secretKeyBytes, "AES");
            Cipher cipher = Cipher.getInstance("AES/CBC/Pkcs7Padding");
            IvParameterSpec ivParameterSpec = new IvParameterSpec(ivBytes);
            cipher.init(Cipher.DECRYPT_MODE, secretKey, ivParameterSpec);
            byte[] decryptedBytes = cipher.doFinal(Base64.getDecoder().decode(data));
            decryptStr = new String(decryptedBytes);
        } catch (Exception e) {
            LOG.error("aes decrypt fail. cause:{}", e.getMessage());
            throw new ShenyuException(e);
        }
        return decryptStr;
    }
}
