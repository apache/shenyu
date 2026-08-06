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

package org.apache.shenyu.plugin.cryptor.strategy;

import org.apache.shenyu.spi.Join;

import javax.crypto.spec.OAEPParameterSpec;
import javax.crypto.spec.PSource;
import java.security.spec.AlgorithmParameterSpec;
import java.security.spec.MGF1ParameterSpec;

/**
 * rsa cryptor (default).
 *
 * <p>Uses RSAES-OAEP ({@code RSA/ECB/OAEPWithSHA-256AndMGF1Padding}) instead of the
 * legacy PKCS#1 v1.5 padding: OAEP is semantically secure and resists
 * Bleichenbacher padding-oracle attacks. The {@link OAEPParameterSpec} pins both the
 * label hash and the MGF1 hash to SHA-256 explicitly, so the transformation is
 * identical across JDKs/providers (some defaults otherwise pair a SHA-256 label
 * with a SHA-1 MGF1). OAEP also embeds an integrity check, so tampered ciphertext
 * fails on decrypt rather than silently yielding corrupted plaintext.
 *
 * <p><b>Not wire-compatible</b> with PKCS#1 v1.5. For interoperating with a peer
 * that only speaks PKCS#1 v1.5, register the rule with the {@code rsa-pkcs1}
 * strategy ({@link RsaPkcs1Strategy}) instead.
 */
@Join
public class RsaStrategy extends AbstractRsaStrategy {

    private static final String TRANSFORMATION = "RSA/ECB/OAEPWithSHA-256AndMGF1Padding";

    private static final OAEPParameterSpec OAEP_PARAMS = new OAEPParameterSpec(
            "SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT);

    @Override
    protected String transformation() {
        return TRANSFORMATION;
    }

    @Override
    protected AlgorithmParameterSpec params() {
        return OAEP_PARAMS;
    }
}
