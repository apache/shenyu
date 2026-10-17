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

import java.security.spec.AlgorithmParameterSpec;

/**
 * rsa cryptor using legacy PKCS#1 v1.5 padding ({@code RSA/ECB/PKCS1Padding}).
 *
 * <p>Provided purely for backward compatibility and for interoperating with
 * peers that only speak PKCS#1 v1.5. PKCS#1 v1.5 is not semantically secure and
 * is vulnerable to Bleichenbacher padding-oracle attacks; new deployments should
 * prefer the default {@link RsaStrategy} (OAEP, SPI name {@code rsa}). Register a
 * rule with the SPI name {@code rsa-pkcs1} only when a PKCS#1 v1.5 peer leaves no
 * other choice.
 */
@Join
public class RsaPkcs1Strategy extends AbstractRsaStrategy {

    private static final String TRANSFORMATION = "RSA/ECB/PKCS1Padding";

    @Override
    protected String transformation() {
        return TRANSFORMATION;
    }

    @Override
    protected AlgorithmParameterSpec params() {
        return null;
    }
}
