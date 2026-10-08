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

/**
 * aes cryptor.
 *
 * <p>Key convention: {@code base64(secret)}. The secret length picks
 * AES-128/192/256. Fixed transformation: AES/GCM/NoPadding (authenticated
 * encryption with a random 96-bit nonce per message).
 */
@Join
public class AesStrategy extends AbstractAeadCryptorStrategy {

    private static final String TRANSFORMATION = "AES/GCM/NoPadding";

    private static final String ALGORITHM = "AES";

    @Override
    protected String getTransformation() {
        return TRANSFORMATION;
    }

    @Override
    protected String getAlgorithm() {
        return ALGORITHM;
    }
}
