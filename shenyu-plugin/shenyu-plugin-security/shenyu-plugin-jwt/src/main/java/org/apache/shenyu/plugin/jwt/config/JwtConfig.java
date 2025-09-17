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

package org.apache.shenyu.plugin.jwt.config;

import java.io.Serializable;

/**
 * The type Jwt config.
 */
public class JwtConfig implements Serializable {

    /**
     * private key.
     */
    private String secretKey;

    /**
     * private handleType.
     */
    private String handleType;

    /**
     * Gets secret key.
     *
     * @return the secret key
     */
    public String getSecretKey() {
        return secretKey;
    }

    /**
     * Sets secret key.
     *
     * @param secretKey the secret key
     */
    public void setSecretKey(final String secretKey) {
        this.secretKey = secretKey;
    }

    /**
     * Gets handleType.
     *
     * @return jwt payload handle type
     */
    public String getHandleType() {
        return handleType;
    }

    /**
     * Sets handleType.
     *
     * @param handleType jwt payload handle type
     */
    public void setHandleType(final String handleType) {
        this.handleType = handleType;
    }
}
