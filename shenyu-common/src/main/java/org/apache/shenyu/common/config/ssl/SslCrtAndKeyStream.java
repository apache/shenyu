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

package org.apache.shenyu.common.config.ssl;

import java.io.InputStream;

/**
 * ssl certificate and key.
 */
public class SslCrtAndKeyStream implements SslCrtAndKey {

    private String domain;

    private InputStream keyCertChainInputStream;

    private InputStream keyInputStream;

    public SslCrtAndKeyStream() {
    }

    public SslCrtAndKeyStream(final String domain, final InputStream keyCertChainInputStream, final InputStream keyInputStream) {
        this.domain = domain;
        this.keyCertChainInputStream = keyCertChainInputStream;
        this.keyInputStream = keyInputStream;
    }

    /**
     * get domain.
     *
     * @return domain
     */
    public String getDomain() {
        return domain;
    }

    /**
     * get keyCertChainInputStream.
     *
     * @return keyCertChainInputStream
     */
    public InputStream getKeyCertChainInputStream() {
        return keyCertChainInputStream;
    }

    /**
     * get keyInputStream.
     *
     * @return keyInputStream
     */
    public InputStream getKeyInputStream() {
        return keyInputStream;
    }
}
