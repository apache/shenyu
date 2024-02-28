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

/**
 * ssl certificate and key.
 */
public class SslCrtAndKeyFile implements SslCrtAndKey {

    private String domain;

    private String keyCertChainFile;

    private String keyFile;

    public SslCrtAndKeyFile() {
    }

    public SslCrtAndKeyFile(final String domain, final String keyCertChainFile, final String keyFile) {
        this.domain = domain;
        this.keyCertChainFile = keyCertChainFile;
        this.keyFile = keyFile;
    }

    /**
     * set domain.
     *
     * @param domain domain
     */
    public void setDomain(final String domain) {
        this.domain = domain;
    }

    /**
     * set keyCertChainFile.
     *
     * @param keyCertChainFile keyCertChainFile
     */
    public void setKeyCertChainFile(final String keyCertChainFile) {
        this.keyCertChainFile = keyCertChainFile;
    }

    /**
     * set keyFile.
     *
     * @param keyFile keyFile
     */
    public void setKeyFile(final String keyFile) {
        this.keyFile = keyFile;
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
     * get keyCertChainFile.
     *
     * @return keyCertChainFile
     */
    public String getKeyCertChainFile() {
        return keyCertChainFile;
    }

    /**
     * get keyFile.
     *
     * @return keyFile
     */
    public String getKeyFile() {
        return keyFile;
    }
}
