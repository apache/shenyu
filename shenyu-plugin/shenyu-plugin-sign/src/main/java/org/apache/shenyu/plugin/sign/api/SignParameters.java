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

package org.apache.shenyu.plugin.sign.api;

import org.apache.shenyu.common.utils.SignUtils;

import java.net.URI;

public class SignParameters {

    public static final SignParameters VERSION_ERROR_PARAMETERS = new SignParameters();

    private final String appKey;

    private final String timestamp;

    private final String signature;

    private final URI uri;

    private final String signAlg;

    private final String version;

    private String parameters;

    public SignParameters(final String version,
                          final String appKey,
                          final String timestamp,
                          final String signature,
                          final URI uri) {
        this(version, appKey, timestamp, signature, uri, SignUtils.SIGN_MD5);
    }

    public SignParameters(final String version,
                          final String appKey,
                          final String timestamp,
                          final String signature,
                          final URI uri,
                          final String signAlg) {
        this.version = version;
        this.appKey = appKey;
        this.timestamp = timestamp;
        this.signature = signature;
        this.uri = uri;
        this.signAlg = signAlg;
    }

    public SignParameters() {
        this(null, null, null, null, null);
    }

    /**
     * Gets appKey.
     *
     * @return appKey
     */
    public String getAppKey() {
        return appKey;
    }

    /**
     * Gets timestamp.
     *
     * @return timestamp
     */
    public String getTimestamp() {
        return timestamp;
    }

    /**
     * Gets signature.
     *
     * @return signature.
     */
    public String getSignature() {
        return signature;
    }

    /**
     * Gets uri.
     *
     * @return uri
     */
    public URI getUri() {
        return uri;
    }

    /**
     * Gets signAlg.
     *
     * @return signAlg
     */
    public String getSignAlg() {
        return signAlg;
    }

    /**
     * Gets parameters.
     *
     * @return parameters
     */
    public String getParameters() {
        return parameters;
    }

    /**
     * Sets parameters.
     *
     * @param parameters parameters
     */
    public void setParameters(final String parameters) {
        this.parameters = parameters;
    }

    /**
     * Gets version.
     *
     * @return version
     */
    public String getVersion() {
        return version;
    }

    @Override
    public String toString() {
        return "SignParameters{"
                + "appKey='" + appKey + '\''
                + ", timestamp='" + timestamp + '\''
                + ", signature='" + signature + '\''
                + ", uri=" + uri
                + ", signAlg='" + signAlg + '\''
                + ", version='" + version + '\''
                + ", parameters='" + parameters + '\''
                + '}';
    }
}
