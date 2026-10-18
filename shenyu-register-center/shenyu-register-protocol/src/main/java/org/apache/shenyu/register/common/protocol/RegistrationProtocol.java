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

package org.apache.shenyu.register.common.protocol;

import java.util.Objects;

/**
 * Stable HTTP registration v1 contract, independent of implementation releases.
 */
public final class RegistrationProtocol {

    /**
     * version header.
     */
    public static final String VERSION_HEADER = "X-Shenyu-Register-Protocol-Version";

    /**
     * client version header.
     */
    public static final String CLIENT_VERSION_HEADER = "X-Shenyu-Client-Version";

    /**
     * capabilities header.
     */
    public static final String CAPABILITIES_HEADER = "X-Shenyu-Register-Capabilities";

    /**
     * v1.
     */
    public static final String V1 = "1";

    /**
     * success.
     */
    public static final String SUCCESS = "success";

    /**
     * unsupported version.
     */
    public static final String UNSUPPORTED_VERSION = "Unsupported registration protocol version";

    /**
     * client path.
     */
    public static final String CLIENT_PATH = "/shenyu-client";

    /**
     * meta path.
     */
    public static final String META_PATH = "/shenyu-client/register-metadata";

    /**
     * uri path.
     */
    public static final String URI_PATH = "/shenyu-client/register-uri";

    /**
     * api doc path.
     */
    public static final String API_DOC_PATH = "/shenyu-client/register-apiDoc";

    /**
     * discovery config path.
     */
    public static final String DISCOVERY_CONFIG_PATH = "/shenyu-client/register-discoveryConfig";

    /**
     * mcp tools path.
     */
    public static final String MCP_TOOLS_PATH = "/shenyu-client/register-mcp";

    /**
     * offline path.
     */
    public static final String OFFLINE_PATH = "/shenyu-client/offline";

    /**
     * beat uri path.
     */
    public static final String BEAT_URI_PATH = "/instance/beat";

    /**
     * login path.
     */
    public static final String LOGIN_PATH = "/platform/login";

    /**
     * v1 capabilities.
     */
    public static final String V1_CAPABILITIES = "metadata,uri,api-doc,discovery-config,mcp,offline,heartbeat";

    private RegistrationProtocol() {
    }

    /**
     * Resolve legacy requests without a version header to v1.
     *
     * @param version the optional protocol header
     * @return the supported protocol version
     * @throws IllegalArgumentException when an explicit version is unsupported
     */
    public static String resolveVersion(final String version) {
        if (Objects.isNull(version) || V1.equals(version)) {
            return V1;
        }
        throw new IllegalArgumentException(UNSUPPORTED_VERSION);
    }
}
