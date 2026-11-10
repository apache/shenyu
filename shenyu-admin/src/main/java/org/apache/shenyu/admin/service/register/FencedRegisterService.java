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

package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.register.common.dto.URIRegisterDTO;

import java.util.List;

/**
 * Direct registration operations used by fencing transactions.
 *
 * <p>These methods intentionally bypass the legacy retry holder. The caller
 * owns the database transaction and retries an unconfirmed sequence itself.</p>
 */
public interface FencedRegisterService {

    /**
     * Return the registration RPC type handled by this service.
     *
     * @return RPC type
     */
    String rpcType();

    /**
     * Apply a register/update without the legacy fallback queue.
     *
     * @param selectorName selector name
     * @param uriList URI payload
     * @param namespaceId namespace id
     * @return result
     */
    String registerURIWithoutFallback(String selectorName, List<URIRegisterDTO> uriList, String namespaceId);

    /**
     * Apply a heartbeat without the legacy fallback queue.
     *
     * @param selectorName selector name
     * @param uriList URI payload
     * @param namespaceId namespace id
     * @return result
     */
    String heartbeatWithoutFallback(String selectorName, List<URIRegisterDTO> uriList, String namespaceId);

    /**
     * Apply an instance draining update without the legacy fallback queue.
     *
     * @param selectorName selector name
     * @param uriList URI payload
     * @param namespaceId namespace id
     * @return result
     */
    String drainingWithoutFallback(String selectorName, List<URIRegisterDTO> uriList, String namespaceId);

    /**
     * Withdraw admission without the legacy fallback queue.
     *
     * @param selectorName selector name
     * @param uriList URI payload
     * @param namespaceId namespace id
     * @return result
     */
    String notReadyWithoutFallback(String selectorName, List<URIRegisterDTO> uriList, String namespaceId);

    /**
     * Restore admission without the legacy fallback queue.
     *
     * @param selectorName selector name
     * @param uriList URI payload
     * @param namespaceId namespace id
     * @return result
     */
    String readyWithoutFallback(String selectorName, List<URIRegisterDTO> uriList, String namespaceId);

    /**
     * Apply an offline operation without the legacy fallback queue.
     *
     * @param selectorName selector name
     * @param uriList URI payload
     * @param namespaceId namespace id
     * @return result
     */
    String offlineWithoutFallback(String selectorName, List<URIRegisterDTO> uriList, String namespaceId);
}
