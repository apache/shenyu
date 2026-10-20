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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;

/**
 * X1 app auth extension facade.
 * After authentication and authorization the facade delegates each transaction to a dedicated transaction bean
 */
public interface AppAuthLifecycleService {

    /**
     * (self-invocation that would bypass transactions is forbidden).
     *
     * @param request apply request
     * @return ShenyuAdminResult whose typed data is an AppAuthResultVO
     */
    ShenyuAdminResult apply(AppAuthApplyRequestDTO request);

    /**
     * Query the result of a previous apply (read-only, recovers lost responses).
     *
     * @param query result query
     * @return ShenyuAdminResult whose typed data is an AppAuthResultVO
     */
    ShenyuAdminResult result(AppAuthResultQueryDTO query);

    /**
     * Revoke a request: locks the same idempotency key as apply and, if created, deletes the auth leaving a REVOKED marker.
     *
     * @param request revoke request
     * @return ShenyuAdminResult
     */
    ShenyuAdminResult revoke(AppAuthRevokeRequestDTO request);

    /**
     * Replace the managed auth paths as a whole (an empty array really clears them).
     *
     * @param request replacePaths request
     * @return ShenyuAdminResult
     */
    ShenyuAdminResult replacePaths(AppAuthReplacePathsRequestDTO request);
}
