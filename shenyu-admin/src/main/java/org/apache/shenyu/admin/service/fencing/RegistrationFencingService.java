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

package org.apache.shenyu.admin.service.fencing;

import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;

/**
 * Synchronous persistence and write application for boot-session fencing.
 */
public interface RegistrationFencingService {

    /**
     * Create an exact, root-owned target binding.
     *
     * @param request target request
     * @param root whether the caller is the existing root principal
     * @return target data
     */
    RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData> createTarget(
            RegistrationFencingProtocol.TargetRequest request, boolean root);

    /**
     * Replace an owner using an explicit root CAS.
     *
     * @param targetId target id
     * @param request replacement request
     * @param root whether the caller is the existing root principal
     * @return new session data
     */
    RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> replace(
            String targetId, RegistrationFencingProtocol.ReplaceRequest request, boolean root);

    /**
     * Disable a target and terminate its session.
     *
     * @param targetId target id
     * @param root whether the caller is the existing root principal
     * @return target data
     */
    RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData> disable(String targetId, boolean root);

    /**
     * Acquire or replay a session.
     *
     * @param request acquire request
     * @param principalId authenticated business principal
     * @return session data
     */
    RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> acquire(
            RegistrationFencingProtocol.AcquireRequest request, String principalId);

    /**
     * Apply one ordered mutation.
     *
     * @param request mutation request
     * @param principalId authenticated business principal
     * @return mutation data
     */
    RegistrationFencingProtocol.Response<RegistrationFencingProtocol.MutationData> mutate(
            RegistrationFencingProtocol.MutationRequest request, String principalId);
}
