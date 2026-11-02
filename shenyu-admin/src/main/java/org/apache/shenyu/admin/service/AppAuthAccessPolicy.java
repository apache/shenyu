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

import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;

/**
 * Access policy for the app-auth lifecycle API: a triple check of service identity
 * (immutable userId whitelist), Shiro action permission and namespace ownership.
 *
 * <p>Regular dashboard users never obtain lifecycle identity automatically; cross-ownership
 * access and missing resources fail uniformly without leaking resource existence.
 */
public interface AppAuthAccessPolicy {

    /**
     * Requires the caller to be a deployment-configured service account holding the action
     * permission, and returns its immutable callerKey.
     *
     * @param permission Shiro action permission, e.g. system:authen:lifecycleApply
     * @return callerKey derived from the immutable userId
     */
    String requireServiceCaller(String permission);

    /**
     * Requires the caller to own the namespace (NamespaceUserService relation).
     *
     * @param callerKey caller key
     * @param namespaceId namespace id
     */
    void requireNamespace(String callerKey, String namespaceId);

    /**
     * Requires the auth id to belong to the caller and the record to be usable
     * (idempotency association + app_auth namespace + CREATED state).
     *
     * @param callerKey caller key
     * @param namespaceId namespace id
     * @param authId auth id
     * @return the associated record under locking semantics, never null
     */
    AppAuthRequestDO requireOwnedAuth(String callerKey, String namespaceId, String authId);
}
