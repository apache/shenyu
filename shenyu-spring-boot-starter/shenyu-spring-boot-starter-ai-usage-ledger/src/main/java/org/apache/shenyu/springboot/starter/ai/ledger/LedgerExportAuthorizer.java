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

package org.apache.shenyu.springboot.starter.ai.ledger;

import reactor.core.publisher.Mono;

import java.security.Principal;

/** Application security adapter; never derive scope from an export query or an arbitrary header. */
@FunctionalInterface
public interface LedgerExportAuthorizer {

    /**
     * Authorize this authenticated principal to read one namespace/tenant/application scope.
     * @param principal application-authenticated principal
     * @return trusted scope, or empty to deny
     */
    Mono<UsageLedgerStore.Scope> authorize(Principal principal);
}
