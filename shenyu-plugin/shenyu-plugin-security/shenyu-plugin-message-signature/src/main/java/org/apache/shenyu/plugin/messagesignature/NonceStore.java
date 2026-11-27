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

package org.apache.shenyu.plugin.messagesignature;

import reactor.core.publisher.Mono;

import java.time.Duration;

/** Shared replay state for the RFC 9421 plugin. */
@FunctionalInterface
public interface NonceStore {

    /**
     * Atomically reserve a verified key/nonce pair until its validity window has passed.
     *
     * @param keyId trusted verification key identifier
     * @param nonce signed request nonce
     * @param ttl reservation lifetime
     * @return true only for the first claimant; storage failures must signal an error
     */
    Mono<Boolean> claim(String keyId, String nonce, Duration ttl);
}
