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

package org.apache.shenyu.plugin.agent.gateway.remote;

/** Gateway-side service credential source; never resolves Agent credentials. */
@FunctionalInterface
public interface RemoteServiceCredentialResolver {
    /**
     * Resolve a credential independently bound to the actual target.
     * Called only on the managed configuration worker, not request/event-loop threads.
     * @param target immutable trusted target reference
     * @return independently bound service credential
     */
    RemoteServerBinding.Credential resolve(RemoteServerBinding.Config target);
}
