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

/**
 * The upstream reported that the session-bound request can no longer be served.
 * The original invocation has already been issued, so callers must return this outcome
 * instead of replaying it after session recovery.
 */
public final class RemoteSessionExpiredException extends IllegalStateException {

    public static final int HTTP_STATUS = 502;

    public static final int RPC_CODE = -32026;

    public static final String MESSAGE = "Remote tool outcome is unknown after session expiry";

    private static final long serialVersionUID = 1L;

    RemoteSessionExpiredException() {
        super("Remote MCP session expired");
    }
}
