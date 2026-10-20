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

package org.apache.shenyu.common.utils;

import java.util.OptionalInt;

/**
 * Selects the port of the application server that should identify a gateway instance.
 */
public final class BoundServerPort {

    /**
     * Namespace of Spring Boot's separate management server.
     */
    public static final String MANAGEMENT_NAMESPACE = "management";

    private BoundServerPort() {
    }

    /**
     * Accept a listener port only after the application server has bound it.
     * A management server and any non-positive observation are not the gateway listener.
     *
     * @param serverNamespace server namespace, empty for the primary application server
     * @param port observed port
     * @return the bound application port, or empty when the observation must not be reported
     */
    public static OptionalInt resolve(final String serverNamespace, final int port) {
        if (MANAGEMENT_NAMESPACE.equals(serverNamespace) || port <= 0) {
            return OptionalInt.empty();
        }
        return OptionalInt.of(port);
    }
}
