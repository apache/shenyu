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

package org.apache.shenyu.plugin.websocket;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.web.reactive.socket.CloseStatus;

import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * Tests the close status relayed to the peer.
 */
final class WebSocketCloseStatusTest {

    @ParameterizedTest
    @ValueSource(ints = {1000, 1012, 1013, 1014, 3000, 4999})
    void preservesCodeAndReasonForSendableCloseStatus(final int code) {
        CloseStatus status = new CloseStatus(code, "Peer closing connection");

        assertSame(status, WebSocketPlugin.adaptCloseStatus(status));
    }

    @ParameterizedTest
    @ValueSource(ints = {1004, 1005, 1006, 1015, 1016})
    void stillMapsReservedAndUnassignedCloseStatusToProtocolError(final int code) {
        assertSame(CloseStatus.PROTOCOL_ERROR, WebSocketPlugin.adaptCloseStatus(new CloseStatus(code)));
    }
}
