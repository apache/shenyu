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

package org.apache.shenyu.protocol.mqtt.repositories;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for {@link SessionRepository}: the session index belongs to the
 * instance, so sessions stored through one repository are invisible to another (#8021).
 */
public final class SessionRepositoryTest {

    private static final String CLIENT_ID = "session-client";

    @Test
    public void instancesDoNotShareSessions() {
        SessionRepository first = new SessionRepository();
        SessionRepository second = new SessionRepository();

        first.add(CLIENT_ID, new MqttSession(CLIENT_ID, false));

        assertNotNull(first.get(CLIENT_ID));
        assertNull(second.get(CLIENT_ID),
                "a static index made every instance observe every other instance's sessions");
    }

    @Test
    public void removeOnlyAffectsTheOwningInstance() {
        SessionRepository first = new SessionRepository();
        SessionRepository second = new SessionRepository();
        first.add(CLIENT_ID, new MqttSession(CLIENT_ID, false));
        second.add(CLIENT_ID, new MqttSession(CLIENT_ID, false));

        first.remove(CLIENT_ID);

        assertNull(first.get(CLIENT_ID));
        assertNotNull(second.get(CLIENT_ID));
    }
}
