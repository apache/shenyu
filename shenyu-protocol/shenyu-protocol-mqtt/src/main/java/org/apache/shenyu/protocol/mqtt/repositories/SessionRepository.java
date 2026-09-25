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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Session repository, keyed by clientId.
 */
public class SessionRepository implements BaseRepository<String, MqttSession> {

    private static final Map<String, MqttSession> SESSION_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final String clientId, final MqttSession session) {
        SESSION_FACTORY.put(clientId, session);
    }

    @Override
    public void remove(final String clientId) {
        SESSION_FACTORY.remove(clientId);
    }

    @Override
    public MqttSession get(final String clientId) {
        return SESSION_FACTORY.get(clientId);
    }

}
