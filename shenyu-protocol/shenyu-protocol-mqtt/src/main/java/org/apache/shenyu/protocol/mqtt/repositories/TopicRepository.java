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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Topic repository.
 * Save the posted message.
 * {@link org.apache.shenyu.protocol.mqtt.agent.MessageAgent}
 */
public class TopicRepository implements BaseRepository<String, String> {

    private static final Map<String, String> TOPIC_FACTORY = new ConcurrentHashMap<>();

    @Override
    public void add(final String topic, final String message) {
        //// todo MessageAgent.java. Carry out message processing and processing
        CompletableFuture.runAsync(() -> TOPIC_FACTORY.put(topic, message));
    }

    @Override
    public void remove(final String topic) {
        TOPIC_FACTORY.remove(topic);
    }

    @Override
    public String get(final String topic) {
        return TOPIC_FACTORY.getOrDefault(topic, null);
    }

}
