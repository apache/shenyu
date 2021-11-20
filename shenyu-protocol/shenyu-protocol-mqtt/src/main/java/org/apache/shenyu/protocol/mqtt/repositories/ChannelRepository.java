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

import io.netty.channel.Channel;

import java.util.Map;

/**
 * channel repository.
 */
public interface ChannelRepository {

    /**
     * add channel.
     * @param clientId clientId
     * @param channel channel
     */
    void add(String clientId, Channel channel);

    /**
     * remove by clientId.
     * @param clientId clientId
     */
    void remove(String clientId);

    /**
     * get factory.
     * @return factory.
     */
    Map<String, Channel> getFactory();

    /**
     * get channel.
     * @param clientId clientId
     * @return Channel
     */
    Channel get(String clientId);

}
