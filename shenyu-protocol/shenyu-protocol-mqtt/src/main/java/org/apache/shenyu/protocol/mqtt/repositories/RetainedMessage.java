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

import io.netty.handler.codec.mqtt.MqttQoS;

/**
 * A retained message together with the QoS of the publish that stored it.
 */
public final class RetainedMessage {

    private final byte[] payload;

    private final MqttQoS qos;

    /**
     * Create a retained message.
     *
     * @param payload the retained payload
     * @param qos the qos of the original retained publish
     */
    public RetainedMessage(final byte[] payload, final MqttQoS qos) {
        this.payload = payload;
        this.qos = qos;
    }

    /**
     * Get the retained payload.
     *
     * @return the payload
     */
    public byte[] getPayload() {
        return payload;
    }

    /**
     * Get the qos of the original retained publish.
     *
     * @return the qos
     */
    public MqttQoS getQos() {
        return qos;
    }
}

