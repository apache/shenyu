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

package org.apache.shenyu.register.server.etcd.client;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.function.BiConsumer;

/**
 * etcd event handler.
 */
public abstract class EtcdListenHandler implements BiConsumer<Event, Node> {

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdListenHandler.class);

    /**
     * etcd data update event handler.
     * @param path data path
     * @param value data
     */
    public abstract void updateHandler(String path, String value);

    /**
     * etcd data delete event handler.
     * @param path data path
     * @param value data
     */
    public abstract void deleteHandler(String path, String value);

    @Override
    public void accept(final Event event, final Node node) {
        switch (event) {
            case DELETE:
                deleteHandler(node.getKey(), node.getValue());
                break;
            case UPDATE:
                updateHandler(node.getKey(), node.getValue());
                break;
            default:
                LOGGER.info(String.format("unrecognized event: %s, key: %s", event, node.getKey()));
        }
    }
}
