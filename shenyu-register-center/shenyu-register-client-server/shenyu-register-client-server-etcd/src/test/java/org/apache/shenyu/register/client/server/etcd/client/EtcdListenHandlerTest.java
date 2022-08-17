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

package org.apache.shenyu.register.client.server.etcd.client;

import org.junit.jupiter.api.Test;

/**
 * test for EtcdListenHandler.
 */
public class EtcdListenHandlerTest {
    @Test
    public void etcdListenHandlerTest() {
        final EtcdListenHandler etcdListenHandler = new EtcdListenHandler() {
            @Override
            public void updateHandler(final String path, final String value) {
            }

            @Override
            public void deleteHandler(final String path, final String value) {
            }
        };
        final Node node = new Node("key", "value", 0L, 0L, 0L);
        etcdListenHandler.accept(Event.UPDATE, node);
        etcdListenHandler.accept(Event.DELETE, node);
        etcdListenHandler.accept(Event.UNRECOGNIZED, node);
        etcdListenHandler.accept(Event.UNRECOGNIZED, node);
        etcdListenHandler.updateHandler(node.getKey(), node.getValue());
        etcdListenHandler.deleteHandler(node.getKey(), node.getValue());
    }
}
