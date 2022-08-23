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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * test for Node.
 */
class NodeTest {

    @Test
    public void nodeTest() {
        final Node node = new Node("key", "value", 0L, 0L, 0L);

        node.setKey("key");
        node.setValue("value");
        node.setCreateReversion(0L);
        node.setModReversion(0L);
        node.setVersion(0L);
        Assertions.assertEquals(node.getCreateReversion(), 0L);
        Assertions.assertEquals(node.getModReversion(), 0L);
        Assertions.assertEquals(node.getVersion(), 0L);
        Assertions.assertEquals(node.getKey(), "key");
        Assertions.assertEquals(node.getValue(), "value");
        Assertions.assertNotNull(node.toString());
        Assertions.assertTrue(node.hashCode() != 0);

        final Node node2 = new Node("key", "value", 0L, 0L, 0L);

        Assertions.assertEquals(node2, node);
        node2.setValue("value2");
        Assertions.assertNotEquals(node2, node);
        node2.setKey("key1");
        Assertions.assertNotEquals(node2, node);
        node2.setVersion(1L);
        Assertions.assertNotEquals(node2, node);
        node2.setModReversion(1L);
        Assertions.assertNotEquals(node2, node);
        node2.setCreateReversion(1L);
        Assertions.assertNotEquals(node2, node);
        Assertions.assertNotEquals(node2, "string");
        Assertions.assertNotEquals(node2, null);
        Assertions.assertEquals(node, node);
    }
}
