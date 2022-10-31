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

package org.apache.shenyu.plugin.websocket.context;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * The Test Case For WebSocketShenyuContextDecorator.
 */
public final class WebSocketShenyuContextDecoratorTest {

    private WebSocketShenyuContextDecorator webSocketShenyuContextDecorator;

    @BeforeEach
    public void setUp() {
        this.webSocketShenyuContextDecorator = new WebSocketShenyuContextDecorator();
    }

    @Test
    public void testDecorator() {
        MetaData metaData = null;
        ShenyuContext shenyuContext = new ShenyuContext();
        webSocketShenyuContextDecorator.decorator(shenyuContext, metaData);
        Assertions.assertEquals(shenyuContext.getMethod(), null);
        Assertions.assertEquals(shenyuContext.getRpcType(), "websocket");
        Assertions.assertEquals(shenyuContext.getModule(), "websocket-websocket");
    }

    @Test
    public void testRpcType() {
        Assertions.assertEquals(webSocketShenyuContextDecorator.rpcType(), "websocket");
    }
}
