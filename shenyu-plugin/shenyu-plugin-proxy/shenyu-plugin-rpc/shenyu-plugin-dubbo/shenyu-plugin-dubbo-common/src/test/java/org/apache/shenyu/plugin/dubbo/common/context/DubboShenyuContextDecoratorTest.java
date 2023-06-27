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

package org.apache.shenyu.plugin.dubbo.common.context;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

/**
 * DubboShenyuContextDecorator test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DubboShenyuContextDecoratorTest {

    private static final String MOCK_ID = "MOCK_ID";

    private static final String MOCK_APP_NAME = "mockAppName";

    private static final String MOCK_SERVICE_NAME = "mockServiceName";

    private static final String MOCK_CONTEXT_PATH = "mockContextPath";

    private DubboShenyuContextDecorator dubboShenyuContextDecorator;

    @BeforeEach
    public void setUp() {
        this.dubboShenyuContextDecorator = new DubboShenyuContextDecorator();
    }

    @Test
    public void decorator() {
        MetaData metaData = MetaData.builder()
                .id(MOCK_ID)
                .appName(MOCK_APP_NAME)
                .contextPath(MOCK_CONTEXT_PATH)
                .serviceName(MOCK_SERVICE_NAME)
                .build();
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext = dubboShenyuContextDecorator.decorator(shenyuContext, metaData);
        assert MOCK_APP_NAME.equals(shenyuContext.getModule());
        assert MOCK_CONTEXT_PATH.equals(shenyuContext.getContextPath());
        assert MOCK_SERVICE_NAME.equals(shenyuContext.getMethod());
    }

    @Test
    public void rpcType() {
        assert RpcTypeEnum.DUBBO.getName().equals(dubboShenyuContextDecorator.rpcType());
    }

}
