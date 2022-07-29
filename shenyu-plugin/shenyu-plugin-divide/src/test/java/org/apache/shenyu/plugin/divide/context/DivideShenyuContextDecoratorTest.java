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

package org.apache.shenyu.plugin.divide.context;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

/**
 * DivideShenyuContextDecorator test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DivideShenyuContextDecoratorTest {

    private static final String MOCK_CONTEXT_PATH = "mockContextPath";

    private DivideShenyuContextDecorator divideShenyuContextDecorator;

    @BeforeEach
    public void setUp() {
        this.divideShenyuContextDecorator = new DivideShenyuContextDecorator();
    }

    /**
     * decoratorTest test case.
     */
    @Test
    public void decoratorTest() {
        MetaData metaData = new MetaData();
        ShenyuContext shenyuContext = new ShenyuContext();
        shenyuContext.setPath(MOCK_CONTEXT_PATH);
        ShenyuContext decorator = divideShenyuContextDecorator.decorator(shenyuContext, metaData);
        assert MOCK_CONTEXT_PATH.equals(decorator.getMethod());
        assert MOCK_CONTEXT_PATH.equals(decorator.getRealUrl());
    }
}
