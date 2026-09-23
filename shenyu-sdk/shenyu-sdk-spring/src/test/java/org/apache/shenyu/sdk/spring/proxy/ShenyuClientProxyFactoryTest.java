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

package org.apache.shenyu.sdk.spring.proxy;

import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.spring.ShenyuClientFactoryBean;
import org.apache.shenyu.sdk.spring.factory.Contract;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationContext;

/**
 * {@link ShenyuClientProxyFactory} test.
 */
public class ShenyuClientProxyFactoryTest {

    @Test
    public void testCreateContextSpecificProxy() {
        ApplicationContext firstContext = mock(ApplicationContext.class);
        ApplicationContext secondContext = mock(ApplicationContext.class);
        prepareContext(firstContext);
        prepareContext(secondContext);

        Object firstProxy = ShenyuClientProxyFactory.createProxy(TestClient.class, firstContext, new ShenyuClientFactoryBean());
        Object secondProxy = ShenyuClientProxyFactory.createProxy(TestClient.class, secondContext, new ShenyuClientFactoryBean());

        assertNotSame(firstProxy, secondProxy);
    }

    private void prepareContext(final ApplicationContext context) {
        Contract contract = mock(Contract.class);
        when(contract.parseAndValidateRequestTemplate(eq(TestClient.class), any(ShenyuClientFactoryBean.class))).thenReturn(Collections.emptyList());
        when(context.getBean(Contract.class)).thenReturn(contract);
        when(context.getBean(ShenyuSdkClient.class)).thenReturn(mock(ShenyuSdkClient.class));
        when(context.getBeansOfType(org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor.class)).thenReturn(Collections.emptyMap());
    }

    private interface TestClient {
    }

}
