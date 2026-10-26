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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

class RegistrarNamespaceTest {

    @Test
    void testNamespaceConfiguration() {
        assertEquals(Collections.singletonList(Constants.SYS_DEFAULT_NAMESPACE_ID), config(null).getNamespace());
        assertEquals(Collections.singletonList(Constants.SYS_DEFAULT_NAMESPACE_ID), config(" ").getNamespace());
        assertEquals(Arrays.asList("first", "second"), config("first" + Constants.SEPARATOR_CHARS + "second").getNamespace());
        assertEquals(Collections.singletonList(Constants.SYS_DEFAULT_NAMESPACE_ID), new TestClientRegisterConfig().getNamespace());
    }

    @Test
    void testUriAndMetadataUseIndependentNamespaceEvents() throws Exception {
        ClientRegisterConfig config = config("first" + Constants.SEPARATOR_CHARS + "second");
        for (BaseApiRegistrarImpl registrar : Arrays.asList(new UriApiRegistrarImplImpl(config), new MateDataApiRegistrarImpl(config))) {
            ShenyuClientRegisterEventPublisher publisher = mock(ShenyuClientRegisterEventPublisher.class);
            ReflectionTestUtils.setField(registrar, "publisher", publisher);
            ApiBean bean = new ApiBean(RpcTypeEnum.HTTP.getName(), "service", this, "/service");
            bean.addApiDefinition(Object.class.getMethod("toString"), "/method");
            registrar.doRegisterBean(bean);
            registrar.doRegisterApi(bean.getApiDefinitions().get(0));
            ArgumentCaptor<DataTypeParent> captor = ArgumentCaptor.forClass(DataTypeParent.class);
            verify(publisher, times(4)).publishEvent(captor.capture());
            List<DataTypeParent> events = captor.getAllValues();
            for (int index = 0; index < events.size(); index += 2) {
                assertNotSame(events.get(index), events.get(index + 1));
                assertEquals("first", namespace(events.get(index)));
                assertEquals("second", namespace(events.get(index + 1)));
            }
        }
    }

    private ClientRegisterConfig config(final String namespace) {
        ShenyuClientConfig config = new ShenyuClientConfig();
        config.setNamespace(namespace);
        ShenyuClientConfig.ClientPropertiesConfig properties = new ShenyuClientConfig.ClientPropertiesConfig();
        properties.getProps().setProperty("port", "8080");
        properties.getProps().setProperty("host", "127.0.0.1");
        properties.getProps().setProperty("appName", "service");
        config.getClient().put(RpcTypeEnum.HTTP.getName(), properties);
        return new ClientRegisterConfigImpl(config, RpcTypeEnum.HTTP, null, null);
    }

    private String namespace(final DataTypeParent event) {
        return event instanceof URIRegisterDTO ? ((URIRegisterDTO) event).getNamespaceId() : ((MetaDataRegisterDTO) event).getNamespaceId();
    }
}
