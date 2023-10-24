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

package org.apache.shenyu.register.client.server.apollo;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;

import com.ctrip.framework.apollo.Config;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration(classes = {ApolloClientServerRegisterRepositoryTest.class})
@RunWith(SpringJUnit4ClassRunner.class)
public class ApolloClientServerRegisterRepositoryTest {


    /**
     * Method under test: {@link ApolloClientServerRegisterRepository#init(ShenyuClientServerRegisterPublisher, ShenyuRegisterCenterConfig)}.
     */
    @Test
    public void testInit() throws NoSuchFieldException, IllegalAccessException {
        final ApolloClientServerRegisterRepository apolloClientServerRegisterRepository = new ApolloClientServerRegisterRepository();
        final ShenyuClientServerRegisterPublisher publisher = mock(ShenyuClientServerRegisterPublisher.class);
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists("http://localhost:8080");
        config.getProps().setProperty("appId", "shenyu");
        config.getProps().setProperty("clusterName", "cluster-name");
        apolloClientServerRegisterRepository.init(publisher, config);
        Field publisherField = ApolloClientServerRegisterRepository.class.getDeclaredField("publisher");
        Field configField = ApolloClientServerRegisterRepository.class.getDeclaredField("config");
        publisherField.setAccessible(true);
        configField.setAccessible(true);
        ShenyuClientServerRegisterPublisher actualPublisher = (ShenyuClientServerRegisterPublisher) publisherField.get(apolloClientServerRegisterRepository);
        Config actualConfig = (Config) configField.get(apolloClientServerRegisterRepository);
        assertNotNull(actualPublisher);
        assertNotNull(actualConfig);
    }

    /**
     * Method under test: {@link ApolloClientServerRegisterRepository#close()}.
     */
    @Test
    public void testClose() throws Exception {
        ShenyuClientServerRegisterPublisher publisher = mock(ShenyuClientServerRegisterPublisher.class);
        ApolloClientServerRegisterRepository apolloClientServerRegisterRepository = new ApolloClientServerRegisterRepository();
        Field publisherField = ApolloClientServerRegisterRepository.class.getDeclaredField("publisher");
        publisherField.setAccessible(true);
        publisherField.set(apolloClientServerRegisterRepository, publisher);
        apolloClientServerRegisterRepository.close();
        verify(publisher).close();
    }
}

