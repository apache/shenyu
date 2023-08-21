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

package org.apache.shenyu.admin.listener.apollo;

import org.apache.shenyu.admin.config.properties.ApolloProperties;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {ApolloDataChangedListener.class})
@RunWith(SpringJUnit4ClassRunner.class)
public class ApolloDataChangedListenerTest {

    @MockBean
    private ApolloClient apolloClient;

    @Autowired
    private ApolloDataChangedListener apolloDataChangedListener;

    /**
     * Method under test: {@link ApolloDataChangedListener#ApolloDataChangedListener(ApolloClient)}.
     */
    @Test
    public void testConstructor() {
        ApolloProperties apolloConfig = new ApolloProperties();
        apolloConfig.setAppId("42");
        apolloConfig.setClusterName("Cluster Name");
        apolloConfig.setEnv("Env");
        apolloConfig.setMeta("Meta");
        apolloConfig.setNamespace("Namespace");
        apolloConfig.setPortalUrl("http://localhost:8080");
        apolloConfig.setToken("ABC123");
        new ApolloDataChangedListener(new ApolloClient(apolloConfig));
    }

    /**
     * Method under test: {@link ApolloDataChangedListener#doPublishConfig(String, Object)}.
     */
    @Test
    public void testPublishConfig() {
        doNothing().when(apolloClient)
                .createOrUpdateItem(Mockito.any(), Mockito.<Object>any(), Mockito.any());
        doNothing().when(apolloClient).publishNamespace(Mockito.any(), Mockito.any());
        apolloDataChangedListener.doPublishConfig("42", "Data");
        verify(apolloClient).createOrUpdateItem(Mockito.any(), Mockito.<Object>any(), Mockito.any());
        verify(apolloClient).publishNamespace(Mockito.any(), Mockito.any());
    }

    /**
     * Method under test: {@link ApolloDataChangedListener#getConfig(String)}.
     */
    @Test
    public void testGetConfig() {
        when(apolloClient.getItemValue(Mockito.any())).thenReturn("42");
        assertEquals("42", apolloDataChangedListener.getConfig("42"));
        verify(apolloClient).getItemValue(Mockito.any());
    }

    /**
     * Method under test: {@link ApolloDataChangedListener#getConfig(String)}.
     */
    @Test
    public void testGetConfig2() {
        when(apolloClient.getItemValue(Mockito.any())).thenReturn("");
        assertEquals("", apolloDataChangedListener.getConfig("42"));
        verify(apolloClient).getItemValue(Mockito.any());
    }
}

