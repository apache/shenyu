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

package org.apache.shenyu.client.spring.websocket.init;

import org.apache.shenyu.client.spring.websocket.annotation.ShenyuServerEndpoint;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.socket.server.standard.SpringConfigurator;

import jakarta.websocket.DeploymentException;
import jakarta.websocket.server.ServerContainer;
import jakarta.websocket.server.ServerEndpointConfig;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * Test for {@link ShenyuServerEndpointerExporter}.
 */
@ExtendWith(MockitoExtension.class)
public class ShenyuServerEndpointerExporterTest {

    @Mock
    private ServerContainer serverContainer;

    @Mock
    private MockPojoWithAnnotationClass pojoWithAnnotation;

    @Mock
    private MockPojoClass pojo;

    @InjectMocks
    private ShenyuServerEndpointerExporter exporter;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this).close();
    }

    /**
     * test for setServerContainer.
     */
    @Test
    public void setServerContainerTest() {
        exporter.setServerContainer(serverContainer);
        assertSame(serverContainer, exporter.getServerContainer());
    }

    /**
     * test for initServletContextContainer.
     */
    @Test
    public void initServletContextContainerNotNullTest() {
        exporter.setServerContainer(serverContainer);
        exporter.initServletContext(null);
        verifyNoInteractions(serverContainer);
    }

    /**
     * test for initServletContextContainer.
     */
    @Test
    public void initServletContextContainerNullTest() {
        exporter.setServerContainer(null);
    }

    /**
     * test for isContextRequired.
     */
    @Test
    public void isContextRequiredTest() {
        assertFalse(exporter.isContextRequired());
    }

    /**
     * test for registerEndpoint.
     */
    @Test
    public void registerEndpointTest() throws DeploymentException {
        exporter.registerEndpoint(pojoWithAnnotation.getClass());
        verify(serverContainer).addEndpoint(any(ServerEndpointConfig.class));
    }

    /**
     * test for registerEndpoint.
     */
    @Test
    public void registerEndpointMissingAnnotationTest() {
        assertThrows(ShenyuException.class, () -> exporter.registerEndpoint(pojo.getClass()));
        verifyNoInteractions(serverContainer);
    }

    /**
     * class for mock with annotation.
     */
    @ShenyuServerEndpoint(path = "/testPath", configurator = SpringConfigurator.class)
    private static class MockPojoWithAnnotationClass {
    }

    /**
     * class for mock not with annotation.
     */
    private static class MockPojoClass {
    }
}
