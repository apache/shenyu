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

package org.apache.shenyu.register.client.beat;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.BoundServerPort;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.utils.RegisterUtils;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.servlet.context.ServletWebServerApplicationContext;
import org.springframework.boot.web.servlet.context.ServletWebServerInitializedEvent;
import org.springframework.boot.web.server.WebServer;

import java.lang.reflect.Method;
import java.net.ServerSocket;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Binds real listeners the way {@code server.port=0} and a fixed port do, then checks the reported identity.
 */
class HeartbeatBoundPortTest {

    @Test
    void reportsDistinctBoundPortsForEphemeralAndFixedListeners() throws Exception {
        final WebServer first = start(0);
        final WebServer second = start(0);
        int fixedPort;
        try (ServerSocket reserved = new ServerSocket(0)) {
            fixedPort = reserved.getLocalPort();
        }
        final WebServer fixed = start(fixedPort);
        final WebServer management = start(0);
        try (MockedConstruction<ScheduledThreadPoolExecutor> ignoredExecutors = mockConstruction(ScheduledThreadPoolExecutor.class);
                MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class);
                MockedStatic<RegisterUtils> register = mockStatic(RegisterUtils.class)) {
            register.when(() -> RegisterUtils.doLogin(anyString(), anyString(), anyString())).thenReturn(Optional.of("mock-token"));
            final HeartbeatListener ephemeral = listener();
            final HeartbeatListener other = listener();
            final HeartbeatListener explicit = listener();
            publish(ephemeral, first, null);
            publish(ephemeral, management, BoundServerPort.MANAGEMENT_NAMESPACE);
            publish(other, second, null);
            publish(explicit, fixed, null);
            final InstanceBeatInfoDTO firstBeat = report(ephemeral, register);
            final InstanceBeatInfoDTO secondBeat = report(other, register);
            final InstanceBeatInfoDTO fixedBeat = report(explicit, register);
            assertTrue(first.getPort() > 0);
            assertEquals(String.valueOf(first.getPort()), firstBeat.getInstancePort());
            assertEquals(String.valueOf(second.getPort()), secondBeat.getInstancePort());
            assertEquals(String.valueOf(fixedPort), fixedBeat.getInstancePort());
            assertNotEquals(firstBeat.getInstancePort(), secondBeat.getInstancePort());
            assertNotEquals(management.getPort(), Integer.parseInt(firstBeat.getInstancePort()));
            ephemeral.onShutdown();
            other.onShutdown();
            explicit.onShutdown();
        } finally {
            first.stop();
            second.stop();
            fixed.stop();
            management.stop();
        }
    }

    private InstanceBeatInfoDTO report(final HeartbeatListener listener, final MockedStatic<RegisterUtils> register) throws Exception {
        Method reportHeartbeat = HeartbeatListener.class.getDeclaredMethod("reportHeartbeat");
        reportHeartbeat.setAccessible(true);
        reportHeartbeat.invoke(listener);
        ArgumentCaptor<String> payload = ArgumentCaptor.forClass(String.class);
        register.verify(() -> RegisterUtils.doHeartBeat(payload.capture(), anyString(), anyString(), anyString()));
        register.clearInvocations();
        return GsonUtils.getInstance().fromJson(payload.getValue(), InstanceBeatInfoDTO.class);
    }

    private void publish(final HeartbeatListener listener, final WebServer server, final String namespace) {
        ServletWebServerApplicationContext applicationContext = mock(ServletWebServerApplicationContext.class);
        when(applicationContext.getServerNamespace()).thenReturn(namespace);
        listener.onWebServerInitialized(new ServletWebServerInitializedEvent(server, applicationContext));
    }

    private HeartbeatListener listener() {
        Properties props = new Properties();
        props.setProperty(Constants.USER_NAME, "admin");
        props.setProperty(Constants.PASS_WORD, "123456");
        ShenyuBootstrapHeartBeatConfig config = new ShenyuBootstrapHeartBeatConfig();
        config.setServerLists("http://127.0.0.1:1");
        config.setProps(props);
        ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setNamespace("shenyu");
        return new HeartbeatListener(config, shenyuConfig);
    }

    private WebServer start(final int port) {
        WebServer server = new TomcatServletWebServerFactory(port).getWebServer();
        server.start();
        return server;
    }
}
