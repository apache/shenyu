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

package org.apache.shenyu.admin.service.fencing;

import org.apache.shenyu.admin.config.properties.RegistrationFencingProperties;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RegistrationFencingTargetMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RegistrationFencingTargetDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.register.FencedRegisterService;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * State machine tests for {@link RegistrationFencingServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class RegistrationFencingServiceImplTest {

    private static final String DOMAIN = "domain-a";

    private static final String STARTUP_A = "6f9619ff-8b86-d011-b42d-00cf4fc964ff";

    private static final String STARTUP_B = "16fd2706-8baf-433b-82eb-8c7fada847da";

    private static final String PRINCIPAL = "user-1";

    private static final String SESSION = "b7c0a1d2-3e4f-4a5b-8c9d-0e1f2a3b4c5d";

    @Mock
    private RegistrationFencingTargetMapper targetMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private FencedRegisterService registerService;

    private RegistrationFencingServiceImpl service;

    private final AtomicReference<RegistrationFencingTargetDO> targetRef = new AtomicReference<>();

    @BeforeEach
    public void setUp() {
        RegistrationFencingProperties properties = new RegistrationFencingProperties();
        properties.setEnabled(true);
        when(registerService.rpcType()).thenReturn("http");
        service = new RegistrationFencingServiceImpl(targetMapper, selectorMapper, pluginMapper,
                discoveryUpstreamMapper, discoveryHandlerMapper, discoveryMapper, properties, List.of(registerService));
        SelectorDO selector = new SelectorDO();
        selector.setId("selector-1");
        selector.setSelectorName("/demo");
        selector.setNamespaceId("ns-1");
        selector.setPluginId("plugin-1");
        when(selectorMapper.selectById("selector-1")).thenReturn(selector);
        when(selectorMapper.selectByIdForUpdate("selector-1")).thenReturn(selector);
        PluginDO plugin = new PluginDO();
        plugin.setId("plugin-1");
        plugin.setName("divide");
        when(pluginMapper.selectById("plugin-1")).thenReturn(plugin);
        when(targetMapper.selectById(anyString())).thenAnswer(invocation -> targetRef.get());
        when(targetMapper.selectByIdForUpdate(anyString())).thenAnswer(invocation -> targetRef.get());
    }

    @Test
    public void createTargetBindsAnExistingUpstreamExactly() {
        DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setUpstreamUrl("127.0.0.1:18080");
        upstream.setNamespaceId("ns-1");
        upstream.setProtocol("http");
        when(discoveryUpstreamMapper.selectBySelectorId("selector-1")).thenReturn(List.of(upstream));
        RegistrationFencingProtocol.TargetData target = service.createTarget(targetRequest("127.0.0.1", 18080), true).data();
        assertNotNull(target);
        assertEquals("NEVER", target.state());
        assertEquals("0", target.generation());

        when(targetMapper.selectBySelectorAddress("selector-1", "127.0.0.1", 18080)).thenReturn(new RegistrationFencingTargetDO());
        assertEquals(RegistrationFencingProtocol.SOURCE_CONFLICT,
                service.createTarget(targetRequest("127.0.0.1", 18080), true).code());

        assertEquals(RegistrationFencingProtocol.SOURCE_CONFLICT,
                service.createTarget(targetRequest("127.0.0.1", 18081), true).code());
    }

    @Test
    public void createTargetCanonicalizesDnsHostBeforePersistingOwner() {
        DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setUpstreamUrl("service.example.com:18080");
        upstream.setNamespaceId("ns-1");
        upstream.setProtocol("http");
        when(discoveryUpstreamMapper.selectBySelectorId("selector-1")).thenReturn(List.of(upstream));

        RegistrationFencingProtocol.TargetRequest request = new RegistrationFencingProtocol.TargetRequest(
                DOMAIN, "ns-1", "selector-1", "http", "SERVICE.EXAMPLE.COM", 18080, PRINCIPAL);
        RegistrationFencingProtocol.TargetData target = service.createTarget(request, true).data();

        assertEquals("service.example.com", target.host());
    }

    @Test
    public void providerManagedSelectorCannotBePreProvisionedForFencing() {
        DiscoveryUpstreamDO upstream = new DiscoveryUpstreamDO();
        upstream.setUpstreamUrl("127.0.0.1:18080");
        upstream.setNamespaceId("ns-1");
        upstream.setProtocol("http");
        when(discoveryUpstreamMapper.selectBySelectorId("selector-1")).thenReturn(List.of(upstream));
        DiscoveryHandlerDO handler = new DiscoveryHandlerDO();
        handler.setDiscoveryId("discovery-1");
        when(discoveryHandlerMapper.selectBySelectorId("selector-1")).thenReturn(handler);
        DiscoveryDO discovery = new DiscoveryDO();
        discovery.setDiscoveryType("nacos");
        when(discoveryMapper.selectById("discovery-1")).thenReturn(discovery);

        assertEquals(RegistrationFencingProtocol.SOURCE_CONFLICT,
                service.createTarget(targetRequest("127.0.0.1", 18080), true).code());
    }

    @Test
    public void firstAcquireGrantsOnceAndReplaysForTheSameStartup() {
        targetRef.set(target("NEVER", 0L, null, null, PRINCIPAL, 0L));
        RegistrationFencingProtocol.SessionData first = service.acquire(acquire(STARTUP_A), PRINCIPAL).data();
        assertNotNull(first.sessionId());
        assertEquals("1", first.generation());
        assertEquals("ACTIVE", first.state());

        RegistrationFencingProtocol.SessionData replay = service.acquire(acquire(STARTUP_A), PRINCIPAL).data();
        assertEquals(first.sessionId(), replay.sessionId());
        assertEquals(first.generation(), replay.generation());
        verify(targetMapper, times(1)).update(any());
    }

    @Test
    public void anotherStartupCannotTakeAnActiveOwner() {
        targetRef.set(target("ACTIVE", 1L, STARTUP_A, SESSION, PRINCIPAL, 0L));
        assertEquals(RegistrationFencingProtocol.OWNER_CONFLICT,
                service.acquire(acquire(STARTUP_B), PRINCIPAL).code());
    }

    @Test
    public void offlineTerminatesTheSessionAndAdvancesGeneration() {
        RegistrationFencingTargetDO target = target("ACTIVE", 1L, STARTUP_A, SESSION, PRINCIPAL, 1L);
        targetRef.set(target);
        when(targetMapper.selectBySessionId(DOMAIN, SESSION)).thenReturn(target);
        when(registerService.offlineWithoutFallback(anyString(), anyList(), anyString())).thenReturn("SUCCESS");

        RegistrationFencingProtocol.Response<RegistrationFencingProtocol.MutationData> offline =
                service.mutate(mutation(SESSION, "1", "2", RegistrationFencingProtocol.OFFLINE, uri()), PRINCIPAL);
        assertEquals(RegistrationFencingProtocol.OK, offline.code());
        assertEquals("RELEASED", target.getState());
        assertNull(target.getOwnerStartupId());
        assertEquals(2L, target.getGeneration());

        assertEquals(RegistrationFencingProtocol.SESSION_RELEASED,
                service.mutate(mutation(SESSION, "1", "3", RegistrationFencingProtocol.REGISTER, uri()), PRINCIPAL).code());
        assertEquals(RegistrationFencingProtocol.CAS_MISMATCH,
                service.acquire(acquire(STARTUP_A), PRINCIPAL).code());
    }

    @Test
    public void sequenceRulesReplayConflictGapAndObsolete() {
        RegistrationFencingTargetDO target = target("ACTIVE", 1L, STARTUP_A, SESSION, PRINCIPAL, 2L);
        URIRegisterDTO payload = uri();
        target.setLastPayload(RegistrationFencingProtocol.REGISTER + ":" + GsonUtils.getInstance().toJson(payload));
        target.setLastResult("SUCCESS");
        targetRef.set(target);
        when(targetMapper.selectBySessionId(DOMAIN, SESSION)).thenReturn(target);
        when(registerService.registerURIWithoutFallback(anyString(), anyList(), anyString())).thenReturn("SUCCESS");

        assertEquals("SUCCESS", service.mutate(
                mutation(SESSION, "1", "2", RegistrationFencingProtocol.REGISTER, payload), PRINCIPAL).data().result());
        assertEquals(RegistrationFencingProtocol.SEQUENCE_CONFLICT, service.mutate(
                mutation(SESSION, "1", "2", RegistrationFencingProtocol.HEARTBEAT, uri()), PRINCIPAL).code());
        assertEquals(RegistrationFencingProtocol.OBSOLETE_OPERATION, service.mutate(
                mutation(SESSION, "1", "1", RegistrationFencingProtocol.REGISTER, payload), PRINCIPAL).code());
        assertEquals(RegistrationFencingProtocol.SEQUENCE_GAP, service.mutate(
                mutation(SESSION, "1", "4", RegistrationFencingProtocol.REGISTER, payload), PRINCIPAL).code());
        assertEquals(RegistrationFencingProtocol.OK, service.mutate(
                mutation(SESSION, "1", "3", RegistrationFencingProtocol.REGISTER, payload), PRINCIPAL).code());
        assertEquals(3L, target.getLastSequence());
    }

    @Test
    public void mutationCannotChangeTheProvisionedProtocol() {
        RegistrationFencingTargetDO target = target("ACTIVE", 1L, STARTUP_A, SESSION, PRINCIPAL, 0L);
        targetRef.set(target);
        when(targetMapper.selectBySessionId(DOMAIN, SESSION)).thenReturn(target);
        URIRegisterDTO payload = uri();
        payload.setProtocol("grpc");

        assertEquals(RegistrationFencingProtocol.SOURCE_CONFLICT, service.mutate(
                mutation(SESSION, "1", "1", RegistrationFencingProtocol.REGISTER, payload), PRINCIPAL).code());
        verify(registerService, never()).registerURIWithoutFallback(anyString(), anyList(), anyString());
    }

    @Test
    public void mutationRejectsPayloadForAnotherServiceOnTheSameAddress() {
        RegistrationFencingTargetDO target = target("ACTIVE", 1L, STARTUP_A, SESSION, PRINCIPAL, 0L);
        targetRef.set(target);
        when(targetMapper.selectBySessionId(DOMAIN, SESSION)).thenReturn(target);
        URIRegisterDTO payload = uri();
        payload.setAppName("another-service");

        assertEquals(RegistrationFencingProtocol.SOURCE_CONFLICT, service.mutate(
                mutation(SESSION, "1", "1", RegistrationFencingProtocol.REGISTER, payload), PRINCIPAL).code());
        verify(registerService, never()).registerURIWithoutFallback(anyString(), anyList(), anyString());
    }

    @Test
    public void replacementRequiresMatchingCasAndIsIdempotentByRequestId() {
        RegistrationFencingTargetDO target = target("ACTIVE", 1L, STARTUP_A, SESSION, PRINCIPAL, 0L);
        targetRef.set(target);

        RegistrationFencingProtocol.ReplaceRequest mismatch = new RegistrationFencingProtocol.ReplaceRequest(
                "9d3d8f7e-9f2a-4a1e-8f2b-2c3d4e5f6a7b", STARTUP_A, "0", STARTUP_B, PRINCIPAL);
        assertEquals(RegistrationFencingProtocol.CAS_MISMATCH, service.replace("target-1", mismatch, true).code());

        RegistrationFencingProtocol.ReplaceRequest request = new RegistrationFencingProtocol.ReplaceRequest(
                "1a2b3c4d-5e6f-4a7b-8c9d-0e1f2a3b4c5d", STARTUP_A, "1", STARTUP_B, PRINCIPAL);
        RegistrationFencingProtocol.SessionData replaced = service.replace("target-1", request, true).data();
        assertEquals(STARTUP_B, replaced.startupId());
        assertEquals("2", replaced.generation());
        service.replace("target-1", request, true);
        verify(targetMapper, times(1)).update(any());
    }

    @Test
    public void replacementRejectsRequestIdReuseWithDifferentIntent() {
        RegistrationFencingTargetDO target = target("ACTIVE", 1L, STARTUP_A, SESSION, PRINCIPAL, 0L);
        targetRef.set(target);
        RegistrationFencingProtocol.ReplaceRequest request = new RegistrationFencingProtocol.ReplaceRequest(
                "1a2b3c4d-5e6f-4a7b-8c9d-0e1f2a3b4c5d", STARTUP_A, "1", STARTUP_B, PRINCIPAL);
        assertEquals(RegistrationFencingProtocol.OK, service.replace("target-1", request, true).code());
        RegistrationFencingProtocol.ReplaceRequest changed = new RegistrationFencingProtocol.ReplaceRequest(
                request.requestId(), STARTUP_B, "2", STARTUP_A, PRINCIPAL);
        assertEquals(RegistrationFencingProtocol.IDEMPOTENCY_CONFLICT,
                service.replace("target-1", changed, true).code());
        verify(targetMapper, times(1)).update(any());
    }

    private RegistrationFencingProtocol.TargetRequest targetRequest(final String host, final int port) {
        return new RegistrationFencingProtocol.TargetRequest(DOMAIN, "ns-1", "selector-1", "http", host, port, PRINCIPAL);
    }

    private RegistrationFencingProtocol.AcquireRequest acquire(final String startupId) {
        return new RegistrationFencingProtocol.AcquireRequest(DOMAIN, "target-1", startupId, "0");
    }

    private RegistrationFencingProtocol.MutationRequest mutation(final String sessionId, final String generation,
                                                                final String sequence, final String operation,
                                                                final URIRegisterDTO payload) {
        return new RegistrationFencingProtocol.MutationRequest(DOMAIN, sessionId, "selector-1", generation, sequence, operation, payload);
    }

    private URIRegisterDTO uri() {
        return URIRegisterDTO.builder().appName("demo").rpcType("http").protocol("http")
                .host("127.0.0.1").port(18080).namespaceId("ns-1").build();
    }

    private RegistrationFencingTargetDO target(final String state, final long generation, final String startupId,
                                               final String sessionId, final String principalId, final long lastSequence) {
        RegistrationFencingTargetDO target = new RegistrationFencingTargetDO();
        target.setId("target-1");
        target.setDomainId(DOMAIN);
        target.setNamespaceId("ns-1");
        target.setSelectorId("selector-1");
        target.setPluginId("plugin-1");
        target.setProtocol("http");
        target.setHost("127.0.0.1");
        target.setPort(18080);
        target.setPrincipalId(principalId);
        target.setEnabled(Boolean.TRUE);
        target.setState(state);
        target.setGeneration(generation);
        target.setOwnerStartupId(startupId);
        target.setSessionId(sessionId);
        target.setLastSequence(lastSequence);
        return target;
    }
}
