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

package org.apache.shenyu.admin.service.register;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.publish.InstanceInfoReportEventPublisher;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.TarsUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ShenyuClientRegisterDivideServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClientRegisterDivideServiceImplTest {
    
    public static final String LOCALHOST = "localhost";
    
    @InjectMocks
    private ShenyuClientRegisterDivideServiceImpl shenyuClientRegisterDivideService;
    
    @Mock
    private MetaDataServiceImpl metaDataService;

    @Mock
    private SelectorService selectorService;

    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    @Mock
    private UpstreamCheckService upstreamCheckService;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private InstanceInfoReportEventPublisher publisher;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Mock
    private DiscoveryService discoveryService;

    @BeforeEach
    public void setUp() {
    }

    @Test
    public void testRpcType() {
        assertEquals(RpcTypeEnum.HTTP.getName(), shenyuClientRegisterDivideService.rpcType());
    }
    
    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        assertEquals(StringUtils.EMPTY, shenyuClientRegisterDivideService.selectorHandler(metaDataRegisterDTO));
    }
    
    @Test
    public void testRuleHandler() {
        assertEquals(new DivideRuleHandle().toJson(), shenyuClientRegisterDivideService.ruleHandler());
    }
    
    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByPathAndNamespaceId(any(), any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().registerMetaData(true).build();
        shenyuClientRegisterDivideService.registerMetadata(metaDataDTO);
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }

    @Test
    public void testBuildHandle() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);

        final String returnStr = "[{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8090',warmup:10,weight:50,status:true,timestamp:1637826588267,\"gray\":false},"
                + "{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8091',warmup:10,weight:50,status:true,timestamp:1637826588267,\"gray\":false}]";
        final String expected = "[{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8090\",\"status\":true,\"timestamp\":1637826588267,\"gray\":false},"
                + "{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"localhost:8091\",\"status\":true,\"timestamp\":1637826588267,\"gray\":false}]";
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8090).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        String actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        List<TarsUpstream> expectedList = GsonUtils.getInstance().fromCurrentList(expected, TarsUpstream.class);
        List<TarsUpstream> actualList = GsonUtils.getInstance().fromCurrentList(actual, TarsUpstream.class);
        assertEquals(expectedList, actualList);
        List<TarsUpstream> resultList = GsonUtils.getInstance().fromCurrentList(actual, TarsUpstream.class);
        assertEquals(resultList.size(), 2);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8092).build());
        selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, TarsUpstream.class);
        assertEquals(resultList.size(), 3);

        list.clear();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8090).build());
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        selectorDO = mock(SelectorDO.class);
        actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);
        resultList = GsonUtils.getInstance().fromCurrentList(actual, TarsUpstream.class);
        assertEquals(resultList.size(), 1);
    }

    /**
     * The status change of an existing upstream should be written back into the selector handle.
     * see <a href="https://github.com/apache/shenyu/issues/6522">issue #6522</a>
     */
    @Test
    public void testBuildHandleWithStatusChanged() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);

        final String returnStr = "[{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8090',warmup:10,weight:50,status:true,timestamp:1637826588267,\"gray\":false},"
                + "{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8091',warmup:10,weight:50,status:true,timestamp:1637826588267,\"gray\":false}]";

        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName())
                .host(LOCALHOST).port(8090).eventType(EventType.DELETED).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        String actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);

        List<DivideUpstream> resultList = GsonUtils.getInstance().fromCurrentList(actual, DivideUpstream.class);
        assertEquals(2, resultList.size());
        assertFalse(resultList.get(0).isStatus());
        assertTrue(resultList.get(1).isStatus());
    }

    /**
     * An offline upstream coming back online should be written back into the selector handle as well,
     * and only the re-registered one should be touched.
     * see <a href="https://github.com/apache/shenyu/issues/6522">issue #6522</a>
     */
    @Test
    public void testBuildHandleWithStatusRecovered() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);

        final String returnStr = "[{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8090',warmup:10,weight:50,status:false,timestamp:1637826588267,\"gray\":false},"
                + "{protocol:'http://',upstreamHost:'localhost',upstreamUrl:'localhost:8091',warmup:10,weight:50,status:false,timestamp:1637826588267,\"gray\":false}]";

        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName())
                .host(LOCALHOST).port(8090).build());
        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorDO.getHandle()).thenReturn(returnStr);
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());
        String actual = shenyuClientRegisterDivideService.buildHandle(list, selectorDO);

        List<DivideUpstream> resultList = GsonUtils.getInstance().fromCurrentList(actual, DivideUpstream.class);
        assertEquals(2, resultList.size());
        assertTrue(resultList.get(0).isStatus());
        assertFalse(resultList.get(1).isStatus());
    }

    @Test
    public void testBuildDivideUpstreamList() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().appName("test1").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8090).build());
        list.add(URIRegisterDTO.builder().appName("test2").rpcType(RpcTypeEnum.HTTP.getName()).host(LOCALHOST).port(8091).build());
        try {
            Method testMethod = shenyuClientRegisterDivideService.getClass().getDeclaredMethod("buildDivideUpstreamList", List.class);
            testMethod.setAccessible(true);
            List<DivideUpstream> result = (List<DivideUpstream>) testMethod.invoke(shenyuClientRegisterDivideService, list);
            assertEquals(result.size(), 2);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    /**
     * Central probe mode (the default): a heartbeat keeps the legacy behavior and writes
     * the alive status back, see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoHeartbeatCentralModeWritesAliveStatus() {
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-central").build();
        when(selectorService.findByNameAndPluginNameAndNamespaceId(anyString(), anyString(), anyString())).thenReturn(selectorDO);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(new ArrayList<>());

        String result = shenyuClientRegisterDivideService.doHeartbeat("selector-central", buildHeartbeatUriList(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        verify(discoveryUpstreamService).changeStatusBySelectorIdAndUrl("s1", "localhost:8080", Boolean.TRUE);
        verify(publisher).publish(any(InstanceInfoReportEvent.class));
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    /**
     * Central probe mode: a heartbeat still revives an explicitly disabled upstream, documenting
     * the unchanged legacy semantics, see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoHeartbeatCentralModeRevivesDisabledUpstreamAsLegacy() {
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-central").build();
        when(selectorService.findByNameAndPluginNameAndNamespaceId(anyString(), anyString(), anyString())).thenReturn(selectorDO);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(buildDisabledUpstreamDataList());

        String result = shenyuClientRegisterDivideService.doHeartbeat("selector-central", buildHeartbeatUriList(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        verify(discoveryUpstreamService).changeStatusBySelectorIdAndUrl("s1", "localhost:8080", Boolean.TRUE);
    }

    /**
     * Local probe mode: a heartbeat must not write back the upstream status on the admin's behalf,
     * while instance info reporting and the snapshot refresh still happen,
     * see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoHeartbeatLocalModeSkipsStatusWriteBack() {
        when(upstreamCheckService.isLocalMode()).thenReturn(true);
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-local").build();
        when(selectorService.findByNameAndPluginNameAndNamespaceId(anyString(), anyString(), anyString())).thenReturn(selectorDO);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(new ArrayList<>());

        String result = shenyuClientRegisterDivideService.doHeartbeat("selector-local", buildHeartbeatUriList(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        verify(discoveryUpstreamService, never()).changeStatusBySelectorIdAndUrl(anyString(), anyString(), any());
        verify(publisher).publish(any(InstanceInfoReportEvent.class));
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    /**
     * Local probe mode: a plain heartbeat must not revive an upstream that the operator explicitly
     * disabled, and the refreshed snapshot must keep carrying the disabled status,
     * see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoHeartbeatLocalModeDoesNotReviveDisabledUpstream() {
        when(upstreamCheckService.isLocalMode()).thenReturn(true);
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-local").build();
        when(selectorService.findByNameAndPluginNameAndNamespaceId(anyString(), anyString(), anyString())).thenReturn(selectorDO);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(buildDisabledUpstreamDataList());

        String result = shenyuClientRegisterDivideService.doHeartbeat("selector-local", buildHeartbeatUriList(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        verify(discoveryUpstreamService, never()).changeStatusBySelectorIdAndUrl(anyString(), anyString(), any());
        ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        DataChangedEvent event = eventCaptor.getValue();
        assertEquals(ConfigGroupEnum.DISCOVER_UPSTREAM, event.getGroupKey());
        // getEventType() is package-private on DataChangedEvent, so read the field reflectively.
        assertEquals(DataEventTypeEnum.REFRESH, ReflectionTestUtils.getField(event, "eventType"));
        List<DiscoverySyncData> source = (List<DiscoverySyncData>) event.getSource();
        assertEquals(1, source.size());
        assertEquals(1, source.get(0).getUpstreamDataList().size());
        assertEquals(1, source.get(0).getUpstreamDataList().get(0).getStatus());
    }

    /**
     * Central probe mode: re-registering an upstream that is disabled in the selector handle
     * revives it in the persisted handle, documenting the unchanged legacy behavior,
     * see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoRegisterURICentralModeKeepsLegacyBehavior() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-register").pluginId("p1")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).handle(buildHandleJson("localhost:8080", false)).build();
        stubDoRegisterURIPath(selectorDO);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(buildDisabledUpstreamDataList());
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());

        String result = shenyuClientRegisterDivideService.doRegisterURI("selector-register", buildRegisterUriList(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        ArgumentCaptor<SelectorDO> captor = ArgumentCaptor.forClass(SelectorDO.class);
        verify(selectorService).updateSelective(captor.capture());
        List<DivideUpstream> persisted = GsonUtils.getInstance().fromCurrentList(captor.getValue().getHandle(), DivideUpstream.class);
        assertEquals(1, persisted.size());
        assertTrue(persisted.get(0).isStatus());
    }

    /**
     * Local probe mode: re-registering an enabled upstream (discovery status 0) still works and
     * the persisted selector handle keeps it enabled,
     * see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoRegisterURILocalModeEnabledUpstreamUnaffected() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);
        when(upstreamCheckService.isLocalMode()).thenReturn(true);
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-register").pluginId("p1")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).handle(buildHandleJson("localhost:8080", true)).build();
        stubDoRegisterURIPath(selectorDO);
        DiscoveryUpstreamData enabled = new DiscoveryUpstreamData();
        enabled.setUrl("localhost:8080");
        enabled.setStatus(0);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(new ArrayList<>(List.of(enabled)));
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());

        String result = shenyuClientRegisterDivideService.doRegisterURI("selector-register", buildRegisterUriList(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        ArgumentCaptor<SelectorDO> captor = ArgumentCaptor.forClass(SelectorDO.class);
        verify(selectorService).updateSelective(captor.capture());
        List<DivideUpstream> persisted = GsonUtils.getInstance().fromCurrentList(captor.getValue().getHandle(), DivideUpstream.class);
        assertEquals(1, persisted.size());
        assertTrue(persisted.get(0).isStatus());
    }

    /**
     * Local probe mode: a brand-new upstream registering for the first time still follows the
     * original default and lands in the persisted selector handle as enabled,
     * see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoRegisterURILocalModeNewNodeDefaultEnabled() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);
        when(upstreamCheckService.isLocalMode()).thenReturn(true);
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-register").pluginId("p1")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).handle("[]").build();
        stubDoRegisterURIPath(selectorDO);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(new ArrayList<>());
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());

        String result = shenyuClientRegisterDivideService.doRegisterURI("selector-register", buildRegisterUriList(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        ArgumentCaptor<SelectorDO> captor = ArgumentCaptor.forClass(SelectorDO.class);
        verify(selectorService).updateSelective(captor.capture());
        List<DivideUpstream> persisted = GsonUtils.getInstance().fromCurrentList(captor.getValue().getHandle(), DivideUpstream.class);
        assertEquals(1, persisted.size());
        assertEquals("localhost:8080", persisted.get(0).getUpstreamUrl());
        assertTrue(persisted.get(0).isStatus());
    }

    /**
     * Local probe mode: a real registration dto carries a namespaceId, so an upstream
     * re-registering over a legacy handle entry without one is treated as a new node by
     * buildHandle (the identity fields differ). This documents the current merge behavior:
     * the legacy entry keeps its persisted status while the new entry is initialized enabled
     * by default, even though the discovery row is disabled. The registration path does not
     * consult discovery status, because status 1 cannot reliably distinguish an operator
     * disablement from a failed admin probe, see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    public void testDoRegisterURILocalModeNamespaceMismatchRegistersAsNewNode() {
        shenyuClientRegisterDivideService = spy(shenyuClientRegisterDivideService);
        when(upstreamCheckService.isLocalMode()).thenReturn(true);
        SelectorDO selectorDO = SelectorDO.builder().id("s1").selectorName("selector-register").pluginId("p1")
                .namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).handle(buildHandleJson("localhost:8080", false)).build();
        stubDoRegisterURIPath(selectorDO);
        when(discoveryUpstreamService.findBySelectorId("s1")).thenReturn(buildDisabledUpstreamDataList());
        doReturn(false).when(shenyuClientRegisterDivideService).doSubmit(any(), any());

        String result = shenyuClientRegisterDivideService.doRegisterURI("selector-register", buildRegisterUriListWithNamespaceId(), Constants.SYS_DEFAULT_NAMESPACE_ID);

        assertEquals(ShenyuResultMessage.SUCCESS, result);
        ArgumentCaptor<SelectorDO> captor = ArgumentCaptor.forClass(SelectorDO.class);
        verify(selectorService).updateSelective(captor.capture());
        List<DivideUpstream> persisted = GsonUtils.getInstance().fromCurrentList(captor.getValue().getHandle(), DivideUpstream.class);
        assertEquals(2, persisted.size());
        assertTrue(persisted.stream().allMatch(upstream -> "localhost:8080".equals(upstream.getUpstreamUrl())));
        // the legacy entry without namespaceId keeps its persisted disabled status.
        assertTrue(persisted.stream().filter(upstream -> Objects.isNull(upstream.getNamespaceId()))
                .noneMatch(DivideUpstream::isStatus));
        // the duplicated entry carrying the dto namespaceId is a new node and defaults to enabled.
        assertTrue(persisted.stream().filter(upstream -> Constants.SYS_DEFAULT_NAMESPACE_ID.equals(upstream.getNamespaceId()))
                .allMatch(DivideUpstream::isStatus));
    }

    private List<URIRegisterDTO> buildRegisterUriListWithNamespaceId() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName())
                .host(LOCALHOST).port(8080).namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).build());
        return list;
    }

    private String buildHandleJson(final String url, final boolean status) {
        return "[{\"weight\":50,\"warmup\":10,\"protocol\":\"http://\",\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"" + url
                + "\",\"status\":" + status + ",\"timestamp\":1637826588267,\"gray\":false}]";
    }

    private void stubDoRegisterURIPath(final SelectorDO selectorDO) {
        when(selectorService.findByNameAndPluginNameAndNamespaceId(anyString(), anyString(), anyString())).thenReturn(selectorDO);
        when(pluginMapper.selectByName(anyString())).thenReturn(PluginDO.builder().id("p1").name("divide").build());
        when(namespacePluginRelMapper.selectByPluginIdAndNamespaceId(anyString(), anyString())).thenReturn(new NamespacePluginVO());
        when(selectorService.buildByNameAndPluginNameAndNamespaceId(anyString(), anyString(), anyString())).thenReturn(new SelectorData());
        when(discoveryService.registerDefaultDiscovery(anyString(), anyString(), anyString())).thenReturn("h1");
    }

    /**
     * The uri list for re-registration tests: no namespaceId on the dto, so the upstream built from
     * it equals the upstream parsed from the selector handle json (equals compares namespaceId too).
     *
     * @return the uri register dto list
     */
    private List<URIRegisterDTO> buildRegisterUriList() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName())
                .host(LOCALHOST).port(8080).build());
        return list;
    }

    private List<URIRegisterDTO> buildHeartbeatUriList() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().protocol("http://").appName("test1").rpcType(RpcTypeEnum.HTTP.getName())
                .host(LOCALHOST).port(8080).namespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID).build());
        return list;
    }

    private List<DiscoveryUpstreamData> buildDisabledUpstreamDataList() {
        DiscoveryUpstreamData disabled = new DiscoveryUpstreamData();
        disabled.setUrl("localhost:8080");
        disabled.setStatus(1);
        List<DiscoveryUpstreamData> list = new ArrayList<>();
        list.add(disabled);
        return list;
    }
}
