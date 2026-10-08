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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DiscoveryUpstreamServiceImplTest {

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    private DiscoveryUpstreamServiceImpl service;

    @BeforeEach
    void setUp() {
        service = new DiscoveryUpstreamServiceImpl(
                discoveryUpstreamMapper,
                discoveryHandlerMapper,
                proxySelectorMapper,
                discoveryMapper,
                discoveryRelMapper,
                selectorMapper,
                pluginMapper,
                discoveryProcessorHolder
        );
    }

    private DiscoveryUpstreamDTO buildDTO(final String url) {
        return buildDTO(null, url);
    }

    private DiscoveryUpstreamDTO buildDTO(final String id, final String url) {
        DiscoveryUpstreamDTO dto = new DiscoveryUpstreamDTO();
        dto.setId(id);
        dto.setDiscoveryHandlerId("handler-1");
        dto.setNamespaceId("ns-1");
        dto.setProtocol("http");
        dto.setUrl(url);
        dto.setStatus(1);
        dto.setWeight(100);
        dto.setProps("{}");
        return dto;
    }

    private void mockExactMatchReturns(final String discoveryHandlerId, final String url, final DiscoveryUpstreamDO result) {
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl(discoveryHandlerId, url)).thenReturn(result);
    }

    private void mockSelectByHandlerIdReturns(final String discoveryHandlerId, final List<DiscoveryUpstreamDO> results) {
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId(discoveryHandlerId)).thenReturn(results);
    }

    // ============ UPDATE PATH ============

    @Test
    void testUpdatePathNormalizesIPv4Url() {
        DiscoveryUpstreamDTO dto = buildDTO("existing-id", "192.168.1.1");
        service.nativeCreateOrUpdate(dto);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals("192.168.1.1:80", captor.getValue().getUpstreamUrl());
    }

    @Test
    void testUpdatePathNormalizesIPv6Url() {
        DiscoveryUpstreamDTO dto = buildDTO("existing-id", "[::1]");
        service.nativeCreateOrUpdate(dto);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals("[::1]:80", captor.getValue().getUpstreamUrl());
    }

    @Test
    void testUpdatePathNormalizesIPv6UrlWithPort() {
        DiscoveryUpstreamDTO dto = buildDTO("existing-id", "[2001:db8::1]:9090");
        service.nativeCreateOrUpdate(dto);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals("[2001:db8::1]:9090", captor.getValue().getUpstreamUrl());
    }

    // ============ INSERT PATH - EXACT MATCH ============

    @Test
    void testExactMatchSkipsInsert() {
        String normalizedUrl = "192.168.1.1:80";
        DiscoveryUpstreamDO existingDo = new DiscoveryUpstreamDO();
        existingDo.setUpstreamUrl(normalizedUrl);
        mockExactMatchReturns("handler-1", normalizedUrl, existingDo);

        DiscoveryUpstreamDTO dto = buildDTO("192.168.1.1");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper).selectByDiscoveryHandlerIdAndUrl("handler-1", normalizedUrl);
        verify(discoveryUpstreamMapper, never()).insert(any());
        verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId(any());
    }

    @Test
    void testExactMatchWithIPv6NormalizedUrlSkipsInsert() {
        String normalizedUrl = "[2001:db8::1]:80";
        DiscoveryUpstreamDO existingDo = new DiscoveryUpstreamDO();
        existingDo.setUpstreamUrl(normalizedUrl);
        mockExactMatchReturns("handler-1", normalizedUrl, existingDo);

        DiscoveryUpstreamDTO dto = buildDTO("[2001:db8::1]");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper).selectByDiscoveryHandlerIdAndUrl("handler-1", normalizedUrl);
        verify(discoveryUpstreamMapper, never()).insert(any());
        verify(discoveryUpstreamMapper, never()).selectByDiscoveryHandlerId(any());
    }

    // ============ FALLBACK MATCH (host+port) ============

    @Test
    void testFallbackMatchMigratesOldIPv4Format() {
        String normalizedUrl = "192.168.1.1:80";
        mockExactMatchReturns("handler-1", normalizedUrl, null);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("192.168.1.1");
        mockSelectByHandlerIdReturns("handler-1", Collections.singletonList(oldRecord));

        DiscoveryUpstreamDTO dto = buildDTO("192.168.1.1");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper, never()).insert(any());
        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }

    @Test
    void testFallbackMatchMigratesUnbracketedIPv6Format() {
        String normalizedUrl = "[2001:db8::1]:9090";
        mockExactMatchReturns("handler-1", normalizedUrl, null);

        /*
         * old record stored without brackets: "2001:db8::1:9090"
         * parseHostPort splits at last colon, giving host="2001:db8::1", port="9090"
         */
        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("2001:db8::1:9090");
        mockSelectByHandlerIdReturns("handler-1", Collections.singletonList(oldRecord));

        DiscoveryUpstreamDTO dto = buildDTO("[2001:db8::1]:9090");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper, never()).insert(any());
        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }

    @Test
    void testFallbackMatchAlreadyNormalizedSkipsUpdate() {
        String normalizedUrl = "[2001:db8::1]:9090";
        mockExactMatchReturns("handler-1", normalizedUrl, null);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl(normalizedUrl);
        mockSelectByHandlerIdReturns("handler-1", Collections.singletonList(oldRecord));

        DiscoveryUpstreamDTO dto = buildDTO("[2001:db8::1]:9090");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper, never()).insert(any());
        verify(discoveryUpstreamMapper, never()).updateSelective(any());
    }

    @Test
    void testFallbackMatchMigrationHandlesInvalidOldUrl() {
        String normalizedUrl = "192.168.1.1:8080";
        mockExactMatchReturns("handler-1", normalizedUrl, null);

        DiscoveryUpstreamDO badRecord = new DiscoveryUpstreamDO();
        badRecord.setId("bad-id");
        badRecord.setUpstreamUrl("not:valid::url%%%");

        DiscoveryUpstreamDO otherRecord = new DiscoveryUpstreamDO();
        otherRecord.setId("other-id");
        otherRecord.setUpstreamUrl("10.0.0.1:80");

        mockSelectByHandlerIdReturns("handler-1", Arrays.asList(badRecord, otherRecord));

        DiscoveryUpstreamDTO dto = buildDTO("192.168.1.1:8080");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper).insert(any());
        verify(discoveryUpstreamMapper, never()).updateSelective(any());
    }

    // ============ INSERT PATH - NO MATCH ============

    @Test
    void testNoMatchInsertsNewRecordWithNormalizedUrl() {
        String normalizedUrl = "192.168.1.1:80";
        mockExactMatchReturns("handler-1", normalizedUrl, null);
        mockSelectByHandlerIdReturns("handler-1", Collections.emptyList());

        DiscoveryUpstreamDTO dto = buildDTO("192.168.1.1");
        service.nativeCreateOrUpdate(dto);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).insert(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }

    @Test
    void testNoMatchInsertsIPv6WithNormalizedUrl() {
        String normalizedUrl = "[2001:db8::1]:80";
        mockExactMatchReturns("handler-1", normalizedUrl, null);
        mockSelectByHandlerIdReturns("handler-1", Collections.emptyList());

        DiscoveryUpstreamDTO dto = buildDTO("[2001:db8::1]");
        service.nativeCreateOrUpdate(dto);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).insert(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }

    @Test
    void testNoMatchInsertsIPv6WithPort() {
        String normalizedUrl = "[::1]:9090";
        mockExactMatchReturns("handler-1", normalizedUrl, null);
        mockSelectByHandlerIdReturns("handler-1", Collections.emptyList());

        DiscoveryUpstreamDTO dto = buildDTO("[::1]:9090");
        service.nativeCreateOrUpdate(dto);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).insert(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }

    // ============ FALLBACK MATCH WITH HOSTNAME ============

    @Test
    void testFallbackMatchWithHostnameAndPort() {
        String normalizedUrl = "example.com:8080";
        mockExactMatchReturns("handler-1", normalizedUrl, null);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("example.com:8080");
        mockSelectByHandlerIdReturns("handler-1", Collections.singletonList(oldRecord));

        DiscoveryUpstreamDTO dto = buildDTO("example.com:8080");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper, never()).insert(any());
        verify(discoveryUpstreamMapper, never()).updateSelective(any());
    }

    @Test
    void testFallbackMatchMigratesHostnameWithoutPort() {
        String normalizedUrl = "example.com:80";
        mockExactMatchReturns("handler-1", normalizedUrl, null);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("example.com");
        mockSelectByHandlerIdReturns("handler-1", Collections.singletonList(oldRecord));

        DiscoveryUpstreamDTO dto = buildDTO("example.com");
        service.nativeCreateOrUpdate(dto);

        verify(discoveryUpstreamMapper, never()).insert(any());
        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }
}
