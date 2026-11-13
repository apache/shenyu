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

package org.apache.shenyu.admin.service.manager.impl;

import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okhttp3.MediaType;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Exercise upstream conversion and the actual Swagger request together.
 */
public class SwaggerUpstreamProtocolTest {

    private static final String DOCUMENT = "{\"openapi\":\"3.0.0\"}";

    @ParameterizedTest
    @CsvSource({
        "https://,example.com,https://example.com/project/v3/api-docs",
        "https://,example.com:8443,https://example.com:8443/project/v3/api-docs",
        "https,example.com,https://example.com/project/v3/api-docs",
        "http://,example.com,http://example.com/project/v3/api-docs",
        "http://,example.com:8080,http://example.com:8080/project/v3/api-docs",
        ",example.com,http://example.com/project/v3/api-docs",
        "https://,[::1]:8443,https://[::1]:8443/project/v3/api-docs"
    })
    public void testStartupScan(final String protocol, final String address, final String expectedUrl) throws IOException {
        assertSwaggerRequest(protocol, address, expectedUrl, false);
    }

    @ParameterizedTest
    @CsvSource({
        "https://,example.com,https://example.com/project/v3/api-docs",
        "https://,example.com:8443,https://example.com:8443/project/v3/api-docs",
        "https,example.com,https://example.com/project/v3/api-docs",
        "http://,example.com,http://example.com/project/v3/api-docs",
        "http://,example.com:8080,http://example.com:8080/project/v3/api-docs",
        ",example.com,http://example.com/project/v3/api-docs",
        "https://,[::1]:8443,https://[::1]:8443/project/v3/api-docs"
    })
    public void testDiscoveryEvent(final String protocol, final String address, final String expectedUrl) throws IOException {
        assertSwaggerRequest(protocol, address, expectedUrl, true);
    }

    private void assertSwaggerRequest(final String protocol, final String address, final String expectedUrl, final boolean event) throws IOException {
        SelectorService selectors = mock(SelectorService.class);
        DiscoveryUpstreamService upstreams = mock(DiscoveryUpstreamService.class);
        PluginMapper plugins = mock(PluginMapper.class);
        ShenyuDictService dictionaries = mock(ShenyuDictService.class);
        TagService tags = mock(TagService.class);
        DocManager docs = mock(DocManager.class);
        PullSwaggerDocServiceImpl pull = new PullSwaggerDocServiceImpl();
        ReflectionTestUtils.setField(pull, "tagService", tags);
        ReflectionTestUtils.setField(pull, "docManager", docs);
        ReflectionTestUtils.setField(pull, "maxSwaggerBodySize", 1024L);
        LoadServiceDocEntryImpl loader = new LoadServiceDocEntryImpl(selectors, upstreams, plugins, pull, dictionaries);
        ShenyuDictVO enabled = new ShenyuDictVO();
        enabled.setDictValue("true");
        when(dictionaries.findByDictCodeName(any(), any())).thenReturn(enabled);
        PluginDO plugin = new PluginDO();
        plugin.setId("divide-id");
        when(plugins.selectByNames(any())).thenReturn(Collections.singletonList(plugin));
        SelectorVO selector = new SelectorVO();
        selector.setId("selector-id");
        selector.setName("/project");
        selector.setEnabled(true);
        CommonPager<SelectorVO> pager = new CommonPager<>();
        pager.setDataList(event ? Collections.emptyList() : Collections.singletonList(selector));
        when(selectors.listByPage(any())).thenReturn(pager);
        DiscoveryUpstreamData upstream = new DiscoveryUpstreamData();
        upstream.setProtocol(protocol);
        upstream.setUrl(address);
        when(upstreams.findBySelectorId(selector.getId())).thenReturn(Collections.singletonList(upstream));
        HttpUtils transport = (HttpUtils) ReflectionTestUtils.getField(PullSwaggerDocServiceImpl.class, "HTTP_UTILS");
        OkHttpClient previous = (OkHttpClient) ReflectionTestUtils.getField(transport, "httpClient");
        List<String> requests = new ArrayList<>();
        OkHttpClient recordingClient = new OkHttpClient.Builder().addInterceptor(chain -> {
            requests.add(chain.request().url().toString());
            return new Response.Builder().request(chain.request()).protocol(Protocol.HTTP_1_1)
                    .code(200).message("OK").body(ResponseBody.create(MediaType.parse("application/json"), DOCUMENT)).build();
        }).build();
        transport.setHttpClient(recordingClient);
        try {
            loader.loadApiDocument();
            if (event) {
                DiscoverySyncData change = new DiscoverySyncData();
                change.setPluginName("divide");
                change.setSelectorName(selector.getName());
                change.setUpstreamDataList(Collections.singletonList(upstream));
                loader.loadDocOnUpstreamChanged(Collections.singletonList(change), DataEventTypeEnum.UPDATE);
            }
            assertEquals(Collections.singletonList(expectedUrl), requests);
            verify(docs).addDocInfo(any(), eq(DOCUMENT), any(), any());
        } finally {
            transport.setHttpClient(previous);
            recordingClient.connectionPool().evictAll();
            recordingClient.dispatcher().executorService().shutdown();
        }
    }
}
