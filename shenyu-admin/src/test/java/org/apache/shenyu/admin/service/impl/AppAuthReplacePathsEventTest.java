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

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.model.entity.AuthParamDO;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

/**
 * replacePaths publishes a complete auth snapshot: the gateway Sign subscriber replaces the whole cached auth with the
 * event payload, so a paths-only change must carry the untouched application parameters (#8135).
 */
@ExtendWith(MockitoExtension.class)
class AppAuthReplacePathsEventTest {

    private static final String AUTH_ID = "auth-1";

    private static final String NAMESPACE_ID = "ns-demo";

    private static final String REQUEST_ID = "req-1";

    @Mock
    private AppAuthRequestMapper requestMapper;

    @Mock
    private AppAuthMapper appAuthMapper;

    @Mock
    private AuthParamMapper authParamMapper;

    @Mock
    private AuthPathMapper authPathMapper;

    private final List<Object> published = new ArrayList<>();

    private AppAuthTransactionService newService() {
        return new AppAuthTransactionService(requestMapper, appAuthMapper, authParamMapper, authPathMapper,
                published::add);
    }

    private void stubOwnership() {
        final AppAuthRequestDO byAuth = new AppAuthRequestDO();
        byAuth.setRequestId(REQUEST_ID);
        when(requestMapper.selectByAuthId(AUTH_ID)).thenReturn(byAuth);

        final AppAuthRequestDO locked = new AppAuthRequestDO();
        locked.setRequestId(REQUEST_ID);
        locked.setAuthId(AUTH_ID);
        locked.setState("CREATED");
        when(requestMapper.selectByKeyForUpdate("svc-a", NAMESPACE_ID, REQUEST_ID)).thenReturn(locked);

        when(appAuthMapper.selectById(AUTH_ID)).thenReturn(AppAuthDO.builder()
                .id(AUTH_ID)
                .namespaceId(NAMESPACE_ID)
                .appKey("app-key")
                .appSecret("app-secret")
                .open(true)
                .enabled(true)
                .build());

        when(authParamMapper.findByAuthId(AUTH_ID))
                .thenReturn(List.of(new AuthParamDO(AUTH_ID, "demo-app", "tenant-context")));
    }

    private void replacePaths(final List<String> pathList) {
        final AppAuthReplacePathsRequestDTO request = new AppAuthReplacePathsRequestDTO();
        request.setNamespaceId(NAMESPACE_ID);
        request.setAuthId(AUTH_ID);
        request.setPathList(pathList);
        newService().replacePaths("svc-a", request);
    }

    private List<AppAuthData> updatePayloads() {
        return published.stream()
                .map(DataChangedEvent.class::cast)
                .peek(event -> assertEquals(ConfigGroupEnum.APP_AUTH, event.getGroupKey()))
                .flatMap(event -> event.getSource().stream())
                .map(AppAuthData.class::cast)
                .collect(Collectors.toList());
    }

    @Test
    void pathReplacementCarriesExistingApplicationParameters() {
        stubOwnership();

        replacePaths(List.of("/api/new1", "/api/new2"));

        final List<AppAuthData> payloads = updatePayloads();
        assertEquals(1, payloads.size());
        final AppAuthData data = payloads.get(0);
        assertEquals(List.of(new AuthParamData("demo-app", "tenant-context")), data.getParamDataList(),
                "the untouched application parameter must survive the paths-only change");
        assertEquals(List.of("/api/new1", "/api/new2"),
                data.getPathDataList().stream().map(AuthPathData::getPath).collect(Collectors.toList()));
        assertEquals("demo-app", data.getPathDataList().get(0).getAppName());
    }

    @Test
    void clearingAllPathsAndRepeatedReplacementKeepApplicationParameters() {
        stubOwnership();

        replacePaths(List.of());
        replacePaths(List.of("/api/only"));

        final List<AppAuthData> payloads = updatePayloads();
        assertEquals(2, payloads.size());
        for (AppAuthData data : payloads) {
            assertEquals(List.of(new AuthParamData("demo-app", "tenant-context")), data.getParamDataList(),
                    "every replacement publishes the parameters, including a complete clear");
            assertEquals("app-key", data.getAppKey());
            assertEquals("app-secret", data.getAppSecret());
            assertEquals(NAMESPACE_ID, data.getNamespaceId());
            assertEquals(Boolean.TRUE, data.getOpen());
        }
        assertEquals(List.of(), payloads.get(0).getPathDataList());
        assertEquals(List.of("/api/only"),
                payloads.get(1).getPathDataList().stream().map(AuthPathData::getPath).collect(Collectors.toList()));
    }
}
