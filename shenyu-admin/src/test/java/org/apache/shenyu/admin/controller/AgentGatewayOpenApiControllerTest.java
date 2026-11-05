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

package org.apache.shenyu.admin.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Apply;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.service.openapi.AgentGatewayOpenApiService;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Explicit permission, namespace and JSON boundary checks. */
class AgentGatewayOpenApiControllerTest {

    @AfterEach
    void cleanSession() {
        SessionUtil.clean();
    }

    @Test
    void namespaceDeniedBeforeEitherServiceEntryPoint() throws Exception {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("user").userName("contributor").build());
        AgentGatewayOpenApiService service = mock(AgentGatewayOpenApiService.class);
        NamespaceUserService namespaces = mock(NamespaceUserService.class);
        when(namespaces.listNamespaceIdByUserId("user")).thenReturn(List.of("owned"));
        AgentGatewayOpenApiController controller = new AgentGatewayOpenApiController(service, namespaces);
        AgentGatewayOpenApiRequest request = new AgentGatewayOpenApiRequest("other", "rule", "source", "{}", List.of());
        // Return the established permission-denied result; throwing would be wrapped by
        // RestControllerAspect into ShenyuAdminException and lose the error code on real requests.
        ShenyuAdminResult preview = controller.preview(request);
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, preview.getCode());
        assertEquals(ShenyuResultMessage.TOKEN_HAS_NO_PERMISSION, preview.getMessage());
        ShenyuAdminResult apply = controller.apply(new Apply(request, "revision", Set.of("GET /orders"), false, true));
        assertEquals(CommonErrorCode.TOKEN_NO_PERMISSION, apply.getCode());
        assertEquals(ShenyuResultMessage.TOKEN_HAS_NO_PERMISSION, apply.getMessage());
        verify(service, never()).preview(any());
        verify(service, never()).apply(any());
        assertEquals("system:plugin:edit", AgentGatewayOpenApiController.class.getMethod("preview", AgentGatewayOpenApiRequest.class)
                .getAnnotation(RequiresPermissions.class).value()[0]);
        assertEquals("system:plugin:edit", AgentGatewayOpenApiController.class.getMethod("apply", Apply.class).getAnnotation(RequiresPermissions.class).value()[0]);
    }

    @Test
    void recordsRoundTripAsRegularJsonAndExposeDiagnostics() throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        AgentGatewayOpenApiRequest request = new AgentGatewayOpenApiRequest("ns", "rule", "source", "{\"openapi\":\"3.0.3\"}", List.of());
        assertEquals(request, mapper.readValue(mapper.writeValueAsString(request), AgentGatewayOpenApiRequest.class));
        AgentGatewayOpenApiPreview preview = new AgentGatewayOpenApiPreview("revision",
                List.of(new AgentGatewayOpenApiPreview.Item("GET /orders", "lookup", "ADD", null, "{\"description\":\"Lookup\"}", List.of())), List.of("catalog warning"));
        assertTrue(mapper.readTree(mapper.writeValueAsString(preview)).get("items").get(0).get("after").isTextual());
    }

    @Test
    void invalidInputsReturnTheRepositoryClientErrorCodeInsteadOfAnHttpError() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("1").userName("admin").build());
        AgentGatewayOpenApiService service = mock(AgentGatewayOpenApiService.class);
        when(service.preview(any())).thenThrow(new IllegalArgumentException("Preview is stale"));
        when(service.apply(any())).thenThrow(new IllegalArgumentException("Select at least one preview item"));
        AgentGatewayOpenApiController controller = new AgentGatewayOpenApiController(service, mock(NamespaceUserService.class));
        AgentGatewayOpenApiRequest request = new AgentGatewayOpenApiRequest("ns", "rule", "source", "{\"openapi\":\"3.0.3\"}", List.of());
        ShenyuAdminResult preview = controller.preview(request);
        assertEquals(400, preview.getCode());
        assertTrue(preview.getMessage().contains("Preview is stale"));
        ShenyuAdminResult apply = controller.apply(new Apply(request, "revision", Set.of("GET /orders"), false, true));
        assertEquals(400, apply.getCode());
        assertTrue(apply.getMessage().contains("Select at least one preview item"));
    }
}
