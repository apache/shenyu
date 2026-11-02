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
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.AppAuthLifecycleService;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.apache.shenyu.admin.aspect.annotation.RestApi;

import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Controller access-contract verification: the four endpoints map one-to-one to their dedicated Shiro permissions,
 * they only delegate to the facade and pass results through, forged caller fields are rejected at binding, and input constraints hold.
 */
@ExtendWith(MockitoExtension.class)
class AppAuthLifecycleControllerAccessTest {

    @Mock
    private AppAuthLifecycleService extensionService;

    @InjectMocks
    private AppAuthLifecycleController controller;

    private final Validator validator = Validation.buildDefaultValidatorFactory().getValidator();

    @Test
    void fourEndpointsCarryDedicatedPermissions() throws Exception {
        assertPermission("apply", PostMapping.class, "/apply", "system:authen:lifecycleApply");
        assertPermission("result", GetMapping.class, "/result", "system:authen:lifecycleResult");
        assertPermission("revoke", PostMapping.class, "/revoke", "system:authen:lifecycleRevoke");
        assertPermission("replacePaths", PutMapping.class, "/replacePaths", "system:authen:lifecycleReplacePaths");
        assertEquals("/appAuth/lifecycle", AppAuthLifecycleController.class.getAnnotation(RestApi.class).value()[0]);
    }

    private void assertPermission(final String method, final Class<? extends java.lang.annotation.Annotation> mapping,
                                  final String path, final String permission) throws Exception {
        final Method declared = AppAuthLifecycleController.class.getDeclaredMethod(method,
                parameterTypes(method));
        final java.lang.annotation.Annotation mappingAnnotation =
                declared.getDeclaredAnnotation(mapping);
        assertTrue(Objects.nonNull(mappingAnnotation), method + "  is missing " + mapping.getSimpleName());
        final String[] paths = (String[]) mappingAnnotation.annotationType()
                .getDeclaredMethod("value").invoke(mappingAnnotation);
        assertEquals(path, paths[0]);
        final RequiresPermissions requires = declared.getDeclaredAnnotation(RequiresPermissions.class);
        assertTrue(Objects.nonNull(requires), method + "  is missing @RequiresPermissions");
        assertEquals(permission, requires.value()[0]);
    }

    private Class<?>[] parameterTypes(final String method) {
        switch (method) {
            case "apply":
                return new Class<?>[] {AppAuthApplyRequestDTO.class};
            case "result":
                return new Class<?>[] {AppAuthResultQueryDTO.class};
            case "revoke":
                return new Class<?>[] {AppAuthRevokeRequestDTO.class};
            default:
                return new Class<?>[] {AppAuthReplacePathsRequestDTO.class};
        }
    }

    @Test
    void delegatesOnlyToFacadeAndPassesResultThrough() {
        final ShenyuAdminResult expected = ShenyuAdminResult.success("ok");
        final AppAuthApplyRequestDTO apply = new AppAuthApplyRequestDTO();
        when(extensionService.apply(apply)).thenReturn(expected);
        assertEquals(expected, controller.apply(apply));
        verify(extensionService).apply(apply);

        final AppAuthResultQueryDTO query = new AppAuthResultQueryDTO();
        when(extensionService.result(query)).thenReturn(expected);
        assertEquals(expected, controller.result(query));
        verify(extensionService).result(query);

        final AppAuthRevokeRequestDTO revoke = new AppAuthRevokeRequestDTO();
        when(extensionService.revoke(revoke)).thenReturn(expected);
        assertEquals(expected, controller.revoke(revoke));
        verify(extensionService).revoke(revoke);

        final AppAuthReplacePathsRequestDTO replace = new AppAuthReplacePathsRequestDTO();
        when(extensionService.replacePaths(replace)).thenReturn(expected);
        assertEquals(expected, controller.replacePaths(replace));
        verify(extensionService).replacePaths(replace);
    }

    @Test
    void forgedCallerFieldsAreRejectedAtBinding() throws Exception {
        final ObjectMapper mapper = new ObjectMapper();
        final String json = "{\"namespaceId\":\"ns-demo\",\"requestId\":\"req-1\",\"appName\":\"app\","
                + "\"pathList\":[],\"caller\":\"svc-forged\",\"appKey\":\"k\",\"appSecret\":\"s\"}";
        assertThrows(Exception.class, () -> mapper.readValue(json, AppAuthApplyRequestDTO.class));
    }

    @Test
    void applyValidationRejectsBlankAndNonAsciiKeys() {
        final AppAuthApplyRequestDTO request = new AppAuthApplyRequestDTO();
        request.setNamespaceId(" ");
        request.setRequestId("rid-with-éüñ");
        request.setAppName("");
        request.setPathList(null);
        final Set<ConstraintViolation<AppAuthApplyRequestDTO>> violations = validator.validate(request);
        assertTrue(violations.size() >= 4, "blank/non-ASCII/blank appName/null pathList must all be rejected, got " + violations.size());
    }

    @Test
    void revokeValidationRequiresReasonLength() {
        final AppAuthRevokeRequestDTO request = new AppAuthRevokeRequestDTO();
        request.setNamespaceId("ns-demo");
        request.setRequestId("req-1");
        request.setReason("");
        final Set<ConstraintViolation<AppAuthRevokeRequestDTO>> violations = validator.validate(request);
        assertTrue(violations.size() >= 1);
    }

}
