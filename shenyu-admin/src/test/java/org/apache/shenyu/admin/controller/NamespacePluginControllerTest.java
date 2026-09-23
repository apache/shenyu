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

import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.junit.jupiter.api.Test;
import org.springframework.core.annotation.AnnotationUtils;

import java.lang.reflect.Method;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for {@link NamespacePluginController}.
 */
public final class NamespacePluginControllerTest {

    @Test
    public void shouldRequireModifyPermissionForSyncPluginData() throws NoSuchMethodException {
        assertPermissions(NamespacePluginController.class.getMethod("syncPluginData", String.class),
                "system:plugin:modify");
    }

    private void assertPermissions(final Method method, final String... expectedPermissions) {
        RequiresPermissions permissions = AnnotationUtils.findAnnotation(method, RequiresPermissions.class);
        if (Objects.isNull(permissions)) {
            permissions = AnnotationUtils.findAnnotation(method.getDeclaringClass(), RequiresPermissions.class);
        }
        assertNotNull(permissions, method.getName() + " should declare @RequiresPermissions");
        assertArrayEquals(expectedPermissions, permissions.value(), method.getName() + " should declare the expected permissions");
    }
}
