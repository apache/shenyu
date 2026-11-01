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

import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.executable.ExecutableValidator;
import org.apache.shenyu.admin.model.dto.NamespaceDTO;
import org.apache.shiro.authz.annotation.Logical;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for NamespaceController.
 */
public final class NamespaceControllerTest {

    @Test
    public void testDeleteRejectsEmptyIds() throws NoSuchMethodException {
        final Validator validator = Validation.buildDefaultValidatorFactory().getValidator();
        final ExecutableValidator executableValidator = validator.forExecutables();
        final NamespaceController controller = new NamespaceController(null);
        final Method method = NamespaceController.class.getMethod("delete", java.util.List.class);

        assertFalse(executableValidator.validateParameters(controller, method,
                new Object[]{Collections.emptyList()}).isEmpty());
    }

    @Test
    public void testMutationsRequireActionPermissions() throws NoSuchMethodException {
        final RequiresPermissions createOrUpdate = NamespaceController.class
                .getMethod("createOrUpdate", NamespaceDTO.class)
                .getDeclaredAnnotation(RequiresPermissions.class);
        assertNotNull(createOrUpdate, "createOrUpdate should declare @RequiresPermissions");
        assertArrayEquals(new String[]{"system:namespace:add", "system:namespace:edit"}, createOrUpdate.value());
        assertEquals(Logical.OR, createOrUpdate.logical(),
                "the shared create/update endpoint accepts either action permission");

        final RequiresPermissions delete = NamespaceController.class
                .getMethod("delete", List.class)
                .getDeclaredAnnotation(RequiresPermissions.class);
        assertNotNull(delete, "delete should declare @RequiresPermissions");
        assertArrayEquals(new String[]{"system:namespace:delete"}, delete.value());
    }
}
