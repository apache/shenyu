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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.NamespaceUserRelMapper;
import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.entity.NamespaceUserRelDO;
import org.apache.shenyu.admin.model.event.namespace.NamespaceCreatedEvent;
import org.apache.shenyu.admin.service.impl.NamespaceUserServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** Tests for {@link NamespaceUserServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class NamespaceUserServiceImplTest {
    @Mock
    private NamespaceUserRelMapper mapper;

    @InjectMocks
    private NamespaceUserServiceImpl service;

    @Test
    public void testCreateAndList() {
        when(mapper.selectByNamespaceIdAndUserId("n", "u")).thenReturn(null);
        assertEquals("n", service.create("n", "u").getNamespaceId());
        when(mapper.selectListByUserId("u")).thenReturn(Arrays.asList(NamespaceUserRelDO.builder().namespaceId("n1").build(),
                NamespaceUserRelDO.builder().namespaceId("n2").build()));
        assertEquals(Arrays.asList("n1", "n2"), service.listNamespaceIdByUserId("u"));
        when(mapper.selectListByUserId("empty")).thenReturn(null);
        assertEquals(Collections.emptyList(), service.listNamespaceIdByUserId("empty"));
        verify(mapper).insertSelective(any(NamespaceUserRelDO.class));
    }

    @Test
    public void testDuplicateAndNamespaceEvent() {
        when(mapper.selectByNamespaceIdAndUserId("n", "u")).thenReturn(NamespaceUserRelDO.builder().build());
        assertThrows(ShenyuAdminException.class, () -> service.create("n", "u"));
        NamespaceDO namespace = NamespaceDO.builder().namespaceId("n2").build();
        when(mapper.selectByNamespaceIdAndUserId("n2", "u2")).thenReturn(null);
        service.onNamespaceCreated(new NamespaceCreatedEvent(namespace, "u2"));
        verify(mapper).selectByNamespaceIdAndUserId("n2", "u2");
    }
}
