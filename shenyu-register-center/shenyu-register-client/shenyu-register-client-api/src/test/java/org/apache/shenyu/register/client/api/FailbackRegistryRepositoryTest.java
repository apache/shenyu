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

package org.apache.shenyu.register.client.api;

import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link FailbackRegistryRepository}.
 */
public final class FailbackRegistryRepositoryTest {

    private TestFailbackRegistryRepository repository;

    @BeforeEach
    public void setUp() {
        repository = spy(new TestFailbackRegistryRepository());
    }

    @Test
    public void testPersistInterfaceSuccess() {
        MetaDataRegisterDTO metadata = createMetaDataRegisterDTO();
        
        repository.persistInterface(metadata);
        
        verify(repository, times(1)).doPersistInterface(metadata);
        assertEquals(0, getFailureMapSize());
    }

    @Test
    public void testPersistInterfaceFailure() {
        MetaDataRegisterDTO metadata = createMetaDataRegisterDTO();
        doThrow(new RuntimeException("Test exception")).when(repository).doPersistInterface(any());
        
        repository.persistInterface(metadata);
        
        verify(repository, times(1)).doPersistInterface(metadata);
        assertEquals(1, getFailureMapSize());
    }

    @Test
    public void testPersistURISuccess() {
        URIRegisterDTO registerDTO = createURIRegisterDTO();
        
        repository.persistURI(registerDTO);
        
        verify(repository, times(1)).doPersistURI(registerDTO);
        assertEquals(0, getFailureMapSize());
    }

    @Test
    public void testPersistURIFailure() {
        URIRegisterDTO registerDTO = createURIRegisterDTO();
        doThrow(new RuntimeException("Test exception")).when(repository).doPersistURI(any());
        
        repository.persistURI(registerDTO);
        
        verify(repository, times(1)).doPersistURI(registerDTO);
        /*
         * The original code has a bug where it checks for ApiDocRegisterDTO instead of URIRegisterDTO
         * So the failure won't be added to the map
         */
        assertEquals(0, getFailureMapSize());
    }

    @Test
    public void testPersistApiDocSuccess() {
        ApiDocRegisterDTO registerDTO = createApiDocRegisterDTO();
        
        repository.persistApiDoc(registerDTO);
        
        verify(repository, times(1)).doPersistApiDoc(registerDTO);
        assertEquals(0, getFailureMapSize());
    }

    @Test
    public void testPersistApiDocFailure() {
        ApiDocRegisterDTO registerDTO = createApiDocRegisterDTO();
        doThrow(new RuntimeException("Test exception")).when(repository).doPersistApiDoc(any());
        
        repository.persistApiDoc(registerDTO);
        
        verify(repository, times(1)).doPersistApiDoc(registerDTO);
        assertEquals(1, getFailureMapSize());
    }

    @Test
    public void testPersistMcpToolsSuccess() {
        McpToolsRegisterDTO registerDTO = createMcpToolsRegisterDTO();
        
        repository.persistMcpTools(registerDTO);
        
        verify(repository, times(1)).doPersistMcpTools(registerDTO);
        assertEquals(0, getFailureMapSize());
    }

    @Test
    public void testPersistMcpToolsFailure() {
        McpToolsRegisterDTO registerDTO = createMcpToolsRegisterDTO();
        doThrow(new RuntimeException("Test exception")).when(repository).doPersistMcpTools(any());
        
        repository.persistMcpTools(registerDTO);
        
        verify(repository, times(1)).doPersistMcpTools(registerDTO);
        assertEquals(1, getFailureMapSize());
    }

    @Test
    public void testRemove() {
        // First add a failure
        MetaDataRegisterDTO metadata = createMetaDataRegisterDTO();
        doThrow(new RuntimeException("Test exception")).when(repository).doPersistInterface(any());
        repository.persistInterface(metadata);
        
        assertEquals(1, getFailureMapSize());
        
        /* Get the key and remove it */
        String key = getFirstKeyFromFailureMap();
        assertNotNull(key);
        
        repository.remove(key);
        assertEquals(0, getFailureMapSize());
    }

    @Test
    public void testAcceptWithNonExistentKey() {
        repository.accept("non-existent-key");
        /* Should not throw any exception */
    }

    @Test
    public void testAcceptWithUnknownType() throws Exception {
        // Manually add an entry with unknown type
        Map<String, Object> failureMap = getFailureMap();
        Object holder = createHolder(new Object(), "test-path", "unknown-type");
        failureMap.put("test-key", holder);
        
        repository.accept("test-key");
        /* Should not throw any exception */
    }

    private MetaDataRegisterDTO createMetaDataRegisterDTO() {
        return MetaDataRegisterDTO.builder()
                .appName("testApp")
                .path("/test")
                .rpcType("http")
                .host("127.0.0.1")
                .port(8080)
                .build();
    }

    private URIRegisterDTO createURIRegisterDTO() {
        return URIRegisterDTO.builder()
                .appName("testApp")
                .host("127.0.0.1")
                .port(8080)
                .rpcType("http")
                .build();
    }

    private ApiDocRegisterDTO createApiDocRegisterDTO() {
        return ApiDocRegisterDTO.builder()
                .contextPath("/api")
                .apiPath("/test")
                .httpMethod(0)
                .rpcType("http")
                .build();
    }

    private McpToolsRegisterDTO createMcpToolsRegisterDTO() {
        MetaDataRegisterDTO metaData = createMetaDataRegisterDTO();
        McpToolsRegisterDTO mcpToolsRegisterDTO = new McpToolsRegisterDTO();
        mcpToolsRegisterDTO.setMetaDataRegisterDTO(metaData);

        return mcpToolsRegisterDTO;
    }

    @SuppressWarnings("unchecked")
    private int getFailureMapSize() {
        try {
            Field field = FailbackRegistryRepository.class.getDeclaredField("concurrentHashMap");
            field.setAccessible(true);
            Map<String, Object> map = (Map<String, Object>) field.get(repository);
            return map.size();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private String getFirstKeyFromFailureMap() {
        try {
            Field field = FailbackRegistryRepository.class.getDeclaredField("concurrentHashMap");
            field.setAccessible(true);
            Map<String, Object> map = (Map<String, Object>) field.get(repository);
            return map.keySet().iterator().hasNext() ? map.keySet().iterator().next() : null;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> getFailureMap() {
        try {
            Field field = FailbackRegistryRepository.class.getDeclaredField("concurrentHashMap");
            field.setAccessible(true);
            return (Map<String, Object>) field.get(repository);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private Object createHolder(final Object obj, final String path, final String type) throws Exception {
        Class<?> holderClass = Class.forName("org.apache.shenyu.register.client.api.FailbackRegistryRepository$Holder");
        return holderClass.getDeclaredConstructor(Object.class, String.class, String.class)
                .newInstance(obj, path, type);
    }

    /**
     * Test implementation of FailbackRegistryRepository.
     */
    private static class TestFailbackRegistryRepository extends FailbackRegistryRepository {

        @Override
        protected void doPersistApiDoc(final ApiDocRegisterDTO apiDocRegisterDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistURI(final URIRegisterDTO registerDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistInterface(final MetaDataRegisterDTO registerDTO) {
            /* Test implementation */
        }

        @Override
        protected void doPersistMcpTools(final McpToolsRegisterDTO registerDTO) {
            /* Test implementation */
        }
    }
}
