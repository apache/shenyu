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

package org.apache.shenyu.admin.service.support;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasEntry;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for AiProxyRealKeyResolver.resolveRealKeys method.
 */
@ExtendWith(MockitoExtension.class)
public final class AiProxyRealKeyResolverTest {
    
    private static final String SELECTOR_ID_1 = "selector-1";
    
    private static final String SELECTOR_ID_2 = "selector-2";
    
    private static final String SELECTOR_ID_3 = "selector-3";
    
    private static final String SELECTOR_ID_NOT_EXIST = "selector-not-exist";
    
    private static final String API_KEY_1 = "api-key-1";
    
    private static final String API_KEY_2 = "api-key-2";
    
    private static final String HANDLE_JSON_1 = "{\"apiKey\":\"" + API_KEY_1 + "\"}";
    
    private static final String HANDLE_JSON_2 = "{\"apiKey\":\"" + API_KEY_2 + "\"}";

    @Mock
    private SelectorMapper selectorMapper;

    private ObjectMapper objectMapper;

    private AiProxyRealKeyResolver resolver;

    @BeforeEach
    public void setUp() {
        // Use real ObjectMapper for JSON parsing
        objectMapper = new ObjectMapper();
        resolver = new AiProxyRealKeyResolver(selectorMapper, objectMapper);
        // Reset cache before each test
        resolver.invalidate(SELECTOR_ID_1);
        resolver.invalidate(SELECTOR_ID_2);
        resolver.invalidate(SELECTOR_ID_3);
        resolver.invalidate(SELECTOR_ID_NOT_EXIST);
    }

    @Test
    public void testResolveRealKeysWithNullInput() {
        Map<String, String> result = resolver.resolveRealKeys(null);
        assertThat(result, is(Collections.emptyMap()));
        verify(selectorMapper, never()).selectByIdSet(any());
    }

    @Test
    public void testResolveRealKeysWithEmptyInput() {
        Map<String, String> result = resolver.resolveRealKeys(Collections.emptySet());
        assertThat(result, is(Collections.emptyMap()));
        verify(selectorMapper, never()).selectByIdSet(any());
    }

    @Test
    public void testResolveRealKeysWithCacheHit() {
        // Pre-populate cache
        SelectorDO selector1 = buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenReturn(selector1);
        resolver.resolveRealKey(SELECTOR_ID_1);

        // Now test batch resolve - should hit cache
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result, hasEntry(SELECTOR_ID_1, API_KEY_1));
        // Should not query database for cached entries
        verify(selectorMapper, never()).selectByIdSet(any());
    }

    @Test
    public void testResolveRealKeysWithCacheMiss() {
        // Test batch database query for cache misses
        SelectorDO selector1 = buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        SelectorDO selector2 = buildSelectorDO(SELECTOR_ID_2, HANDLE_JSON_2);

        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);
        selectorIds.add(SELECTOR_ID_2);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(List.of(selector1, selector2));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(2));
        assertThat(result, hasEntry(SELECTOR_ID_1, API_KEY_1));
        assertThat(result, hasEntry(SELECTOR_ID_2, API_KEY_2));
        verify(selectorMapper, times(1)).selectByIdSet(selectorIds);
    }

    @Test
    public void testResolveRealKeysWithMixedCacheHitAndMiss() {
        // Pre-populate cache for selector1
        SelectorDO selector1 = buildSelectorDO(SELECTOR_ID_1, HANDLE_JSON_1);
        when(selectorMapper.selectById(SELECTOR_ID_1)).thenReturn(selector1);
        resolver.resolveRealKey(SELECTOR_ID_1);

        // selector2 is not in cache
        SelectorDO selector2 = buildSelectorDO(SELECTOR_ID_2, HANDLE_JSON_2);
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);
        selectorIds.add(SELECTOR_ID_2);

        when(selectorMapper.selectByIdSet(Set.of(SELECTOR_ID_2))).thenReturn(List.of(selector2));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(2));
        assertThat(result, hasEntry(SELECTOR_ID_1, API_KEY_1));
        assertThat(result, hasEntry(SELECTOR_ID_2, API_KEY_2));
        // Should only query for selector2
        verify(selectorMapper, times(1)).selectByIdSet(Set.of(SELECTOR_ID_2));
    }

    @Test
    public void testResolveRealKeysWithNonExistentSelector() {
        // Test null caching for non-existent selectors
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_NOT_EXIST);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(Collections.emptyList());

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_NOT_EXIST));
        assertThat(result.get(SELECTOR_ID_NOT_EXIST), is(nullValue()));
        verify(selectorMapper, times(1)).selectByIdSet(selectorIds);

        // Verify null is cached - second call should not query database
        Map<String, String> result2 = resolver.resolveRealKeys(selectorIds);
        assertThat(result2, hasKey(SELECTOR_ID_NOT_EXIST));
        assertThat(result2.get(SELECTOR_ID_NOT_EXIST), is(nullValue()));
        verify(selectorMapper, times(1)).selectByIdSet(selectorIds);
    }

    @Test
    public void testResolveRealKeysWithSelectorHavingNullHandle() {
        // Test selector with null handle
        SelectorDO selector = buildSelectorDO(SELECTOR_ID_1, null);
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(List.of(selector));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result.get(SELECTOR_ID_1), is(nullValue()));
    }

    @Test
    public void testResolveRealKeysWithSelectorHavingInvalidHandle() {
        // Test selector with invalid JSON handle
        SelectorDO selector = buildSelectorDO(SELECTOR_ID_1, "invalid-json");
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(List.of(selector));

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result.get(SELECTOR_ID_1), is(nullValue()));
    }

    @Test
    public void testResolveRealKeysWithMultipleNonExistentSelectors() {
        // Test multiple non-existent selectors
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_NOT_EXIST);
        selectorIds.add("another-not-exist");

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(Collections.emptyList());

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(2));
        assertThat(result, hasKey(SELECTOR_ID_NOT_EXIST));
        assertThat(result, hasKey("another-not-exist"));
        assertThat(result.get(SELECTOR_ID_NOT_EXIST), is(nullValue()));
        assertThat(result.get("another-not-exist"), is(nullValue()));
    }

    @Test
    public void testResolveRealKeysWithNullReturnFromMapper() {
        // Test when mapper returns null
        Set<String> selectorIds = new HashSet<>();
        selectorIds.add(SELECTOR_ID_1);

        when(selectorMapper.selectByIdSet(selectorIds)).thenReturn(null);

        Map<String, String> result = resolver.resolveRealKeys(selectorIds);

        assertThat(result.size(), is(1));
        assertThat(result, hasKey(SELECTOR_ID_1));
        assertThat(result.get(SELECTOR_ID_1), is(nullValue()));
    }

    private SelectorDO buildSelectorDO(final String id, final String handle) {
        SelectorDO selector = new SelectorDO();
        selector.setId(id);
        selector.setHandle(handle);
        return selector;
    }
}

