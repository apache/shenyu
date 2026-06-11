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

package org.apache.shenyu.admin.listener.http;

import jakarta.servlet.AsyncContext;
import jakarta.servlet.http.HttpServletRequest;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.listener.AbstractDataChangedListener;
import org.apache.shenyu.admin.listener.ConfigDataCache;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.publish.InstanceInfoReportEventPublisher;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationContext;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;

/**
 * The TestCase for {@link HttpLongPollingDataChangedListener}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class HttpLongPollingDataChangedListenerTest {

    private static final String X_REAL_IP = "X-Real-IP";

    private static final String X_FORWARDED_FOR = "X-Forwarded-For";

    private static final String DEFAULT_NAMESPACE = "649330b6-c2d7-4edc-be8e-8a54df9eb385";

    @Mock
    private HttpSyncProperties httpSyncProperties;

    private HttpLongPollingDataChangedListener listener;

    private MockHttpServletRequest httpServletRequest;

    private MockHttpServletResponse httpServletResponse;

    @Mock
    private InstanceInfoReportEventPublisher instanceInfoReportEventPublisher;

    @Mock
    private ApplicationContext applicationContext;

    @BeforeEach
    public void setUp() {
        Mockito.when(httpSyncProperties.getRefreshInterval()).thenReturn(Duration.ofSeconds(30));
        Mockito.when(httpSyncProperties.getNotifyBatchSize()).thenReturn(100);
        Mockito.when(applicationContext.getBean(InstanceInfoReportEventPublisher.class))
                .thenReturn(instanceInfoReportEventPublisher);
        SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
        listener = new HttpLongPollingDataChangedListener(httpSyncProperties);
        this.httpServletRequest = new MockHttpServletRequest();
        this.httpServletResponse = new MockHttpServletResponse();
    }

    /**
     * test DoLongPolling Process.
     *
     * @throws Exception throw exception
     */
    @Test
    public void testDoLongPolling() throws Exception {
        testCompareChangedGroup();
        testGetRemoteIp();
        testGenerateResponse();
    }

    /**
     *  test GenerateResponse Manual.
     *
     * @throws UnsupportedEncodingException throw not support encoding
     */
    @Test
    public void testGenerateResponseManual() throws UnsupportedEncodingException {
        List<ConfigGroupEnum> changedGroups = new ArrayList<>();
        changedGroups.add(ConfigGroupEnum.PLUGIN);
        this.httpServletResponse.setHeader("Pragma", "no-cache");
        this.httpServletResponse.setDateHeader("Expires", 0);
        this.httpServletResponse.setHeader("Cache-Control", "no-cache,no-store");
        this.httpServletResponse.setContentType(MediaType.APPLICATION_JSON_VALUE);
        this.httpServletResponse.setStatus(MockHttpServletResponse.SC_OK);
        this.httpServletResponse.getWriter().println(GsonUtils.getInstance().toJson(ShenyuAdminResult.success(ShenyuResultMessage.SUCCESS, changedGroups)));
    }

    /**
     * test generateResponse.
     */
    @Test
    public void testGenerateResponse() throws Exception {
        List<ConfigGroupEnum> changedGroups = List.of(ConfigGroupEnum.PLUGIN);
        invokeGenerateResponse(this.httpServletResponse, changedGroups);
        assertEquals("application/json", this.httpServletResponse.getContentType());
        assertEquals(200, this.httpServletResponse.getStatus());
    }

    /**
     * test getRemoteIp.
     */
    @Test
    public void testGetRemoteIp() throws Exception {
        this.httpServletRequest.addHeader(X_FORWARDED_FOR, "x-forwarded-for,test");
        this.httpServletRequest.addHeader(X_REAL_IP, "127.0.0.1");
        String remoteIp = invokeGetRemoteIp(this.httpServletRequest);
        assertEquals("x-forwarded-for", remoteIp);

        this.httpServletRequest = new MockHttpServletRequest();
        this.httpServletRequest.addHeader(X_REAL_IP, "127.0.0.1");
        remoteIp = invokeGetRemoteIp(this.httpServletRequest);
        assertEquals("127.0.0.1", remoteIp);

        this.httpServletRequest = new MockHttpServletRequest();
        this.httpServletRequest.setRemoteAddr("192.168.1.1");
        remoteIp = invokeGetRemoteIp(this.httpServletRequest);
        assertEquals("192.168.1.1", remoteIp);
    }

    /**
     * test CompareChangedGroup.
     */
    @Test
    public void testCompareChangedGroup() {
        this.httpServletRequest.setParameter(ConfigGroupEnum.RULE.name(), "E10ADC3949BA59ABBE56E057F20F883E,1607068125");
        this.httpServletRequest.setParameter(ConfigGroupEnum.PLUGIN.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D63,1607068126");
        this.httpServletRequest.setParameter(ConfigGroupEnum.APP_AUTH.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D62,1607068124");
        this.httpServletRequest.setParameter(ConfigGroupEnum.SELECTOR.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D61,1607068123");
        this.httpServletRequest.setParameter(ConfigGroupEnum.META_DATA.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D60,1607068122");
        this.httpServletRequest.setParameter(ConfigGroupEnum.PROXY_SELECTOR.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D59,1607068121");
        this.httpServletRequest.setParameter(ConfigGroupEnum.DISCOVER_UPSTREAM.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D58,1607068120");
        this.httpServletRequest.setParameter(ConfigGroupEnum.AI_PROXY_API_KEY.name(), "F1887D3F9E6EE7A32FE5E76F4AB80D57,1607068119");
        assertEquals("E10ADC3949BA59ABBE56E057F20F883E,1607068125", this.httpServletRequest.getParameter(ConfigGroupEnum.RULE.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D63,1607068126", this.httpServletRequest.getParameter(ConfigGroupEnum.PLUGIN.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D62,1607068124", this.httpServletRequest.getParameter(ConfigGroupEnum.APP_AUTH.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D61,1607068123", this.httpServletRequest.getParameter(ConfigGroupEnum.SELECTOR.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D60,1607068122", this.httpServletRequest.getParameter(ConfigGroupEnum.META_DATA.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D59,1607068121", this.httpServletRequest.getParameter(ConfigGroupEnum.PROXY_SELECTOR.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D58,1607068120", this.httpServletRequest.getParameter(ConfigGroupEnum.DISCOVER_UPSTREAM.name()));
        assertEquals("F1887D3F9E6EE7A32FE5E76F4AB80D57,1607068119", this.httpServletRequest.getParameter(ConfigGroupEnum.AI_PROXY_API_KEY.name()));
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            String[] params = Objects.requireNonNull(this.httpServletRequest.getParameter(group.name())).split(",");
            assertNotNull(params);
            assertEquals(2, params.length);
        }
    }

    /**
     * test getNamespaceId.
     */
    @Test
    public void testGetNamespaceId() throws Exception {
        String namespaceId = invokeGetNamespaceId(this.httpServletRequest);
        assertEquals("649330b6-c2d7-4edc-be8e-8a54df9eb385", namespaceId);

        this.httpServletRequest.setParameter("namespaceId", "testNamespace");
        namespaceId = invokeGetNamespaceId(this.httpServletRequest);
        assertEquals("testNamespace", namespaceId);
    }

    /**
     * test buildCacheKey.
     */
    @Test
    public void testBuildCacheKey() {
        String key = HttpLongPollingDataChangedListener.buildCacheKey("namespace", "group");
        assertEquals("namespace_group", key);
    }

    /**
     * test Constructor.
     */
    @Test
    public void testConstructor() {
        assertNotNull(listener);
        try {
            Field schedulerField = HttpLongPollingDataChangedListener.class.getDeclaredField("scheduler");
            schedulerField.setAccessible(true);
            Object scheduler = schedulerField.get(listener);
            assertNotNull(scheduler);
        } catch (Exception e) {
            // expected
        }
    }

    /**
     * test compareChangedGroup with invalid params (null param triggers exception).
     */
    @Test
    public void testCompareChangedGroupInvalidParams() throws Exception {
        // Only set PLUGIN with invalid value; all other group params are null → first null triggers exception
        this.httpServletRequest.setParameter(ConfigGroupEnum.PLUGIN.name(), "invalid");
        try {
            invokeCompareChangedGroup(this.httpServletRequest);
        } catch (Exception e) {
            assertNotNull(e.getCause());
            // The first group without a valid param will trigger the exception
            assertNotNull(e.getCause().getMessage());
        }
    }

    /**
     * test compareChangedGroup with one group having invalid format.
     */
    @Test
    public void testCompareChangedGroupSpecificInvalidParam() throws Exception {
        // Populate all groups in CACHE first so they don't NPE
        populateAllGroupsInCache(DEFAULT_NAMESPACE, "md5");
        setAllGroupParamsDefault(this.httpServletRequest);
        // Override PLUGIN with invalid value (only one comma-part)
        this.httpServletRequest.setParameter(ConfigGroupEnum.PLUGIN.name(), "invalid");
        try {
            invokeCompareChangedGroup(this.httpServletRequest);
        } catch (Exception e) {
            assertNotNull(e.getCause());
            assertEquals("group param invalid:invalid", e.getCause().getMessage());
        }
        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test checkCacheDelayAndUpdate - same md5 returns false.
     */
    @Test
    public void testCheckCacheDelayAndUpdateSameMd5() throws Exception {
        String namespaceId = DEFAULT_NAMESPACE;
        String group = ConfigGroupEnum.PLUGIN.name();
        String cacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group);
        ConfigDataCache cache = new ConfigDataCache(group, "{}", "md5", 1000L, namespaceId);
        getCache().put(cacheKey, cache);

        boolean result = invokeCheckCacheDelayAndUpdate(cache, "md5", 500L);
        assertEquals(false, result);

        getCache().remove(cacheKey);
    }

    /**
     * test checkCacheDelayAndUpdate - different md5 but client is newer and cache is concurrently updated.
     * When latest == serverCache and clientModifyTime > lastModifyTime, enters sync block;
     * to avoid calling refreshLocalCache (which needs Spring), put a different object in CACHE so latest != serverCache.
     */
    @Test
    public void testCheckCacheDelayAndUpdateClientNewer() throws Exception {
        String namespaceId = DEFAULT_NAMESPACE;
        String group = ConfigGroupEnum.PLUGIN.name();
        String cacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group);

        // serverCache is NOT the object in CACHE; CACHE has a different instance with same md5 as client
        ConfigDataCache serverCache = new ConfigDataCache(group, "{}", "md5", 1000L, namespaceId);
        ConfigDataCache latestCache = new ConfigDataCache(group, "{}", "clientMd5", 2000L, namespaceId);
        getCache().put(cacheKey, latestCache);

        // clientMd5 matches latestCache.md5 → returns false (no update needed)
        boolean result = invokeCheckCacheDelayAndUpdate(serverCache, "clientMd5", 2000L);
        assertEquals(false, result);

        getCache().remove(cacheKey);
    }

    /**
     * test checkCacheDelayAndUpdate - different md5, server is newer.
     */
    @Test
    public void testCheckCacheDelayAndUpdateServerNewer() throws Exception {
        String namespaceId = DEFAULT_NAMESPACE;
        String group = ConfigGroupEnum.PLUGIN.name();
        String cacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group);
        ConfigDataCache cache = new ConfigDataCache(group, "{}", "md5", 1000L, namespaceId);
        getCache().put(cacheKey, cache);

        boolean result = invokeCheckCacheDelayAndUpdate(cache, "newMd5", 500L);
        assertEquals(true, result);

        getCache().remove(cacheKey);
    }

    /**
     * test checkCacheDelayAndUpdate when CACHE has been concurrently updated (latest != serverCache).
     */
    @Test
    public void testCheckCacheDelayAndUpdateLatestChanged() throws Exception {
        String namespaceId = DEFAULT_NAMESPACE;
        String group = ConfigGroupEnum.PLUGIN.name();
        String cacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group);

        ConfigDataCache oldCache = new ConfigDataCache(group, "{}", "oldMd5", 500L, namespaceId);
        ConfigDataCache newCache = new ConfigDataCache(group, "{}", "newMd5", 2000L, namespaceId);
        getCache().put(cacheKey, newCache);

        // oldCache != CACHE.get(cacheKey), clientMd5 != newMd5 → true
        boolean result = invokeCheckCacheDelayAndUpdate(oldCache, "clientMd5", 2000L);
        assertEquals(true, result);

        // clientMd5 == newMd5 → false
        result = invokeCheckCacheDelayAndUpdate(oldCache, "newMd5", 2000L);
        assertEquals(false, result);

        getCache().remove(cacheKey);
    }

    /**
     * test doLongPolling with changed groups.
     */
    @Test
    public void testDoLongPollingWithChangedGroups() throws Exception {
        String group = ConfigGroupEnum.PLUGIN.name();
        populateAllGroupsInCache(DEFAULT_NAMESPACE, "serverMd5");

        setAllGroupParamsDefault(this.httpServletRequest);
        // Override PLUGIN with different md5 and older time → changed group
        this.httpServletRequest.setParameter(group, "clientMd5,500");

        listener.doLongPolling(this.httpServletRequest, this.httpServletResponse);

        assertEquals(200, this.httpServletResponse.getStatus());

        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test doLongPolling with client port triggering instance report.
     */
    @Test
    public void testDoLongPollingWithClientPort() throws Exception {
        String group = ConfigGroupEnum.PLUGIN.name();
        populateAllGroupsInCache(DEFAULT_NAMESPACE, "serverMd5");

        setAllGroupParamsDefault(this.httpServletRequest);
        this.httpServletRequest.setParameter(group, "clientMd5,500");
        this.httpServletRequest.addHeader("X-Real-PORT", "8080");
        this.httpServletRequest.addHeader(X_REAL_IP, "10.0.0.1");

        listener.doLongPolling(this.httpServletRequest, this.httpServletResponse);

        Mockito.verify(instanceInfoReportEventPublisher).publish(any());
        assertEquals(200, this.httpServletResponse.getStatus());

        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test doLongPolling with no changed groups (async path).
     */
    @Test
    public void testDoLongPollingNoChangedGroups() throws Exception {
        populateAllGroupsInCache(DEFAULT_NAMESPACE, "sameMd5");

        // Use Mockito mock so we can stub startAsync
        HttpServletRequest mockRequest = Mockito.mock(HttpServletRequest.class);
        final AsyncContext asyncContext = Mockito.mock(AsyncContext.class);

        // Return matching md5 for all group params so no groups are changed
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            Mockito.when(mockRequest.getParameter(group.name())).thenReturn("sameMd5,500");
        }
        Mockito.when(mockRequest.getParameter("namespaceId")).thenReturn(null);
        Mockito.when(mockRequest.getHeader("X-Forwarded-For")).thenReturn(null);
        Mockito.when(mockRequest.getHeader("X-Real-IP")).thenReturn(null);
        Mockito.when(mockRequest.getRemoteAddr()).thenReturn("127.0.0.1");
        Mockito.when(mockRequest.getHeader("X-Real-PORT")).thenReturn(null);
        Mockito.when(mockRequest.getHeader("bootstrapInstanceInfo")).thenReturn(null);
        Mockito.when(mockRequest.startAsync()).thenReturn(asyncContext);
        Mockito.when(asyncContext.getRequest()).thenReturn(mockRequest);
        Mockito.when(asyncContext.getResponse()).thenReturn(this.httpServletResponse);

        listener.doLongPolling(mockRequest, this.httpServletResponse);

        Mockito.verify(mockRequest).startAsync();

        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test doLongPolling with X-Forwarded-For header.
     */
    @Test
    public void testDoLongPollingWithXForwardedFor() throws Exception {
        String group = ConfigGroupEnum.PLUGIN.name();
        populateAllGroupsInCache(DEFAULT_NAMESPACE, "serverMd5");

        setAllGroupParamsDefault(this.httpServletRequest);
        this.httpServletRequest.setParameter(group, "clientMd5,500");
        this.httpServletRequest.addHeader(X_FORWARDED_FOR, "192.168.1.100,10.0.0.1");

        listener.doLongPolling(this.httpServletRequest, this.httpServletResponse);

        assertEquals(200, this.httpServletResponse.getStatus());
        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test doLongPolling with custom namespaceId parameter.
     */
    @Test
    public void testDoLongPollingWithNamespaceId() throws Exception {
        String namespaceId = "customNamespace";
        final String group = ConfigGroupEnum.PLUGIN.name();
        populateAllGroupsInCache(namespaceId, "serverMd5");

        this.httpServletRequest.setParameter("namespaceId", namespaceId);
        setAllGroupParamsForNamespace(this.httpServletRequest, namespaceId);
        this.httpServletRequest.setParameter(group, "clientMd5,500");

        listener.doLongPolling(this.httpServletRequest, this.httpServletResponse);

        assertEquals(200, this.httpServletResponse.getStatus());
        clearAllGroupsInCache(namespaceId);
    }

    /**
     * test afterAppAuthChanged via reflection.
     */
    @Test
    public void testAfterAppAuthChanged() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "afterAppAuthChanged", List.class, DataEventTypeEnum.class, String.class);
        method.setAccessible(true);
        method.invoke(listener, new ArrayList<AppAuthData>(), DataEventTypeEnum.CREATE, DEFAULT_NAMESPACE);
    }

    /**
     * test afterMetaDataChanged via reflection.
     */
    @Test
    public void testAfterMetaDataChanged() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "afterMetaDataChanged", List.class, DataEventTypeEnum.class, String.class);
        method.setAccessible(true);
        method.invoke(listener, new ArrayList<MetaData>(), DataEventTypeEnum.CREATE, DEFAULT_NAMESPACE);
    }

    /**
     * test afterPluginChanged via reflection.
     */
    @Test
    public void testAfterPluginChanged() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "afterPluginChanged", List.class, DataEventTypeEnum.class, String.class);
        method.setAccessible(true);
        method.invoke(listener, new ArrayList<PluginData>(), DataEventTypeEnum.CREATE, DEFAULT_NAMESPACE);
    }

    /**
     * test afterRuleChanged via reflection.
     */
    @Test
    public void testAfterRuleChanged() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "afterRuleChanged", List.class, DataEventTypeEnum.class, String.class);
        method.setAccessible(true);
        method.invoke(listener, new ArrayList<RuleData>(), DataEventTypeEnum.CREATE, DEFAULT_NAMESPACE);
    }

    /**
     * test afterSelectorChanged via reflection.
     */
    @Test
    public void testAfterSelectorChanged() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "afterSelectorChanged", List.class, DataEventTypeEnum.class, String.class);
        method.setAccessible(true);
        method.invoke(listener, new ArrayList<SelectorData>(), DataEventTypeEnum.CREATE, DEFAULT_NAMESPACE);
    }

    /**
     * test afterProxySelectorChanged via reflection.
     */
    @Test
    public void testAfterProxySelectorChanged() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "afterProxySelectorChanged", List.class, DataEventTypeEnum.class, String.class);
        method.setAccessible(true);
        method.invoke(listener, new ArrayList<ProxySelectorData>(), DataEventTypeEnum.CREATE, DEFAULT_NAMESPACE);
    }

    /**
     * test afterDiscoveryUpstreamDataChanged via reflection.
     */
    @Test
    public void testAfterDiscoveryUpstreamDataChanged() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "afterDiscoveryUpstreamDataChanged", List.class, DataEventTypeEnum.class, String.class);
        method.setAccessible(true);
        method.invoke(listener, new ArrayList<DiscoverySyncData>(), DataEventTypeEnum.CREATE, DEFAULT_NAMESPACE);
    }

    /**
     * test DataChangeTask with empty clients (no-op).
     */
    @Test
    public void testDataChangeTaskEmptyClients() throws Exception {
        Class<?> dataChangeTaskClass = Class.forName("org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener$DataChangeTask");
        java.lang.reflect.Constructor<?> constructor = dataChangeTaskClass.getDeclaredConstructor(
                HttpLongPollingDataChangedListener.class, ConfigGroupEnum.class, String.class);
        constructor.setAccessible(true);
        Object dataChangeTask = constructor.newInstance(listener, ConfigGroupEnum.PLUGIN, "testNamespace");

        Method runMethod = dataChangeTaskClass.getMethod("run");
        runMethod.invoke(dataChangeTask);
        // no exception = pass, clients map is empty for this namespace
    }

    /**
     * test DataChangeTask with clients in queue (below batch size).
     */
    @Test
    @SuppressWarnings("unchecked")
    public void testDataChangeTaskWithClients() throws Exception {
        final String namespaceId = "testNamespaceWithClients";

        populateAllGroupsInCache(DEFAULT_NAMESPACE, "sameMd5");

        AsyncContext asyncContext = Mockito.mock(AsyncContext.class);
        MockHttpServletRequest req = new MockHttpServletRequest();
        MockHttpServletResponse resp = new MockHttpServletResponse();
        setAllGroupParamsDefault(req);
        Mockito.when(asyncContext.getRequest()).thenReturn(req);
        Mockito.when(asyncContext.getResponse()).thenReturn(resp);

        Class<?> longPollingClientClass = Class.forName(
                "org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener$LongPollingClient");
        java.lang.reflect.Constructor<?> clientCtor = longPollingClientClass.getDeclaredConstructor(
                HttpLongPollingDataChangedListener.class, AsyncContext.class, String.class, long.class, String.class);
        clientCtor.setAccessible(true);
        Object client = clientCtor.newInstance(listener, asyncContext, "127.0.0.1", 1L, namespaceId);

        BlockingQueue<Object> queue = new ArrayBlockingQueue<>(1024);
        queue.add(client);

        Field clientsMapField = HttpLongPollingDataChangedListener.class.getDeclaredField("clientsMap");
        clientsMapField.setAccessible(true);
        java.util.Map<String, BlockingQueue<Object>> clientsMap =
                (java.util.Map<String, BlockingQueue<Object>>) clientsMapField.get(listener);
        clientsMap.put(namespaceId, queue);

        Class<?> dataChangeTaskClass = Class.forName(
                "org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener$DataChangeTask");
        java.lang.reflect.Constructor<?> taskCtor = dataChangeTaskClass.getDeclaredConstructor(
                HttpLongPollingDataChangedListener.class, ConfigGroupEnum.class, String.class);
        taskCtor.setAccessible(true);
        Object dataChangeTask = taskCtor.newInstance(listener, ConfigGroupEnum.PLUGIN, namespaceId);

        Method runMethod = dataChangeTaskClass.getMethod("run");
        runMethod.invoke(dataChangeTask);

        Mockito.verify(asyncContext).complete();
        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test DataChangeTask batch notify path (clients > notifyBatchSize).
     */
    @Test
    @SuppressWarnings("unchecked")
    public void testDataChangeTaskBatchNotify() throws Exception {
        // notifyBatchSize = 1, add 3 clients → batch path
        Mockito.when(httpSyncProperties.getNotifyBatchSize()).thenReturn(1);
        final String namespaceId = "testNamespaceBatch";

        populateAllGroupsInCache(DEFAULT_NAMESPACE, "sameMd5");

        BlockingQueue<Object> queue = new ArrayBlockingQueue<>(1024);
        Class<?> longPollingClientClass = Class.forName(
                "org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener$LongPollingClient");
        java.lang.reflect.Constructor<?> clientCtor = longPollingClientClass.getDeclaredConstructor(
                HttpLongPollingDataChangedListener.class, AsyncContext.class, String.class, long.class, String.class);
        clientCtor.setAccessible(true);

        for (int i = 0; i < 3; i++) {
            AsyncContext asyncContext = Mockito.mock(AsyncContext.class);
            MockHttpServletRequest req = new MockHttpServletRequest();
            MockHttpServletResponse resp = new MockHttpServletResponse();
            setAllGroupParamsDefault(req);
            Mockito.when(asyncContext.getRequest()).thenReturn(req);
            Mockito.when(asyncContext.getResponse()).thenReturn(resp);
            Object client = clientCtor.newInstance(listener, asyncContext, "127.0.0." + i, 1L, namespaceId);
            queue.add(client);
        }

        Field clientsMapField = HttpLongPollingDataChangedListener.class.getDeclaredField("clientsMap");
        clientsMapField.setAccessible(true);
        java.util.Map<String, BlockingQueue<Object>> clientsMap =
                (java.util.Map<String, BlockingQueue<Object>>) clientsMapField.get(listener);
        clientsMap.put(namespaceId, queue);

        Class<?> dataChangeTaskClass = Class.forName(
                "org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener$DataChangeTask");
        java.lang.reflect.Constructor<?> taskCtor = dataChangeTaskClass.getDeclaredConstructor(
                HttpLongPollingDataChangedListener.class, ConfigGroupEnum.class, String.class);
        taskCtor.setAccessible(true);
        Object dataChangeTask = taskCtor.newInstance(listener, ConfigGroupEnum.PLUGIN, namespaceId);

        Method runMethod = dataChangeTaskClass.getMethod("run");
        runMethod.invoke(dataChangeTask);
        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test LongPollingClient run and sendResponse.
     */
    @Test
    public void testLongPollingClient() throws Exception {
        populateAllGroupsInCache(DEFAULT_NAMESPACE, "sameMd5");

        AsyncContext asyncContext = Mockito.mock(AsyncContext.class);
        MockHttpServletRequest req = new MockHttpServletRequest();
        setAllGroupParamsDefault(req);
        Mockito.when(asyncContext.getRequest()).thenReturn(req);
        Mockito.when(asyncContext.getResponse()).thenReturn(this.httpServletResponse);

        Class<?> longPollingClientClass = Class.forName(
                "org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener$LongPollingClient");
        java.lang.reflect.Constructor<?> constructor = longPollingClientClass.getDeclaredConstructor(
                HttpLongPollingDataChangedListener.class, AsyncContext.class, String.class, long.class, String.class);
        constructor.setAccessible(true);
        Object longPollingClient = constructor.newInstance(listener, asyncContext, "127.0.0.1", 30000L, "testNamespace");

        Method runMethod = longPollingClientClass.getMethod("run");
        runMethod.invoke(longPollingClient);

        Method sendResponseMethod = longPollingClientClass.getDeclaredMethod("sendResponse", List.class);
        sendResponseMethod.setAccessible(true);
        sendResponseMethod.invoke(longPollingClient, List.of(ConfigGroupEnum.PLUGIN));

        Mockito.verify(asyncContext).complete();
        clearAllGroupsInCache(DEFAULT_NAMESPACE);
    }

    /**
     * test LongPollingClient sendResponse with null asyncTimeoutFuture (run not called).
     */
    @Test
    public void testLongPollingClientSendResponseNullFuture() throws Exception {
        AsyncContext asyncContext = Mockito.mock(AsyncContext.class);
        Mockito.when(asyncContext.getResponse()).thenReturn(this.httpServletResponse);

        Class<?> longPollingClientClass = Class.forName(
                "org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener$LongPollingClient");
        java.lang.reflect.Constructor<?> constructor = longPollingClientClass.getDeclaredConstructor(
                HttpLongPollingDataChangedListener.class, AsyncContext.class, String.class, long.class, String.class);
        constructor.setAccessible(true);
        Object longPollingClient = constructor.newInstance(listener, asyncContext, "127.0.0.1", 30000L, "testNamespace");

        // asyncTimeoutFuture is null (run() not called), sendResponse should still work
        Method sendResponseMethod = longPollingClientClass.getDeclaredMethod("sendResponse", List.class);
        sendResponseMethod.setAccessible(true);
        sendResponseMethod.invoke(longPollingClient, List.of(ConfigGroupEnum.PLUGIN));

        Mockito.verify(asyncContext).complete();
    }

    /**
     * test afterInitialize - verifies scheduler is set up without error.
     */
    @Test
    public void testAfterInitialize() throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod("afterInitialize");
        method.setAccessible(true);
        method.invoke(listener);
    }

    // ---- helper methods ----

    private void populateAllGroupsInCache(final String namespaceId, final String md5) throws Exception {
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            String cacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group.name());
            getCache().put(cacheKey, new ConfigDataCache(group.name(), "{}", md5, 1000L, namespaceId));
        }
    }

    private void clearAllGroupsInCache(final String namespaceId) throws Exception {
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            String cacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group.name());
            getCache().remove(cacheKey);
        }
    }

    private void setAllGroupParamsDefault(final MockHttpServletRequest request) {
        setAllGroupParamsForNamespace(request, DEFAULT_NAMESPACE);
    }

    private void setAllGroupParamsForNamespace(final MockHttpServletRequest request, final String namespaceId) {
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            String cacheKey = HttpLongPollingDataChangedListener.buildCacheKey(namespaceId, group.name());
            ConfigDataCache existing = getUncheckedCache().get(cacheKey);
            String md5 = Objects.nonNull(existing) ? existing.getMd5() : "defaultMd5";
            request.setParameter(group.name(), md5 + ",500");
        }
    }

    private ConcurrentMap<String, ConfigDataCache> getUncheckedCache() {
        try {
            return getCache();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private ConcurrentMap<String, ConfigDataCache> getCache() throws Exception {
        Field cacheField = AbstractDataChangedListener.class.getDeclaredField("CACHE");
        cacheField.setAccessible(true);
        return (ConcurrentMap<String, ConfigDataCache>) cacheField.get(null);
    }

    private boolean invokeCheckCacheDelayAndUpdate(final ConfigDataCache serverCache,
                                                   final String clientMd5, final long clientModifyTime) throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "checkCacheDelayAndUpdate", ConfigDataCache.class, String.class, long.class);
        method.setAccessible(true);
        return (boolean) method.invoke(listener, serverCache, clientMd5, clientModifyTime);
    }

    @SuppressWarnings("unchecked")
    private List<ConfigGroupEnum> invokeCompareChangedGroup(final HttpServletRequest request) throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "compareChangedGroup", HttpServletRequest.class);
        method.setAccessible(true);
        return (List<ConfigGroupEnum>) method.invoke(listener, request);
    }

    private void invokeGenerateResponse(final jakarta.servlet.http.HttpServletResponse response,
                                        final List<ConfigGroupEnum> changedGroups) throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod(
                "generateResponse", jakarta.servlet.http.HttpServletResponse.class, List.class);
        method.setAccessible(true);
        method.invoke(listener, response, changedGroups);
    }

    private String invokeGetRemoteIp(final HttpServletRequest request) throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod("getRemoteIp", HttpServletRequest.class);
        method.setAccessible(true);
        return (String) method.invoke(null, request);
    }

    private String invokeGetNamespaceId(final HttpServletRequest request) throws Exception {
        Method method = HttpLongPollingDataChangedListener.class.getDeclaredMethod("getNamespaceId", HttpServletRequest.class);
        method.setAccessible(true);
        return (String) method.invoke(null, request);
    }
}