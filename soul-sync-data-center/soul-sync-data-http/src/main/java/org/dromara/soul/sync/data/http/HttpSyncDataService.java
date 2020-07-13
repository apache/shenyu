/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.sync.data.http;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.constant.HttpConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.ThreadUtils;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.sync.data.api.SyncDataService;
import org.dromara.soul.sync.data.http.config.HttpConfig;
import org.dromara.soul.sync.data.http.handler.HttpSyncDataHandler;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.OkHttp3ClientHttpRequestFactory;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * HTTP long polling implementation.
 *
 * @author huangxiaofeng
 * @author xiaoyu
 */
@SuppressWarnings("all")
@Slf4j
public class HttpSyncDataService extends HttpSyncDataHandler implements SyncDataService, AutoCloseable {
    
    private static final AtomicBoolean RUNNING = new AtomicBoolean(false);
    
    /**
     * cache group config with md5 info.
     */
    private static final ConcurrentMap<ConfigGroupEnum, ConfigData> GROUP_CACHE = new ConcurrentHashMap<>();
    
    private static final Gson GSON = new Gson();
    
    /**
     * default: 10s.
     */
    private Duration connectionTimeout = Duration.ofSeconds(10);
    
    /**
     * only use for http long polling.
     */
    private RestTemplate httpClient;
    
    private ExecutorService executor;
    
    private HttpConfig httpConfig;
    
    private List<String> serverList;
    
    public HttpSyncDataService(final HttpConfig httpConfig, final PluginDataSubscriber pluginDataSubscriber,
                               final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
        this.httpConfig = httpConfig;
        this.serverList = Lists.newArrayList(Splitter.on(",").split(httpConfig.getUrl()));
        this.start(httpConfig);
    }
    
    private void start(final HttpConfig httpConfig) {

        // init RestTemplate
        OkHttp3ClientHttpRequestFactory factory = new OkHttp3ClientHttpRequestFactory();
        factory.setConnectTimeout((int) this.connectionTimeout.toMillis());
        factory.setReadTimeout((int) HttpConstants.CLIENT_POLLING_READ_TIMEOUT);
        this.httpClient = new RestTemplate(factory);

        // It could be initialized multiple times, so you need to control that.
        if (RUNNING.compareAndSet(false, true)) {
            // fetch all group configs.
            this.fetchGroupConfig(ConfigGroupEnum.values());
            int threadSize = serverList.size();
            this.executor = new ThreadPoolExecutor(threadSize, threadSize, 60L, TimeUnit.SECONDS,
                    new LinkedBlockingQueue<>(),
                    SoulThreadFactory.create("http-long-polling", true));
            // start long polling, each server creates a thread to listen for changes.
            this.serverList.forEach(server -> this.executor.execute(new HttpLongPollingTask(server)));
        } else {
            log.info("soul http long polling was started, executor=[{}]", executor);
        }
    }
    
    private void fetchGroupConfig(final ConfigGroupEnum... groups) throws SoulException {
        for (int index = 0; index < this.serverList.size(); index++) {
            String server = serverList.get(index);
            try {
                this.doFetchGroupConfig(server, groups);
                break;
            } catch (SoulException e) {
                // no available server, throw exception.
                if (index >= serverList.size() - 1) {
                    throw e;
                }
                log.warn("fetch config fail, try another one: {}", serverList.get(index + 1));
            }
        }
    }

    private void doFetchGroupConfig(final String server, final ConfigGroupEnum... groups) {

        StringBuilder params = new StringBuilder();
        for (ConfigGroupEnum groupKey : groups) {
            params.append("groupKeys").append("=").append(groupKey.name()).append("&");
        }

        String url = server + "/configs/fetch?" + StringUtils.removeEnd(params.toString(), "&");
        log.info("request configs: [{}]", url);
        String json = null;
        try {
            json = this.httpClient.getForObject(url, String.class);
        } catch (RestClientException e) {
            String message = String.format("fetch config fail from server[%s], %s", url, e.getMessage());
            log.warn(message);
            throw new SoulException(message, e);
        }

        // update local cache
        boolean updated = this.updateCacheWithJson(json);
        if (updated) {
            log.info("get latest configs: [{}]", json);
            return;
        }

        // not updated. it is likely that the current config server has not been updated yet. wait a moment.
        log.info("The config of the server[{}] has not been updated or is out of date. Wait for 30s to listen for changes again.", server);
        ThreadUtils.sleep(TimeUnit.SECONDS, 30);

    }

    /**
     * If the MD5 values are different and the last update time of the old data is less than
     * the last update time of the new data, the configuration cache is considered to have been changed.
     * @param newVal the lasted config
     * @param groupEnum the group enum
     * @return true: if need update
     */
    private <T> boolean updateCacheIfNeed(final ConfigData<T> newVal, final ConfigGroupEnum groupEnum) {
        // first init cache
        if (GROUP_CACHE.putIfAbsent(groupEnum, newVal) == null) {
            return true;
        }
        ResultHolder holder = new ResultHolder(false);
        GROUP_CACHE.merge(groupEnum, newVal, (oldVal, value) -> {
            // must compare the last update time
            if (oldVal == null || (!StringUtils.equals(oldVal.getMd5(), newVal.getMd5())
                    && oldVal.getLastModifyTime() < newVal.getLastModifyTime())) {
                log.info("update {} config: {}", groupEnum, newVal);
                holder.result = true;
                return newVal;
            }
            log.info("Get the same config, the [{}] config cache will not be updated, md5:{}", groupEnum, oldVal.getMd5());
            return oldVal;
        });
        return holder.result;
    }

    /**
     * update local cache.
     * @param json the response from config server.
     * @return true: the local cache was updated. false: not updated.
     */
    private boolean updateCacheWithJson(final String json) {

        JsonObject jsonObject = GSON.fromJson(json, JsonObject.class);
        JsonObject data = jsonObject.getAsJsonObject("data");

        // if the config cache will be updated?
        boolean updated = false;

        // plugin
        JsonObject pluginData = data.getAsJsonObject(ConfigGroupEnum.PLUGIN.name());
        if (pluginData != null) {
            ConfigData<PluginData> result = GSON.fromJson(pluginData, new TypeToken<ConfigData<PluginData>>() {
            }.getType());
            if (this.updateCacheIfNeed(result, ConfigGroupEnum.PLUGIN)) {
                updated = true;
                this.flushAllPlugin(result.getData());
            }
        }

        // rule
        JsonObject ruleData = data.getAsJsonObject(ConfigGroupEnum.RULE.name());
        if (ruleData != null) {
            ConfigData<RuleData> result = GSON.fromJson(ruleData, new TypeToken<ConfigData<RuleData>>() {
            }.getType());
            if (this.updateCacheIfNeed(result, ConfigGroupEnum.RULE)) {
                updated = true;
                this.flushAllRule(result.getData());
            }
        }

        // selector
        JsonObject selectorData = data.getAsJsonObject(ConfigGroupEnum.SELECTOR.name());
        if (selectorData != null) {
            ConfigData<SelectorData> result = GSON.fromJson(selectorData, new TypeToken<ConfigData<SelectorData>>() {
            }.getType());
            if (this.updateCacheIfNeed(result, ConfigGroupEnum.SELECTOR)) {
                updated = true;
                this.flushAllSelector(result.getData());
            }
        }

        // appAuth
        JsonObject appAuthData = data.getAsJsonObject(ConfigGroupEnum.APP_AUTH.name());
        if (appAuthData != null) {
            ConfigData<AppAuthData> result = GSON.fromJson(appAuthData, new TypeToken<ConfigData<AppAuthData>>() {
            }.getType());
            if (this.updateCacheIfNeed(result, ConfigGroupEnum.APP_AUTH)) {
                updated = true;
                this.flushAllAppAuth(result.getData());
            }
        }

        // metaData
        JsonObject metaData = data.getAsJsonObject(ConfigGroupEnum.META_DATA.name());
        if (metaData != null) {
            ConfigData<MetaData> result = GSON.fromJson(metaData, new TypeToken<ConfigData<MetaData>>() {
            }.getType());
            if (this.updateCacheIfNeed(result, ConfigGroupEnum.META_DATA)) {
                updated = true;
                this.flushMetaData(result.getData());
            }
        }

        return updated;
    }
    
    @SuppressWarnings("unchecked")
    private void doLongPolling(final String server) {

        MultiValueMap<String, String> params = new LinkedMultiValueMap<>(8);
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            ConfigData<?> cacheConfig = GROUP_CACHE.get(group);
            String value = String.join(",", cacheConfig.getMd5(), String.valueOf(cacheConfig.getLastModifyTime()));
            params.put(group.name(), Lists.newArrayList(value));
        }
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity httpEntity = new HttpEntity(params, headers);

        String listenerUrl = server + "/configs/listener";
        log.debug("request listener configs: [{}]", listenerUrl);
        JsonArray groupJson = null;
        try {
            String json = this.httpClient.postForEntity(listenerUrl, httpEntity, String.class).getBody();
            log.debug("listener result: [{}]", json);
            groupJson = GSON.fromJson(json, JsonObject.class).getAsJsonArray("data");
        } catch (RestClientException e) {
            String message = String.format("listener configs fail, server:[%s], %s", server, e.getMessage());
            throw new SoulException(message, e);
        }

        if (groupJson != null) {
            // fetch group configuration async.
            ConfigGroupEnum[] changedGroups = GSON.fromJson(groupJson, ConfigGroupEnum[].class);
            if (ArrayUtils.isNotEmpty(changedGroups)) {
                log.info("Group config changed: {}", Arrays.toString(changedGroups));
                this.doFetchGroupConfig(server, changedGroups);
            }
        }
    }
    
    @Override
    public void close() throws Exception {
        RUNNING.set(false);
        if (executor != null) {
            executor.shutdownNow();
            // help gc
            executor = null;
        }
    }
    
    class HttpLongPollingTask implements Runnable {

        private String server;

        private final int retryTimes = 3;

        HttpLongPollingTask(final String server) {
            this.server = server;
        }

        @Override
        public void run() {
            while (RUNNING.get()) {
                for (int time = 1; time <= retryTimes; time++) {
                    try {
                        doLongPolling(server);
                    } catch (Exception e) {
                        // print warnning log.
                        if (time < retryTimes) {
                            log.warn("Long polling failed, tried {} times, {} times left, will be suspended for a while! {}",
                                    time, retryTimes - time, e.getMessage());
                            ThreadUtils.sleep(TimeUnit.SECONDS, 5);
                            continue;
                        }
                        // print error, then suspended for a while.
                        log.error("Long polling failed, try again after 5 minutes!", e);
                        ThreadUtils.sleep(TimeUnit.MINUTES, 5);
                    }
                }
            }
            log.warn("Stop http long polling.");
        }
    }

    private static final class ResultHolder {

        private boolean result;

        ResultHolder(final boolean result) {
            this.result = result;
        }
    }

}
