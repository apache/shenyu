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

package org.apache.shenyu.sync.data.http;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.HttpConstants;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ThreadUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.apache.shenyu.sync.data.http.refresh.DataRefreshFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * HTTP long polling implementation.
 */
@SuppressWarnings("all")
public class HttpSyncDataService implements SyncDataService, AutoCloseable {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(HttpSyncDataService.class);

    private static final AtomicBoolean RUNNING = new AtomicBoolean(false);

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

    private DataRefreshFactory factory;

    public HttpSyncDataService(final HttpConfig httpConfig, final PluginDataSubscriber pluginDataSubscriber,
                               final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers) {
        this.factory = new DataRefreshFactory(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
        this.httpConfig = httpConfig;
        this.serverList = Lists.newArrayList(Splitter.on(",").split(httpConfig.getUrl()));
        this.httpClient = createRestTemplate();
        this.start();
    }

    private RestTemplate createRestTemplate() {
        OkHttp3ClientHttpRequestFactory factory = new OkHttp3ClientHttpRequestFactory();
        factory.setConnectTimeout((int) this.connectionTimeout.toMillis());
        factory.setReadTimeout((int) HttpConstants.CLIENT_POLLING_READ_TIMEOUT);
        return new RestTemplate(factory);
    }

    private void start() {
        // It could be initialized multiple times, so you need to control that.
        if (RUNNING.compareAndSet(false, true)) {
            // fetch all group configs.
            this.fetchGroupConfig(ConfigGroupEnum.values());
            int threadSize = serverList.size();
            this.executor = new ThreadPoolExecutor(threadSize, threadSize, 60L, TimeUnit.SECONDS,
                    new LinkedBlockingQueue<>(),
                    ShenyuThreadFactory.create("http-long-polling", true));
            // start long polling, each server creates a thread to listen for changes.
            this.serverList.forEach(server -> this.executor.execute(new HttpLongPollingTask(server)));
        } else {
            LOG.info("shenyu http long polling was started, executor=[{}]", executor);
        }
    }

    private void fetchGroupConfig(final ConfigGroupEnum... groups) throws ShenyuException {
        for (int index = 0; index < this.serverList.size(); index++) {
            String server = serverList.get(index);
            try {
                this.doFetchGroupConfig(server, groups);
                break;
            } catch (ShenyuException e) {
                // no available server, throw exception.
                if (index >= serverList.size() - 1) {
                    throw e;
                }
                LOG.warn("fetch config fail, try another one: {}", serverList.get(index + 1));
            }
        }
    }

    private void doFetchGroupConfig(final String server, final ConfigGroupEnum... groups) {
        StringBuilder params = new StringBuilder();
        for (ConfigGroupEnum groupKey : groups) {
            params.append("groupKeys").append("=").append(groupKey.name()).append("&");
        }
        String url = server + "/configs/fetch?" + StringUtils.removeEnd(params.toString(), "&");
        LOG.info("request configs: [{}]", url);
        String json = null;
        try {
            json = this.httpClient.getForObject(url, String.class);
        } catch (RestClientException e) {
            String message = String.format("fetch config fail from server[%s], %s", url, e.getMessage());
            LOG.warn(message);
            throw new ShenyuException(message, e);
        }
        // update local cache
        boolean updated = this.updateCacheWithJson(json);
        if (updated) {
            LOG.info("get latest configs: [{}]", json);
            return;
        }
        // not updated. it is likely that the current config server has not been updated yet. wait a moment.
        LOG.info("The config of the server[{}] has not been updated or is out of date. Wait for 30s to listen for changes again.", server);
        ThreadUtils.sleep(TimeUnit.SECONDS, 30);
    }

    /**
     * update local cache.
     *
     * @param json the response from config server.
     * @return true: the local cache was updated. false: not updated.
     */
    private boolean updateCacheWithJson(final String json) {
        JsonObject jsonObject = GSON.fromJson(json, JsonObject.class);
        JsonObject data = jsonObject.getAsJsonObject("data");
        // if the config cache will be updated?
        return factory.executor(data);
    }

    @SuppressWarnings("unchecked")
    private void doLongPolling(final String server) {
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>(8);
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            ConfigData<?> cacheConfig = factory.cacheConfigData(group);
            if (cacheConfig != null) {
                String value = String.join(",", cacheConfig.getMd5(), String.valueOf(cacheConfig.getLastModifyTime()));
                params.put(group.name(), Lists.newArrayList(value));
            }
        }
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity httpEntity = new HttpEntity(params, headers);
        String listenerUrl = server + "/configs/listener";
        LOG.debug("request listener configs: [{}]", listenerUrl);
        JsonArray groupJson = null;
        try {
            String json = this.httpClient.postForEntity(listenerUrl, httpEntity, String.class).getBody();
            LOG.debug("listener result: [{}]", json);
            groupJson = GSON.fromJson(json, JsonObject.class).getAsJsonArray("data");
        } catch (RestClientException e) {
            String message = String.format("listener configs fail, server:[%s], %s", server, e.getMessage());
            throw new ShenyuException(message, e);
        }
        if (groupJson != null) {
            // fetch group configuration async.
            ConfigGroupEnum[] changedGroups = GSON.fromJson(groupJson, ConfigGroupEnum[].class);
            if (ArrayUtils.isNotEmpty(changedGroups)) {
                LOG.info("Group config changed: {}", Arrays.toString(changedGroups));
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
                        // print warnning LOG.
                        if (time < retryTimes) {
                            LOG.warn("Long polling failed, tried {} times, {} times left, will be suspended for a while! {}",
                                    time, retryTimes - time, e.getMessage());
                            ThreadUtils.sleep(TimeUnit.SECONDS, 5);
                            continue;
                        }
                        // print error, then suspended for a while.
                        LOG.error("Long polling failed, try again after 5 minutes!", e);
                        ThreadUtils.sleep(TimeUnit.MINUTES, 5);
                    }
                }
            }
            LOG.warn("Stop http long polling.");
        }
    }
}
