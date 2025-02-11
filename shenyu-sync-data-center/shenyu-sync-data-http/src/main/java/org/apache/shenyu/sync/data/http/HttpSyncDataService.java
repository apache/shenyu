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
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import okhttp3.Headers;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConfigData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.ThreadUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.apache.shenyu.sync.data.http.refresh.DataRefreshFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * HTTP long polling implementation.
 */
public class HttpSyncDataService implements SyncDataService {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(HttpSyncDataService.class);

    private static final AtomicBoolean RUNNING = new AtomicBoolean(false);

    private ExecutorService executor;

    private final List<String> serverList;

    private final DataRefreshFactory factory;

    private final AccessTokenManager accessTokenManager;

    private final OkHttpClient okHttpClient;

    private final ShenyuConfig shenyuConfig;

    public HttpSyncDataService(final HttpConfig httpConfig,
                               final PluginDataSubscriber pluginDataSubscriber,
                               final OkHttpClient okHttpClient,
                               final List<MetaDataSubscriber> metaDataSubscribers,
                               final List<AuthDataSubscriber> authDataSubscribers,
                               final List<ProxySelectorDataSubscriber> proxySelectorDataSubscribers,
                               final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers,
                               final AccessTokenManager accessTokenManager,
                               final ShenyuConfig shenyuConfig) {
        this.accessTokenManager = accessTokenManager;
        this.factory = new DataRefreshFactory(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers, proxySelectorDataSubscribers, discoveryUpstreamDataSubscribers);
        this.serverList = Lists.newArrayList(Splitter.on(",").split(httpConfig.getUrl()));
        this.okHttpClient = okHttpClient;
        this.shenyuConfig = shenyuConfig;
        this.start();
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
        params.append("namespaceId").append("=").append(shenyuConfig.getNamespace());
        String url = server + Constants.SHENYU_ADMIN_PATH_CONFIGS_FETCH + "?" + StringUtils.removeEnd(params.toString(), "&");
        LOG.info("request configs: [{}]", url);
        String json;
        Request request = new Request.Builder().url(url)
                .addHeader(Constants.X_ACCESS_TOKEN, this.accessTokenManager.getAccessToken())
                .get()
                .build();
        try (Response response = okHttpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                String message = String.format("fetch config fail from server[%s], http status code[%s]", url, response.code());
                LOG.warn(message);
                throw new ShenyuException(message);
            }
            ResponseBody responseBody = response.body();
            Assert.notNull(responseBody, "Resolve response responseBody failed.");
            json = responseBody.string();
        } catch (IOException e) {
            String message = String.format("fetch config fail from server[%s], %s", url, e.getMessage());
            LOG.warn(message);
            throw new ShenyuException(message, e);
        }
        // update local cache
        boolean updated = this.updateCacheWithJson(json);
        if (updated) {
            LOG.debug("get latest configs: [{}]", json);
            return;
        }
        // not updated. it is likely that the current config server has not been updated yet. wait a moment.
        LOG.info("The config of the server[{}] has not been updated or is out of date. Wait for listening for changes again.", server);
        ThreadUtils.sleep(TimeUnit.SECONDS, 5);
    }


    /**
     * update local cache.
     *
     * @param json the response from config server.
     * @return true: the local cache was updated. false: not updated.
     */
    private boolean updateCacheWithJson(final String json) {
        JsonObject jsonObject = GsonUtils.getGson().fromJson(json, JsonObject.class);
        // if the config cache will be updated?
        return factory.executor(jsonObject.getAsJsonObject("data"));
    }

    private void doLongPolling(final String server) {
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>(8);
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            ConfigData<?> cacheConfig = factory.cacheConfigData(group);
            if (Objects.nonNull(cacheConfig)) {
                String value = String.join(",", cacheConfig.getMd5(), String.valueOf(cacheConfig.getLastModifyTime()));
                params.put(group.name(), Lists.newArrayList(value));
            }
        }
        params.put("namespaceId", Lists.newArrayList(shenyuConfig.getNamespace()));
        LOG.debug("listener params: [{}]", params);
        Headers headers = new Headers.Builder()
                .add(Constants.X_ACCESS_TOKEN, this.accessTokenManager.getAccessToken())
                .add("Content-Type", "application/x-www-form-urlencoded")
                .build();
        String listenerUrl = server + Constants.SHENYU_ADMIN_PATH_CONFIGS_LISTENER;
        String uri = UriComponentsBuilder.fromHttpUrl(listenerUrl).queryParams(params).build(true).toUriString();
        Request request = new Request.Builder()
                .url(uri)
                .headers(headers)
                .post(RequestBody.create("", null))
                .build();

        JsonArray groupJson;
        try (Response response = okHttpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                String message = String.format("listener configs fail, server:[%s], http status code[%s]", server, response.code());
                throw new ShenyuException(message);
            }
            ResponseBody responseBody = response.body();
            Assert.notNull(responseBody, "Resolve response body failed.");
            String json = responseBody.string();
            LOG.info("listener result: [{}]", json);
            JsonObject responseFromServer = GsonUtils.getGson().fromJson(json, JsonObject.class);
            JsonElement element = responseFromServer.get("data");
            if (element.isJsonNull()) {
                return;
            }
            groupJson = responseFromServer.getAsJsonArray("data");
        } catch (IOException e) {
            String message = String.format("listener configs fail, server:[%s], %s", server, e.getMessage());
            throw new ShenyuException(message, e);
        }

        if (Objects.nonNull(groupJson) && !groupJson.isEmpty()) {
            // fetch group configuration async.
            ConfigGroupEnum[] changedGroups = GsonUtils.getGson().fromJson(groupJson, ConfigGroupEnum[].class);
            LOG.info("Group config changed: {}", Arrays.toString(changedGroups));
            this.doFetchGroupConfig(server, changedGroups);
        }
    }

    @Override
    public void close() {
        RUNNING.set(false);
        if (Objects.nonNull(executor)) {
            executor.shutdownNow();
            // help gc
            executor = null;
        }
    }

    class HttpLongPollingTask implements Runnable {

        private final String server;

        HttpLongPollingTask(final String server) {
            this.server = server;
        }

        @Override
        public void run() {
            while (RUNNING.get()) {
                int retryTimes = 10;
                for (int time = 1; time <= retryTimes; time++) {
                    try {
                        //do long polling.
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
