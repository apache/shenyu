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

package org.dromara.soul.web.cache;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
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
import org.dromara.soul.web.config.SoulConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.boot.CommandLineRunner;
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
 * @since 2.0.0
 */
@SuppressWarnings("all")
public class HttpLongPollSyncCache extends HttpCacheHandler implements CommandLineRunner, DisposableBean {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpLongPollSyncCache.class);

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
     * only use for http long polling, init by {@link  CommandLineRunner}.
     */
    private RestTemplate httpClient;

    private ExecutorService executor;

    private SoulConfig.HttpConfig httpConfig;

    private List<String> serverList;

    public HttpLongPollSyncCache(final SoulConfig.HttpConfig httpConfig) {
        this.httpConfig = httpConfig;
        serverList = Lists.newArrayList(Splitter.on(",").split(httpConfig.getUrl()));
    }

    @Override
    public void run(final String... args) {

        // init RestTemplate
        OkHttp3ClientHttpRequestFactory factory = new OkHttp3ClientHttpRequestFactory();
        factory.setConnectTimeout((int) this.connectionTimeout.toMillis());
        factory.setReadTimeout((int) HttpConstants.CLIENT_POLLING_READ_TIMEOUT);
        this.httpClient = new RestTemplate(factory);

        // It could be initialized multiple times, so you need to control that.
        if (RUNNING.compareAndSet(false, true)) {

            // fetch all group configs.
            this.fetchGroupConfig(ConfigGroupEnum.values());

            // one thread for listener, another one for fetch configuration data.
            this.executor = new ThreadPoolExecutor(3, 3, 0L, TimeUnit.MILLISECONDS,
                    new LinkedBlockingQueue<>(),
                    SoulThreadFactory.create("http-long-polling", true));

            // start long polling.
            this.executor.execute(new HttpLongPollingTask());
        } else {
            LOGGER.info("soul http long polling was started, executor=[{}]", executor);
        }
    }

    @Override
    public void destroy() {
        RUNNING.set(false);
        if (executor != null) {
            executor.shutdownNow();
            // help gc
            executor = null;
        }
    }

    private void fetchGroupConfig(final ConfigGroupEnum... groups) throws SoulException {
        StringBuilder params = new StringBuilder();
        for (ConfigGroupEnum groupKey : groups) {
            params.append("groupKeys")
                    .append("=")
                    .append(groupKey.name())
                    .append("&");
        }

        SoulException ex = null;
        for (String server : serverList) {
            String url = server + "/configs/fetch?" + StringUtils.removeEnd(params.toString(), "&");
            LOGGER.info("request configs: [{}]", url);
            try {
                String json = this.httpClient.getForObject(url, String.class);
                LOGGER.info("get latest configs: [{}]", json);
                updateCacheWithJson(json);
                return;
            } catch (Exception e) {
                LOGGER.warn("request configs fail, server:[{}]", server);
                ex = new SoulException("Init cache error, serverList:" + httpConfig.getUrl(), e);
                // try next server, if have another one.
            }
        }

        if (ex != null) {
            throw ex;
        }
    }

    private void updateCacheWithJson(final String json) {

        JsonObject jsonObject = GSON.fromJson(json, JsonObject.class);
        JsonObject data = jsonObject.getAsJsonObject("data");

        // plugin
        JsonObject pluginData = data.getAsJsonObject(ConfigGroupEnum.PLUGIN.name());
        if (pluginData != null) {
            ConfigData<PluginData> result = GSON.fromJson(pluginData, new TypeToken<ConfigData<PluginData>>() {
            }.getType());
            GROUP_CACHE.put(ConfigGroupEnum.PLUGIN, result);
            this.flushAllPlugin(result.getData());
        }

        // rule
        JsonObject ruleData = data.getAsJsonObject(ConfigGroupEnum.RULE.name());
        if (ruleData != null) {
            ConfigData<RuleData> result = GSON.fromJson(ruleData, new TypeToken<ConfigData<RuleData>>() {
            }.getType());
            GROUP_CACHE.put(ConfigGroupEnum.RULE, result);
            this.flushAllRule(result.getData());
        }

        // selector
        JsonObject selectorData = data.getAsJsonObject(ConfigGroupEnum.SELECTOR.name());
        if (selectorData != null) {
            ConfigData<SelectorData> result = GSON.fromJson(selectorData, new TypeToken<ConfigData<SelectorData>>() {
            }.getType());
            GROUP_CACHE.put(ConfigGroupEnum.SELECTOR, result);
            this.flushAllSelector(result.getData());
        }

        // appAuth
        JsonObject appAuthData = data.getAsJsonObject(ConfigGroupEnum.APP_AUTH.name());
        if (appAuthData != null) {
            ConfigData<AppAuthData> result = GSON.fromJson(appAuthData, new TypeToken<ConfigData<AppAuthData>>() {
            }.getType());
            GROUP_CACHE.put(ConfigGroupEnum.APP_AUTH, result);
            this.flushAllAppAuth(result.getData());
        }

        // metaData
        JsonObject metaData = data.getAsJsonObject(ConfigGroupEnum.META_DATA.name());
        if (metaData != null) {
            ConfigData<MetaData> result = GSON.fromJson(metaData, new TypeToken<ConfigData<MetaData>>() {
            }.getType());
            GROUP_CACHE.put(ConfigGroupEnum.META_DATA, result);
            this.flushMetaData(result.getData());
        }





    }

    @SuppressWarnings("unchecked")
    private void doLongPolling() {
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>(16);
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            ConfigData<?> cacheConfig = GROUP_CACHE.get(group);
            String value = String.join(",", cacheConfig.getMd5(), String.valueOf(cacheConfig.getLastModifyTime()));
            params.put(group.name(), Lists.newArrayList(value));
        }
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity httpEntity = new HttpEntity(params, headers);
        for (String server : serverList) {
            String listenerUrl = server + "/configs/listener";
            LOGGER.debug("request listener configs: [{}]", listenerUrl);
            try {
                String json = this.httpClient.postForEntity(listenerUrl, httpEntity, String.class).getBody();
                LOGGER.debug("listener result: [{}]", json);
                JsonArray groupJson = GSON.fromJson(json, JsonObject.class).getAsJsonArray("data");
                if (groupJson != null) {
                    // fetch group configuration async.
                    ConfigGroupEnum[] changedGroups = GSON.fromJson(groupJson, ConfigGroupEnum[].class);
                    if (ArrayUtils.isNotEmpty(changedGroups)) {
                        LOGGER.info("Group config changed: {}", Arrays.toString(changedGroups));
                        this.fetchGroupConfig(changedGroups);
                    }
                }
                break;
            } catch (RestClientException e) {
                LOGGER.error("listener configs fail, can not connection this server:[{}]", listenerUrl);
                /*  ex = new SoulException("Init cache error, serverList:" + serverList, e);*/
                // try next server, if have another one.
            }
        }
    }

    class HttpLongPollingTask implements Runnable {
        @Override
        public void run() {
            while (RUNNING.get()) {
                try {
                    doLongPolling();
                } catch (Exception e) {
                    LOGGER.error(e.getMessage(), e);
                }
            }
            LOGGER.warn("Stop http long polling.");
        }
    }

}
