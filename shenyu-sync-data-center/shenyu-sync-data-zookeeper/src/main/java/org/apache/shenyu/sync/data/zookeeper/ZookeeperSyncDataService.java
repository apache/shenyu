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

package org.apache.shenyu.sync.data.zookeeper;

import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.recipes.cache.ChildData;
import org.apache.curator.framework.recipes.cache.TreeCacheEvent;
import org.apache.curator.framework.recipes.cache.TreeCacheListener;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.PathMatchUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * this cache data with zookeeper.
 */
public class ZookeeperSyncDataService implements SyncDataService {

    private static final Logger LOGGER = LoggerFactory.getLogger(ZookeeperSyncDataService.class);

    private final ZookeeperClient zkClient;

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    /**
     * Instantiates a new Zookeeper cache manager.
     *
     * @param zkClient             the zk client
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     */
    public ZookeeperSyncDataService(final ZookeeperClient zkClient,
                                    final PluginDataSubscriber pluginDataSubscriber,
                                    final List<MetaDataSubscriber> metaDataSubscribers,
                                    final List<AuthDataSubscriber> authDataSubscribers) {
        this.zkClient = zkClient;
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        watcherData();
        watchAppAuth();
        watchMetaData();
    }

    private void watcherData() {
        zkClient.addCache(DefaultPathConstants.PLUGIN_PARENT, new PluginCacheListener());
        zkClient.addCache(DefaultPathConstants.SELECTOR_PARENT, new SelectorCacheListener());
        zkClient.addCache(DefaultPathConstants.RULE_PARENT, new RuleCacheListener());
    }

    private void watchAppAuth() {
        zkClient.addCache(DefaultPathConstants.APP_AUTH_PARENT, new AuthCacheListener());
    }

    private void watchMetaData() {
        zkClient.addCache(DefaultPathConstants.META_DATA, new MetadataCacheListener());
    }

    private void cachePluginData(final PluginData pluginData) {
        Optional.ofNullable(pluginData)
                .flatMap(data -> Optional.ofNullable(pluginDataSubscriber))
                .ifPresent(e -> e.onSubscribe(pluginData));
    }

    private void cacheSelectorData(final SelectorData selectorData) {
        Optional.ofNullable(selectorData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber)
                        .ifPresent(e -> e.onSelectorSubscribe(data)));
    }

    private void unCacheSelectorData(final String dataPath) {
        SelectorData selectorData = new SelectorData();
        final String selectorId = dataPath.substring(dataPath.lastIndexOf("/") + 1);
        final String str = dataPath.substring(DefaultPathConstants.SELECTOR_PARENT.length());
        final String pluginName = str.substring(1, str.length() - selectorId.length() - 1);
        selectorData.setPluginName(pluginName);
        selectorData.setId(selectorId);

        Optional.ofNullable(pluginDataSubscriber)
                .ifPresent(e -> e.unSelectorSubscribe(selectorData));
    }

    private void cacheRuleData(final RuleData ruleData) {
        Optional.ofNullable(ruleData)
                .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber)
                        .ifPresent(e -> e.onRuleSubscribe(data)));
    }

    private void unCacheRuleData(final String dataPath) {
        String substring = dataPath.substring(dataPath.lastIndexOf("/") + 1);
        final String str = dataPath.substring(DefaultPathConstants.RULE_PARENT.length());
        final String pluginName = str.substring(1, str.length() - substring.length() - 1);
        final List<String> list = Lists.newArrayList(Splitter.on(DefaultPathConstants.SELECTOR_JOIN_RULE).split(substring));

        RuleData ruleData = new RuleData();
        ruleData.setPluginName(pluginName);
        ruleData.setSelectorId(list.get(0));
        ruleData.setId(list.get(1));

        Optional.ofNullable(pluginDataSubscriber)
                .ifPresent(e -> e.unRuleSubscribe(ruleData));
    }

    private void cacheAuthData(final AppAuthData appAuthData) {
        Optional.ofNullable(appAuthData)
                .ifPresent(data -> authDataSubscribers.forEach(e -> e.onSubscribe(data)));
    }

    private void unCacheAuthData(final String dataPath) {
        final String key = dataPath.substring(DefaultPathConstants.APP_AUTH_PARENT.length() + 1);
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(key);
        authDataSubscribers.forEach(e -> e.unSubscribe(appAuthData));
    }

    private void cacheMetaData(final MetaData metaData) {
        Optional.ofNullable(metaData)
                .ifPresent(data -> metaDataSubscribers.forEach(e -> e.onSubscribe(metaData)));
    }

    private void unCacheMetaData(final MetaData metaData) {
        Optional.ofNullable(metaData)
                .ifPresent(data -> metaDataSubscribers.forEach(e -> e.unSubscribe(metaData)));
    }

    @Override
    public void close() {
        if (Objects.nonNull(zkClient)) {
            zkClient.close();
        }
    }

    abstract class AbstractDataSyncListener implements TreeCacheListener {
        @Override
        public final void childEvent(final CuratorFramework client, final TreeCacheEvent event) {
            ChildData childData = event.getData();
            if (null == childData) {
                return;
            }
            String path = childData.getPath();
            if (Strings.isNullOrEmpty(path)) {
                return;
            }
            event(event.getType(), path, childData);
        }

        /**
         * data sync event.
         *
         * @param type tree cache event type.
         * @param path tree cache event path.
         * @param data tree cache event data.
         */
        protected abstract void event(TreeCacheEvent.Type type, String path, ChildData data);
    }

    class PluginCacheListener extends AbstractDataSyncListener {

        private static final String PLUGIN_PATH = DefaultPathConstants.PLUGIN_PARENT + "/*";

        @Override
        public void event(final TreeCacheEvent.Type type, final String path, final ChildData data) {
            // if not uri register path, return.
            if (!PathMatchUtils.match(PLUGIN_PATH, path)) {
                return;
            }

            String pluginName = path.substring(path.lastIndexOf("/") + 1);

            // delete a plugin
            if (type.equals(TreeCacheEvent.Type.NODE_REMOVED)) {
                final PluginData pluginData = new PluginData();
                pluginData.setName(pluginName);
                Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSubscribe(pluginData));
            }

            // create or update
            Optional.ofNullable(data)
                    .ifPresent(e -> cachePluginData(GsonUtils.getInstance().fromJson(new String(data.getData(), StandardCharsets.UTF_8), PluginData.class)));
        }
    }

    class SelectorCacheListener extends AbstractDataSyncListener {

        // /shenyu/selector/{plugin}/{selector}
        private static final String SELECTOR_PATH = DefaultPathConstants.SELECTOR_PARENT + "/*/*";

        @Override
        public void event(final TreeCacheEvent.Type type, final String path, final ChildData data) {

            // if not uri register path, return.
            if (!PathMatchUtils.match(SELECTOR_PATH, path)) {
                return;
            }

            if (type.equals(TreeCacheEvent.Type.NODE_REMOVED)) {
                unCacheSelectorData(path);
            }

            // create or update
            Optional.ofNullable(data)
                    .ifPresent(e -> cacheSelectorData(GsonUtils.getInstance().fromJson(new String(data.getData(), StandardCharsets.UTF_8), SelectorData.class)));
        }
    }

    class MetadataCacheListener extends AbstractDataSyncListener {

        private static final String META_DATA_PATH = DefaultPathConstants.META_DATA + "/*";

        @Override
        public void event(final TreeCacheEvent.Type type, final String path, final ChildData data) {
            // if not uri register path, return.
            if (!PathMatchUtils.match(META_DATA_PATH, path)) {
                return;
            }

            if (type.equals(TreeCacheEvent.Type.NODE_REMOVED)) {
                final String realPath = path.substring(DefaultPathConstants.META_DATA.length() + 1);
                MetaData metaData = new MetaData();
                try {
                    metaData.setPath(URLDecoder.decode(realPath, StandardCharsets.UTF_8.name()));
                } catch (UnsupportedEncodingException e) {
                    throw new ShenyuException(e);
                }
                unCacheMetaData(metaData);
            }

            // create or update
            Optional.ofNullable(data)
                    .ifPresent(e -> cacheMetaData(GsonUtils.getInstance().fromJson(new String(data.getData(), StandardCharsets.UTF_8), MetaData.class)));
        }
    }

    class AuthCacheListener extends AbstractDataSyncListener {

        private static final String APP_AUTH_PATH = DefaultPathConstants.APP_AUTH_PARENT + "/*";

        @Override
        public void event(final TreeCacheEvent.Type type, final String path, final ChildData data) {
            // if not uri register path, return.
            if (!PathMatchUtils.match(APP_AUTH_PATH, path)) {
                return;
            }

            if (type.equals(TreeCacheEvent.Type.NODE_REMOVED)) {
                unCacheAuthData(path);
            }

            // create or update
            Optional.ofNullable(data)
                    .ifPresent(e -> cacheAuthData(GsonUtils.getInstance().fromJson(new String(data.getData(), StandardCharsets.UTF_8), AppAuthData.class)));
        }
    }

    class RuleCacheListener extends AbstractDataSyncListener {

        // /shenyu/rule/{plugin}/{rule}
        private static final String RULE_PATH = DefaultPathConstants.RULE_PARENT + "/*/*";

        @Override
        public void event(final TreeCacheEvent.Type type, final String path, final ChildData data) {
            // if not uri register path, return.
            if (!PathMatchUtils.match(RULE_PATH, path)) {
                return;
            }

            if (type.equals(TreeCacheEvent.Type.NODE_REMOVED)) {
                unCacheRuleData(path);
            }

            // create or update
            Optional.ofNullable(data)
                    .ifPresent(e -> cacheRuleData(GsonUtils.getInstance().fromJson(new String(data.getData(), StandardCharsets.UTF_8), RuleData.class)));
        }
    }

}
