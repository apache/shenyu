package org.apache.shenyu.sync.data.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import org.apache.commons.logging.Log;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.dto.*;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class ApolloDataService implements SyncDataService {
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ApolloDataService.class);

    private final Config configService;

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    private final Map<String, ConfigChangeListener> cache = new ConcurrentHashMap<>();

    public ApolloDataService(final Config configService, final PluginDataSubscriber pluginDataSubscriber,
                             final List<MetaDataSubscriber> metaDataSubscribers,
                             final List<AuthDataSubscriber> authDataSubscribers) {
        this.configService = configService;
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
        watch();

    }

    public void watch() {
        watchPluginData();
        watchSelectorData();
        watchRuleData();
        watchAuthData();
        watchMetaData();
    }

    /**
     * watch plugin data.
     */
    private void watchPluginData() {
        ConfigChangeListener configChangeListener = changeEvent -> {
            changeEvent.changedKeys().forEach(key -> {
                if (key.contains(NacosPathConstants.PLUGIN_DATA_ID)) {
                    List<PluginData> pluginDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configService.getProperty(key, ""), PluginData.class).values());
                    pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
                        LOG.info("apollo listener pluginData: {}", pluginDataList);
                        subscriber.unSubscribe(pluginData);
                        subscriber.onSubscribe(pluginData);
                    }));
                }

            });
        };
        cache.put(DefaultPathConstants.PLUGIN_PARENT, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }


    /**
     * watch selector data.
     */
    private void watchSelectorData() {
        ConfigChangeListener configChangeListener = changeEvent -> {
            changeEvent.changedKeys().forEach(key -> {
                if (key.contains(NacosPathConstants.SELECTOR_DATA_ID)) {
                    List<SelectorData> selectorDataList = GsonUtils.getInstance().toObjectMapList(configService.getProperty(key, ""), SelectorData.class).values().stream().flatMap(Collection::stream).collect(Collectors.toList());
                    selectorDataList.forEach(selectorData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
                        LOG.info("apollo listener selectorData: {}", selectorDataList);
                        subscriber.unSelectorSubscribe(selectorData);
                        subscriber.onSelectorSubscribe(selectorData);
                    }));
                }

            });
        };
        cache.put(DefaultPathConstants.SELECTOR_PARENT, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }

    /**
     * watch rule data.
     */
    private void watchRuleData() {
        ConfigChangeListener configChangeListener = changeEvent -> {
            changeEvent.changedKeys().forEach(key -> {
                if (key.contains(NacosPathConstants.RULE_DATA_ID)) {
                    List<RuleData> ruleDataList = GsonUtils.getInstance().toObjectMapList(configService.getProperty(key, ""), RuleData.class).values()
                            .stream().flatMap(Collection::stream)
                            .collect(Collectors.toList());
                    ruleDataList.forEach(ruleData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(subscriber -> {
                        LOG.info("apollo listener ruleData: {}", ruleDataList);
                        subscriber.unRuleSubscribe(ruleData);
                        subscriber.onRuleSubscribe(ruleData);
                    }));
                }

            });
        };
        cache.put(NacosPathConstants.RULE_DATA_ID, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }

    /**
     * watch meta data.
     */
    private void watchMetaData() {
        ConfigChangeListener configChangeListener = changeEvent -> {
            changeEvent.changedKeys().forEach(key -> {
                if (key.contains(NacosPathConstants.META_DATA_ID)) {
                    List<MetaData> metaDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configService.getProperty(key, ""), MetaData.class).values());
                    metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> {
                        LOG.info("apollo listener metaData: {}", metaDataList);
                        subscriber.unSubscribe(metaData);
                        subscriber.onSubscribe(metaData);
                    }));
                }

            });
        };
        cache.put(NacosPathConstants.META_DATA_ID, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }

    /**
     * watch auth data.
     */
    private void watchAuthData() {
        ConfigChangeListener configChangeListener = changeEvent -> {
            changeEvent.changedKeys().forEach(key -> {
                if (key.contains(NacosPathConstants.AUTH_DATA_ID)) {
                    List<AppAuthData> appAuthDataList = new ArrayList<>(GsonUtils.getInstance().toObjectMap(configService.getProperty(key, ""), AppAuthData.class).values());
                    appAuthDataList.forEach(appAuthData -> authDataSubscribers.forEach(subscriber -> {
                        LOG.info("apollo listener appAuthData: {}", appAuthDataList);
                        subscriber.unSubscribe(appAuthData);
                        subscriber.onSubscribe(appAuthData);
                    }));
                }

            });
        };
        cache.put(NacosPathConstants.AUTH_DATA_ID, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }


    @Override
    public void close() {
        configService.removeChangeListener(cache.get(NacosPathConstants.PLUGIN_DATA_ID));
        configService.removeChangeListener(cache.get(NacosPathConstants.SELECTOR_DATA_ID));
        configService.removeChangeListener(cache.get(NacosPathConstants.RULE_DATA_ID));
        configService.removeChangeListener(cache.get(NacosPathConstants.AUTH_DATA_ID));
        configService.removeChangeListener(cache.get(NacosPathConstants.META_DATA_ID));
        cache.clear();
    }
}
