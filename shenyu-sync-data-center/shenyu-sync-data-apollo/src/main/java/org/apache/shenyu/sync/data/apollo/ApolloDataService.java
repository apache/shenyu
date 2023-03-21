package org.apache.shenyu.sync.data.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.*;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

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
                if (key.contains(DefaultPathConstants.PLUGIN_PARENT)) {
                    String value = configService.getProperty(key, "");
                    PluginData pluginData = GsonUtils.getInstance().fromJson(value, PluginData.class);
                    System.out.println("watchPluginData----->"+pluginData);
                    pluginDataSubscriber.onSubscribe(GsonUtils.getInstance().fromJson(value, PluginData.class));
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
                if (key.contains(DefaultPathConstants.SELECTOR_PARENT)) {
                    String value = configService.getProperty(key, "");
                    final SelectorData selectorData = GsonUtils.getInstance().fromJson(value, SelectorData.class);
                    System.out.println("watchSelectorData----->"+selectorData);
                    Optional.ofNullable(selectorData)
                            .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onSelectorSubscribe(data)));
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
                if (key.contains(DefaultPathConstants.RULE_PARENT)) {
                    String value = configService.getProperty(key, "");
                    final RuleData ruleData = GsonUtils.getInstance().fromJson(value, RuleData.class);
                    System.out.println("watchRuleData----->"+ruleData);
                    Optional.ofNullable(ruleData)
                            .ifPresent(data -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onRuleSubscribe(data)));
                }

            });
        };
        cache.put(DefaultPathConstants.RULE_PARENT, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }

    /**
     * watch meta data.
     */
    private void watchMetaData() {
        ConfigChangeListener configChangeListener = changeEvent -> {
            changeEvent.changedKeys().forEach(key -> {
                if (key.contains(DefaultPathConstants.META_DATA)) {
                    String value = configService.getProperty(key, "");
                    final MetaData metaData = GsonUtils.getInstance().fromJson(value, MetaData.class);
                    Optional.ofNullable(metaData)
                            .ifPresent(data -> metaDataSubscribers.forEach(e -> e.onSubscribe(metaData)));
                }

            });
        };
        cache.put(DefaultPathConstants.META_DATA, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }

    /**
     * watch auth data.
     */
    private void watchAuthData() {
        ConfigChangeListener configChangeListener = changeEvent -> {
            changeEvent.changedKeys().forEach(key -> {
                if (key.contains(DefaultPathConstants.APP_AUTH_PARENT)) {
                    String value = configService.getProperty(key, "");
                    final AppAuthData appAuthData = GsonUtils.getInstance().fromJson(value, AppAuthData.class);
                    Optional.ofNullable(appAuthData)
                            .ifPresent(data -> authDataSubscribers.forEach(e -> e.onSubscribe(data)));
                }

            });
        };
        cache.put(DefaultPathConstants.APP_AUTH_PARENT, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }


    @Override
    public void close() {
        configService.removeChangeListener(cache.get(DefaultPathConstants.PLUGIN_PARENT));
        configService.removeChangeListener(cache.get(DefaultPathConstants.SELECTOR_PARENT));
        configService.removeChangeListener(cache.get(DefaultPathConstants.RULE_PARENT));
        configService.removeChangeListener(cache.get(DefaultPathConstants.META_DATA));
        configService.removeChangeListener(cache.get(DefaultPathConstants.APP_AUTH_PARENT));
        cache.clear();
    }
}
