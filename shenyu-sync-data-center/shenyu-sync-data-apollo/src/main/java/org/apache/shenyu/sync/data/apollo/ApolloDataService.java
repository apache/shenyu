package org.apache.shenyu.sync.data.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigChangeListener;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
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
        // TODO watch plugin data
        watchPluginData();
        // TODO watch meta data
        // TODO watch auth data

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
                    System.out.println(pluginData);
                    pluginDataSubscriber.onSubscribe(GsonUtils.getInstance().fromJson(value, PluginData.class));
                }

            });
        };
        cache.put(DefaultPathConstants.PLUGIN_PARENT, configChangeListener);
        configService.addChangeListener(configChangeListener);
    }


    @Override
    public void close() throws Exception {
        cache.clear();
    }
}
