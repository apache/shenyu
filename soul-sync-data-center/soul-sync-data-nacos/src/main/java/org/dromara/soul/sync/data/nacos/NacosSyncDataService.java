package org.dromara.soul.sync.data.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.sync.data.api.SyncDataService;
import org.dromara.soul.sync.data.nacos.handler.NacosCacheHandler;

import java.util.List;

/**
 * @author Chenxj
 * @author xiaoyu
 */
public class NacosSyncDataService extends NacosCacheHandler implements AutoCloseable, SyncDataService {
    
    public NacosSyncDataService(final ConfigService configService, final List<PluginDataSubscriber> pluginDataSubscribers,
                                final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers) {
        
        super(configService, pluginDataSubscribers, metaDataSubscribers, authDataSubscribers);
        start();
    }
    
    public void start() {
        watcherData(pluginDataId, this::updatePluginMap);
        watcherData(selectorDataId, this::updateSelectorMap);
        watcherData(ruleDataId, this::updateRuleMap);
        watcherData(metaDataId, this::updateMetaDataMap);
        watcherData(authDataId, this::updateAuthMap);
    }
    
    @Override
    public void close() {
        listeners.forEach((dataId, lss) -> {
            lss.forEach(listener -> configService.removeListener(dataId, group, listener));
            lss.clear();
        });
        listeners.clear();
    }
}
