package org.dromara.soul.sync.data.nacos.handler;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Executor;
import java.util.stream.Collectors;

/**
 * NacosCacheHandler
 *
 * @author Chenxjx
 * @author xiaoyu
 */
@Slf4j
public class NacosCacheHandler {
    
    protected static final String group = "DEFAULT_GROUP";
    
    protected static final String pluginDataId = "soul.plugin.json";
    
    protected static final String selectorDataId = "soul.selector.json";
    
    protected static final String ruleDataId = "soul.rule.json";
    
    protected static final String authDataId = "soul.auth.json";
    
    protected static final String metaDataId = "soul.meta.json";
    
    protected static final Map<String, List<Listener>> listeners = Maps.newConcurrentMap();
    
    protected final ConfigService configService;
    
    private final Map<String, PluginDataSubscriber> pluginDataSubscriberMap;
    
    private final List<MetaDataSubscriber> metaDataSubscribers;
    
    private final List<AuthDataSubscriber> authDataSubscribers;
    
    public NacosCacheHandler(final ConfigService configService, final List<PluginDataSubscriber> pluginDataSubscribers,
                             final List<MetaDataSubscriber> metaDataSubscribers,
                             final List<AuthDataSubscriber> authDataSubscribers) {
        this.configService = configService;
        this.pluginDataSubscriberMap = pluginDataSubscribers.stream().collect(Collectors.toMap(PluginDataSubscriber::pluginNamed, e -> e));
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
    }
    
    protected void updatePluginMap(final String configInfo) {
        try {
            List<PluginData> pluginDataList = GsonUtils.getInstance().fromList(configInfo, PluginData.class);
            pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriberMap.get(pluginData.getName())).ifPresent(e -> e.unSubscribe(pluginData)));
            pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriberMap.get(pluginData.getName())).ifPresent(e -> e.onSubscribe(pluginData)));
        } catch (Exception e) {
            log.error("sync plugin data have error:", e);
        }
    }
    
    protected void updateSelectorMap(String configInfo) {
        try {
            List<SelectorData> selectorDataList = GsonUtils.getInstance().fromList(configInfo, SelectorData.class);
            selectorDataList.forEach(selectorData -> Optional.ofNullable(pluginDataSubscriberMap.get(selectorData.getPluginName())).ifPresent(e -> e.unSelectorSubscribe(selectorData)));
            selectorDataList.forEach(selectorData -> Optional.ofNullable(pluginDataSubscriberMap.get(selectorData.getPluginName())).ifPresent(e -> e.onSelectorSubscribe(selectorData)));
        } catch (Exception e) {
            log.error("sync selector data have error:", e);
        }
    }
    
    protected void updateRuleMap(final String configInfo) {
        try {
            List<RuleData> ruleDataList = GsonUtils.getInstance().fromList(configInfo, RuleData.class);
            ruleDataList.forEach(ruleData -> Optional.ofNullable(pluginDataSubscriberMap.get(ruleData.getPluginName())).ifPresent(e -> e.unRuleSubscribe(ruleData)));
            ruleDataList.forEach(ruleData -> Optional.ofNullable(pluginDataSubscriberMap.get(ruleData.getPluginName())).ifPresent(e -> e.onRuleSubscribe(ruleData)));
        } catch (Exception e) {
            log.error("sync rule data have error:", e);
        }
    }
    
    protected void updateMetaDataMap(final String configInfo) {
        try {
            List<MetaData> metaDataList = GsonUtils.getInstance().fromList(configInfo, MetaData.class);
            metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> subscriber.unSubscribe(metaData)));
            metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(metaData)));
        } catch (Exception e) {
            log.error("sync meta data have error:", e);
        }
    }
    
    protected void updateAuthMap(final String configInfo) {
        try {
            List<AppAuthData> appAuthDataList = GsonUtils.getInstance().fromList(configInfo, AppAuthData.class);
            appAuthDataList.forEach(authData -> authDataSubscribers.forEach(subscriber -> subscriber.unSubscribe(authData)));
            appAuthDataList.forEach(authData -> authDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(authData)));
        } catch (Exception e) {
            log.error("sync auth data have error:", e);
        }
    }
    
    private String getConfigAndSignListener(String dataId, Listener listener) {
        try {
            return configService.getConfigAndSignListener(dataId, group, 6000, listener);
        } catch (Exception e) {
            return "{}";
        }
    }
    
    protected void watcherData(String dataId, OnChange oc) {
        try {
            Listener listener = new Listener() {
                @Override
                public void receiveConfigInfo(String configInfo) {
                    oc.change(configInfo);
                }
                
                @Override
                public Executor getExecutor() {
                    return null;
                }
            };
            oc.change(getConfigAndSignListener(dataId, listener));
            listeners.getOrDefault(dataId, new ArrayList<>()).add(listener);
        } catch (Exception e) {
            log.error("watcher data have error:", e);
        }
    }
    
    protected interface OnChange {
        
        void change(String changeData);
    }
}
