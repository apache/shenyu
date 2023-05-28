package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 *
 */
public class DefaultDiscoveryHandler implements DiscoveryHandler {
    // discoveryId,ShenyuDiscoveryService
    Map<String, ShenyuDiscoveryService> cache;

    private ApplicationEventPublisher eventPublisher;

    private boolean needSyncUpstream(String pluginName) {
        PluginEnum pluginEnumByName = PluginEnum.getPluginEnumByName(pluginName);
        switch (pluginEnumByName) {
            case TCP:
            case WEB_SOCKET:
                return true;
            default:
                return false;
        }
    }

    @Override
    public void create(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO) {
        String type = discoveryDO.getType();
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin(type);
        String props = discoveryDO.getProps();
        DiscoveryConfig discoveryConfig = GsonUtils.getGson().fromJson(props, DiscoveryConfig.class);
        discoveryConfig.setServerList(discoveryDO.getServiceList());
        discoveryService.init(discoveryConfig);
        String listenerNode = discoveryDO.getListenerNode();
        discoveryService.watcher(listenerNode, getDiscoveryDataChangedEventListener(proxySelectorDTO.getPluginName(), discoveryDO.getType()));
    }

    /**
     * 这边 是关闭 shenyuDiscoveryService 的整个监听服务
     *
     * @param discoveryDO
     */
    @Override
    public void removeDiscovery(DiscoveryDO discoveryDO) {
        ShenyuDiscoveryService shenyuDiscoveryService = cache.get(discoveryDO.getId());
        shenyuDiscoveryService.shutdown();
    }

    /**
     * 这边只作为删除  proxySelector 使用
     *
     * @param discoveryDO
     * @param proxySelectorDTO
     */
    @Override
    public void removeProxySelector(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO) {
        DataChangedEvent dataChangedEvent = new DataChangedEvent(buildProxySelectorKey(discoveryDO, proxySelectorDTO), GsonUtils.getInstance().toJson(proxySelectorDTO), DataChangedEvent.Event.DELETED);
        eventPublisher.publishEvent(dataChangedEvent);
    }

    private String buildProxySelectorKey(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO) {

        return "";
    }


    private DataChangedEventListener getDiscoveryDataChangedEventListener(String pluginName, String discoveryType) {
        return new DiscoveryDataChangedEventSyncListener(needSyncUpstream(pluginName), "local".equals(discoveryType));
    }

}
