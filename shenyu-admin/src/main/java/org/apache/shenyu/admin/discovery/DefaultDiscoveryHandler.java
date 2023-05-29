package org.apache.shenyu.admin.discovery;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.discovery.parse.CustomDiscoveryUpstreamParser;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 一共有三类
 * path =>  /shenyu/discovery/{pluginName}/{selectorId}
 * 1. tcp，upd ws 需要自己自定义的注册的 自己维护下游的 upstream 列表的 数据的
 * 211.这样的数据需要自定义 proxySelector
 * 2. dubbo， tars ， springcloud 的这样的 不需要 维护的 下游 upstream 数据的 因为 使用的 泛化 都是支持的 不需要手动维护下游的 upstream
 * path ==>  /dubbo/{interface}/providers
 * 222.这样的数据只有一个 proxySelector  更加的虚拟化
 * data =[dubbo://192.168.99.1:20880/com.alibaba.dubbo.samples.api.GreetingService?anyhost=true&application=demo-provider&dubbo=2.6.2&generic=false&interface=com.alibaba.dubbo.samples.api.GreetingService&methods=sayHello&pid=12938&side=provider&timestamp=1533264631849]
 * 3. 最为特殊的 discovery 为 local的数据
 * 需要手动为维护 下游的 upstream 和 proxySelector  通样的
 * upstream 也分为 自己维护下游的 upstream 列表的  (TCP) 和不需要手动维护的 DUBBO
 *
 */
public class DefaultDiscoveryHandler implements DiscoveryHandler {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultDiscoveryHandler.class);
    public static final String KEY_TEMPLATE = "/shenyu/discovery/%s/%s";

    Map<String, ShenyuDiscoveryService> cache;

    private ApplicationEventPublisher eventPublisher;

    private Boolean needSync;

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
        //ExtensionLoader has cache no need to cache again
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin(type);
        String props = discoveryDO.getProps();
        DiscoveryConfig discoveryConfig = GsonUtils.getGson().fromJson(props, DiscoveryConfig.class);
        discoveryConfig.setServerList(discoveryDO.getServiceList());
        discoveryService.init(discoveryConfig);
        String listenerNode = discoveryDO.getListenerNode();
        String key = String.format(KEY_TEMPLATE, discoveryDO.getName(), proxySelectorDTO.getId());
        if (StringUtils.isEmpty(discoveryService.getData(key))) {
            LOG.info("shenyu discovery {} is empty need register it ", key);
            discoveryService.register(key, GsonUtils.getInstance().toJson(proxySelectorDTO));
        }
        discoveryService.watcher(listenerNode, getDiscoveryDataChangedEventListener(proxySelectorDTO.getPluginName(), discoveryDO.getType()));
    }


    @Override
    public void update(ProxySelectorDTO proxySelectorDTO) {
        if(needSync){
            DataChangedEvent dataChangedEvent = new DataChangedEvent(buildProxySelectorKey(proxySelectorDTO), GsonUtils.getInstance().toJson(proxySelectorDTO), DataChangedEvent.Event.UPDATED);
            eventPublisher.publishEvent(dataChangedEvent);
        }
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
        if(needSync){
            DataChangedEvent dataChangedEvent = new DataChangedEvent(buildProxySelectorKey(proxySelectorDTO), GsonUtils.getInstance().toJson(proxySelectorDTO), DataChangedEvent.Event.DELETED);
            eventPublisher.publishEvent(dataChangedEvent);
        }
    }

    private String buildProxySelectorKey(ProxySelectorDTO proxySelectorDTO) {
        return "";
    }


    private DataChangedEventListener getDiscoveryDataChangedEventListener(String pluginName, String discoveryType) {
        //return new DiscoveryDataChangedEventSyncListener(needSyncUpstream(pluginName), "local".equals(discoveryType));
        return null;
    }

}
