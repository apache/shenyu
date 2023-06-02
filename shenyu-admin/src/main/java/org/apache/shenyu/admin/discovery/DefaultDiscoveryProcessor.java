package org.apache.shenyu.admin.discovery;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.discovery.parse.CustomDiscoveryUpstreamParser;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;

import java.util.*;

/**
 *
 */
public class DefaultDiscoveryProcessor implements DiscoveryProcessor {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultDiscoveryProcessor.class);

    // /shenyu/discovery/{pluginName}/{selectorId}
    public static final String KEY_TEMPLATE = "%s/%s/%s";

    public static final String DEFAULT_LISTENER_NODE = "/shenyu/discovery";

    Map<String, ShenyuDiscoveryService> discoveryServiceCache;

    private ApplicationEventPublisher eventPublisher;

    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private ProxySelectorMapper proxySelectorMapper;

    @Override
    public void createDiscovery(DiscoveryDO discoveryDO) {
        String type = discoveryDO.getType();
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin(type);
        String props = discoveryDO.getProps();
        DiscoveryConfig discoveryConfig = GsonUtils.getGson().fromJson(props, DiscoveryConfig.class);
        discoveryConfig.setServerList(discoveryDO.getServiceList());
        discoveryService.init(discoveryConfig);
        discoveryServiceCache.put(discoveryDO.getId(), discoveryService);
    }


    @Override
    public void createProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryHandlerDTO.getDiscoveryId());
        if (Objects.isNull(shenyuDiscoveryService)) {
            LOG.warn("before start ProxySelector you need init DiscoveryId={}", discoveryHandlerDTO.getDiscoveryId());
            return;
        }
        String listenerNode = discoveryHandlerDTO.getListenerNode();
        String key = buildProxySelectorKey(listenerNode, proxySelectorDTO);
        if (StringUtils.isEmpty(shenyuDiscoveryService.getData(key))) {
            LOG.info("shenyu discovery {} is empty need register it ", key);
            shenyuDiscoveryService.register(key, GsonUtils.getInstance().toJson(proxySelectorDTO));
        }
        shenyuDiscoveryService.watcher(key, getDiscoveryDataChangedEventListener(proxySelectorDTO.getType(), discoveryHandlerDTO.getProps()));
    }

    /**
     * 这边 是关闭 shenyuDiscoveryService 的整个监听服务
     *
     * @param discoveryDO
     */
    @Override
    public void removeDiscovery(DiscoveryDO discoveryDO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryDO.getId());
        shenyuDiscoveryService.shutdown();
    }

    /**
     * 这边只作为删除  proxySelector 使用
     *
     * @param proxySelectorDTO
     */
    @Override
    public void removeProxySelector(ProxySelectorDTO proxySelectorDTO) {
        org.apache.shenyu.admin.listener.DataChangedEvent dataChangedEvent = new org.apache.shenyu.admin.listener.
                DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE, Collections.singletonList(proxySelectorDTO));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    private String buildProxySelectorKey(final String listenerNode, final ProxySelectorDTO proxySelectorDTO) {
        String key = StringUtils.isBlank(listenerNode) ?
                DEFAULT_LISTENER_NODE : listenerNode;
        return String.format(KEY_TEMPLATE, key, proxySelectorDTO.getPluginName(), proxySelectorDTO.getId());
    }


    private DataChangedEventListener getDiscoveryDataChangedEventListener(String discoveryType, String customProps) {
        Map<String, String> customMap = GsonUtils.getInstance().toObjectMap(customProps, String.class);
        return new DiscoveryDataChangedEventSyncListener(eventPublisher, discoveryUpstreamMapper,
                new CustomDiscoveryUpstreamParser(customMap, proxySelectorMapper), "local".equals(discoveryType));
    }

}
