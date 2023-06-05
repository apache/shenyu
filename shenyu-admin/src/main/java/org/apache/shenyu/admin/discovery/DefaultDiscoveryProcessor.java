package org.apache.shenyu.admin.discovery;

import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.discovery.parse.CustomDiscoveryUpstreamParser;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * DefaultDiscoveryProcessor.
 */
public class DefaultDiscoveryProcessor implements DiscoveryProcessor, ApplicationEventPublisherAware {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultDiscoveryProcessor.class);

    public static final String KEY_TEMPLATE = "%s/%s/%s";

    public static final String DEFAULT_LISTENER_NODE = "/shenyu/discovery";

    private final Map<String, ShenyuDiscoveryService> discoveryServiceCache;

    private ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final ProxySelectorMapper proxySelectorMapper;

    /**
     * DefaultDiscoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     * @param proxySelectorMapper     proxySelectorMapper
     */
    public DefaultDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper, final ProxySelectorMapper proxySelectorMapper) {
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.proxySelectorMapper = proxySelectorMapper;
        this.discoveryServiceCache = new ConcurrentHashMap<>();
    }

    @Override
    public void createDiscovery(final DiscoveryDO discoveryDO) {
        String type = discoveryDO.getType();
        ShenyuDiscoveryService discoveryService = ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class).getJoin(type);
        String props = discoveryDO.getProps();
        DiscoveryConfig discoveryConfig = GsonUtils.getGson().fromJson(props, DiscoveryConfig.class);
        discoveryConfig.setServerList(discoveryDO.getServiceList());
        discoveryService.init(discoveryConfig);
        discoveryServiceCache.put(discoveryDO.getId(), discoveryService);
    }


    @Override
    public void createProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
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
        // TODO: 2023/6/5 need to
        //DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE, );
    }


    /**
     * removeDiscovery by ShenyuDiscoveryService#shutdown .
     *
     * @param discoveryDO discoveryDO
     */
    @Override
    public void removeDiscovery(final DiscoveryDO discoveryDO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryDO.getId());
        shenyuDiscoveryService.shutdown();
    }

    /**
     * removeProxySelector.
     *
     * @param proxySelectorDTO proxySelectorDTO
     */
    @Override
    public void removeProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryHandlerDTO.getDiscoveryId());
        org.apache.shenyu.admin.listener.DataChangedEvent dataChangedEvent = new org.apache.shenyu.admin.listener.
                DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE, Collections.singletonList(proxySelectorDTO));
        eventPublisher.publishEvent(dataChangedEvent);
        String key = buildProxySelectorKey(discoveryHandlerDTO.getListenerNode(), proxySelectorDTO);
        shenyuDiscoveryService.unWatcher(key);
    }

    @Override
    public void changeUpstream(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO, List<DiscoveryUpstreamDTO> upstreamDTOS) {
        throw new NotImplementedException("changeUpstream don't support in DefaultDiscoveryProcessor");
    }


    /**
     * buildProxySelectorKey.
     *
     * @param listenerNode     listenerNode
     * @param proxySelectorDTO proxySelectorDTO
     * @return key
     */
    private String buildProxySelectorKey(final String listenerNode, final ProxySelectorDTO proxySelectorDTO) {
        String key = StringUtils.isBlank(listenerNode) ?
                DEFAULT_LISTENER_NODE : listenerNode;
        return String.format(KEY_TEMPLATE, key, proxySelectorDTO.getPluginName(), proxySelectorDTO.getId());
    }

    /**
     * getDiscoveryDataChangedEventListener.
     *
     * @param discoveryType discoveryType
     * @param customProps   customProps
     * @return DataChangedEventListener
     */
    private DataChangedEventListener getDiscoveryDataChangedEventListener(final String discoveryType, final String customProps) {
        Map<String, String> customMap = GsonUtils.getInstance().toObjectMap(customProps, String.class);
        return new DiscoveryDataChangedEventSyncListener(eventPublisher, discoveryUpstreamMapper,
                new CustomDiscoveryUpstreamParser(customMap, proxySelectorMapper), "local".equals(discoveryType));
    }

    @Override
    public void setApplicationEventPublisher(final ApplicationEventPublisher eventPublisher) {
        this.eventPublisher = eventPublisher;
    }
}
