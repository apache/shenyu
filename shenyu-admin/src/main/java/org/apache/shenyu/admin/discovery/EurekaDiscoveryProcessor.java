package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.transfer.DiscoveryTransfer;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;

import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class EurekaDiscoveryProcessor extends AbstractDiscoveryProcessor {

    /**
     * DefaultDiscoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     */
    public EurekaDiscoveryProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper,
                                    final DiscoveryHandlerMapper discoveryHandlerMapper,
                                    final ProxySelectorMapper proxySelectorMapper) {
        super(discoveryUpstreamMapper, discoveryHandlerMapper, proxySelectorMapper);
    }

    @Override
    public void createProxySelector(final DiscoveryHandlerDTO discoveryHandlerDTO, final ProxySelectorDTO proxySelectorDTO) {
        ShenyuDiscoveryService shenyuDiscoveryService = discoveryServiceCache.get(discoveryHandlerDTO.getDiscoveryId());
        String key = super.buildProxySelectorKey(discoveryHandlerDTO.getListenerNode());
        if (Objects.isNull(shenyuDiscoveryService)) {
            throw new ShenyuAdminException(String.format("before start ProxySelector you need init DiscoveryId=%s", discoveryHandlerDTO.getDiscoveryId()));
        }
        Set<String> cacheKey = dataChangedEventListenerCache.get(discoveryHandlerDTO.getDiscoveryId());
        if (Objects.nonNull(cacheKey) && cacheKey.contains(key)) {
            throw new ShenyuAdminException(String.format("shenyu discovery has watcher key = %s", key));
        }
        shenyuDiscoveryService.watch(key, getDiscoveryDataChangedEventListener(discoveryHandlerDTO, proxySelectorDTO));
        cacheKey.add(key);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE,
                Collections.singletonList(DiscoveryTransfer.INSTANCE.mapToData(proxySelectorDTO)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

}
