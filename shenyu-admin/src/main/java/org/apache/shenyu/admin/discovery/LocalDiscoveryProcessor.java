package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;

import java.util.Collections;
import java.util.List;

/**
 * LocalDiscoveryProcessor.
 */
public class LocalDiscoveryProcessor implements DiscoveryProcessor, ApplicationEventPublisherAware {

    private static final Logger LOG = LoggerFactory.getLogger(LocalDiscoveryProcessor.class);

    private ApplicationEventPublisher eventPublisher;

    @Override
    public void createDiscovery(DiscoveryDO discoveryDO) {
        LOG.info("shenyu discovery local mode do nothing in createDiscovery");
    }

    @Override
    public void createProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO) {
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE, Collections.singletonList(covert(proxySelectorDTO, null)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void removeDiscovery(DiscoveryDO discoveryDO) {
        LOG.info("shenyu discovery local mode do nothing in removeDiscovery");
    }

    @Override
    public void removeProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO) {
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE, Collections.singletonList(covert(proxySelectorDTO, null)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void changeUpstream(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO, List<DiscoveryUpstreamDTO> upstreamDTOS) {
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE, Collections.singletonList(covert(proxySelectorDTO, upstreamDTOS)));
        eventPublisher.publishEvent(dataChangedEvent);
    }

    @Override
    public void setApplicationEventPublisher(ApplicationEventPublisher applicationEventPublisher) {
        this.eventPublisher = applicationEventPublisher;
    }



}
