package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationEventPublisherAware;

import java.util.List;

public class LocalDiscoveryProcessor implements DiscoveryProcessor, ApplicationEventPublisherAware {

    private static final Logger LOG = LoggerFactory.getLogger(LocalDiscoveryProcessor.class);


    @Override
    public void createDiscovery(DiscoveryDO discoveryDO) {
        //do nothing
        LOG.info("shenyu discovery local mode do nothingn in createDiscovery¬");
    }

    @Override
    public void createProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO) {
        //

    }

    @Override
    public void removeDiscovery(DiscoveryDO discoveryDO) {
        //do nothing
        LOG.info("shenyu discovery local mode do nothing in removeDiscovery");
    }

    @Override
    public void removeProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO) {
        //

    }

    @Override
    public void changeUpstream(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO, List<DiscoveryUpstreamDTO> upstreamDTOS) {

    }

    @Override
    public void setApplicationEventPublisher(ApplicationEventPublisher applicationEventPublisher) {

    }
}
