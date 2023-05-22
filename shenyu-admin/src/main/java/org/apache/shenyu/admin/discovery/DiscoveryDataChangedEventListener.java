package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.convert.selector.DiscoveryUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * DiscoveryHandler.
 */
public class DiscoveryDataChangedEventListener implements DataChangedEventListener {

    private DiscoveryUpstreamParser discoveryUpstreamParser;

    private DiscoveryDO discoveryDO;

    private ApplicationEventPublisher eventPublisher;

    @Override
    public void onChange(DataChangedEvent event) {
        ProxySelectorData proxySelectorData = buildProxySelectorData(event.getKey(), event.getValue());
        //推送 gateway 数据|并且把数据 持久化到数据库(如果是 local 形式 就 不持久化了 因为本身就是 crud 数据库的)
        org.apache.shenyu.admin.listener.DataChangedEvent dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE, Collections.singletonList(proxySelectorData));
        eventPublisher.publishEvent(dataChangedEvent);

    }


    private ProxySelectorData buildProxySelectorData(String key, String value) {
        List<DiscoveryUpstreamDTO> discoveryUpstreamDTOS = discoveryUpstreamParser.parseList(value);
        String[] keys = parseKey(key);
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        proxySelectorData.setPluginName(discoveryDO.getName());
        proxySelectorData.setName(keys[1]);
        proxySelectorData.setId(keys[2]);
        List<DiscoveryUpstream> collect = discoveryUpstreamDTOS.stream().map(s -> new DiscoveryUpstream()).collect(Collectors.toList());
        proxySelectorData.setDiscoveryUpstreamList(collect);
        return proxySelectorData;
    }

    //todo
    private String[] parseKey(String key) {
        return null;
    }


}
