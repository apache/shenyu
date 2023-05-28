package org.apache.shenyu.admin.discovery;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.convert.selector.DiscoveryUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.springframework.beans.BeanUtils;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * DiscoveryHandler.
 */
public class DiscoveryDataChangedEventSyncListener implements DataChangedEventListener {

    private DiscoveryUpstreamParser discoveryUpstreamParser;

    private DiscoveryDO discoveryDO;

    private ApplicationEventPublisher eventPublisher;

    private DiscoveryUpstreamMapper discoveryUpstreamMapper;


    boolean needSync;

    boolean needPersistence;


    public DiscoveryDataChangedEventSyncListener(boolean needSync, boolean needPersistence) {
        this.needSync = needSync;
        this.needPersistence = needPersistence;
    }

    @Override
    public void onChange(DataChangedEvent event) {
        ProxySelectorData proxySelectorData = buildProxySelectorData(event.getKey(), event.getValue());
        //推送 gateway 数据|并且把数据 持久化到数据库(如果是 local 形式 就 不持久化了 因为本身就是 crud 数据库的)
        if (needSync) {
            org.apache.shenyu.admin.listener.DataChangedEvent dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE, Collections.singletonList(proxySelectorData));
            eventPublisher.publishEvent(dataChangedEvent);
        }
        DataChangedEvent.Event currentEvent = event.getEvent();
        if (needPersistence) {
            List<DiscoveryUpstreamData> discoveryUpstreamList = proxySelectorData.getDiscoveryUpstreamList();
            if (CollectionUtils.isEmpty(discoveryUpstreamList)) {
                //TODO: 28/5/2023 添加 日志
                return;
            }
            switch (currentEvent) {
                case ADDED:
                    discoveryUpstreamList.forEach(d -> {
                        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
                        BeanUtils.copyProperties(d, discoveryUpstreamDO);
                        discoveryUpstreamMapper.insert(discoveryUpstreamDO);
                    });
                    break;
                case UPDATED:
                    discoveryUpstreamList.forEach(d -> {
                        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
                        BeanUtils.copyProperties(d, discoveryUpstreamDO);
                        discoveryUpstreamMapper.update(discoveryUpstreamDO);
                    });
                    break;
                case DELETED:
                    if (CollectionUtils.isNotEmpty(discoveryUpstreamList)) {
                        List<String> upstreamIds = discoveryUpstreamList.stream().map(DiscoveryUpstreamData::getId).collect(Collectors.toList());
                        discoveryUpstreamMapper.deleteByIds(upstreamIds);
                    }
                    break;
            }
        }
    }


    private ProxySelectorData buildProxySelectorData(String key, String value) {
        List<DiscoveryUpstreamData> discoveryUpstreamDTOS = discoveryUpstreamParser.parseList(value);
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        proxySelectorData.setDiscoveryUpstreamList(discoveryUpstreamDTOS);
        String[] keys = parseKey(key);
        String pluginName = keys[0];
        String name = keys[1];
        proxySelectorData.setPluginName(pluginName);
        proxySelectorData.setName(name);
        return proxySelectorData;
    }

    //todo
    private String[] parseKey(String key) {
        return null;
    }


}
