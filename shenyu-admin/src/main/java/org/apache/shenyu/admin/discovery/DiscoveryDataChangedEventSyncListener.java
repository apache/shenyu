package org.apache.shenyu.admin.discovery;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.discovery.api.listener.DataChangedEvent;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.apache.shenyu.admin.discovery.parse.keyValueParser;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * DiscoveryHandler.
 */
public class DiscoveryDataChangedEventSyncListener implements DataChangedEventListener {

    private static final Logger LOG = LoggerFactory.getLogger(DiscoveryDataChangedEventSyncListener.class);

    private final keyValueParser keyValueParser;

    private DiscoveryDO discoveryDO;

    private final ApplicationEventPublisher eventPublisher;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    boolean needPersistence;


    public DiscoveryDataChangedEventSyncListener(ApplicationEventPublisher eventPublisher,
                                                 DiscoveryUpstreamMapper discoveryUpstreamMapper,
                                                 keyValueParser keyValueParser,
                                                 boolean needPersistence) {
        this.eventPublisher = eventPublisher;
        this.keyValueParser = keyValueParser;
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.needPersistence = needPersistence;
    }

    @Override
    public void onChange(DataChangedEvent event) {
        ProxySelectorData proxySelectorData = buildProxySelectorData(event.getKey(), event.getValue());
        //推送 gateway 数据|并且把数据 持久化到数据库(如果是 local 形式 就 不持久化了 因为本身就是 crud 数据库的)
        org.apache.shenyu.admin.listener.DataChangedEvent dataChangedEvent = null;
        DataChangedEvent.Event currentEvent = event.getEvent();
        if (needPersistence) {
            List<DiscoveryUpstreamData> discoveryUpstreamList = proxySelectorData.getDiscoveryUpstreamList();
            if (CollectionUtils.isEmpty(discoveryUpstreamList)) {
                LOG.warn("shenyu proxySelectorData#discoveryUpstreamList is empty");
                return;
            }
            switch (currentEvent) {
                case ADDED:
                    discoveryUpstreamList.forEach(d -> {
                        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
                        BeanUtils.copyProperties(d, discoveryUpstreamDO);
                        discoveryUpstreamMapper.insert(discoveryUpstreamDO);
                    });
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE, Collections.singletonList(proxySelectorData));
                    break;
                case UPDATED:
                    discoveryUpstreamList.forEach(d -> {
                        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
                        BeanUtils.copyProperties(d, discoveryUpstreamDO);
                        discoveryUpstreamMapper.update(discoveryUpstreamDO);
                    });
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(proxySelectorData));
                    break;
                case DELETED:
                    if (CollectionUtils.isNotEmpty(discoveryUpstreamList)) {
                        List<String> upstreamIds = discoveryUpstreamList.stream().map(DiscoveryUpstreamData::getId).collect(Collectors.toList());
                        discoveryUpstreamMapper.deleteByIds(upstreamIds);
                    }
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE, Collections.singletonList(proxySelectorData));
                    break;
            }
        }
        //FIXME: 2023/5/31  同步到 gateway
        if (Objects.nonNull(dataChangedEvent)) {
            eventPublisher.publishEvent(dataChangedEvent);
        }
    }


    private ProxySelectorData buildProxySelectorData(String key, String value) {
        List<DiscoveryUpstreamData> discoveryUpstreamDTOS = keyValueParser.parseValue(value);
        ProxySelectorData proxySelectorData = keyValueParser.parseKey(key);
        proxySelectorData.setDiscoveryUpstreamList(discoveryUpstreamDTOS);
        return proxySelectorData;
    }

}
