package org.apache.shenyu.admin.discovery;

import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.common.dto.DiscoverySyncData;
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
        DiscoverySyncData discoverySyncData = buildProxySelectorData(event.getKey(), event.getValue());
        //推送 gateway 数据|并且把数据 持久化到数据库(如果是 local 形式 就 不持久化了 因为本身就是 crud 数据库的)
        org.apache.shenyu.admin.listener.DataChangedEvent dataChangedEvent = null;
        DataChangedEvent.Event currentEvent = event.getEvent();
        List<DiscoveryUpstreamData> upstreamDataList = discoverySyncData.getUpstreamDataList();
        if (needPersistence) {
            if (CollectionUtils.isEmpty(upstreamDataList)) {
                LOG.warn("shenyu proxySelectorData#discoveryUpstreamList is empty");
                return;
            }
            switch (currentEvent) {
                case ADDED:
                    upstreamDataList.forEach(d -> {
                        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
                        BeanUtils.copyProperties(d, discoveryUpstreamDO);
                        discoveryUpstreamMapper.insert(discoveryUpstreamDO);
                    });
                    fillFullyDiscoverySyncData(discoverySyncData);
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.CREATE, Collections.singletonList(discoverySyncData));
                    break;
                case UPDATED:
                    upstreamDataList.forEach(d -> {
                        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
                        BeanUtils.copyProperties(d, discoveryUpstreamDO);
                        discoveryUpstreamMapper.update(discoveryUpstreamDO);
                    });
                    fillFullyDiscoverySyncData(discoverySyncData);
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData));
                    break;
                case DELETED:
                    if (CollectionUtils.isNotEmpty(upstreamDataList)) {
                        List<String> upstreamIds = upstreamDataList.stream().map(DiscoveryUpstreamData::getId).collect(Collectors.toList());
                        discoveryUpstreamMapper.deleteByIds(upstreamIds);
                    }
                    fillFullyDiscoverySyncData(discoverySyncData);
                    dataChangedEvent = new org.apache.shenyu.admin.listener.DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, DataEventTypeEnum.DELETE, Collections.singletonList(discoverySyncData));
                    break;
            }
        }
        //FIXME: 2023/5/31  同步到 gateway
        if (Objects.nonNull(dataChangedEvent)) {
            eventPublisher.publishEvent(dataChangedEvent);
        }
    }

    private void fillFullyDiscoverySyncData(DiscoverySyncData discoverySyncData) {
        ProxySelectorData proxySelectorData = discoverySyncData.getProxySelectorData();
        List<DiscoveryUpstreamDO> discoveryUpstreamDOS = discoveryUpstreamMapper.selectByProxySelectorId(proxySelectorData.getId());
        List<DiscoveryUpstreamData> collect = discoveryUpstreamDOS.stream().map(discoveryUpstreamDO -> {
            DiscoveryUpstreamData discoveryUpstreamDTO = new DiscoveryUpstreamData();
            BeanUtils.copyProperties(discoveryUpstreamDO, discoveryUpstreamDTO);
            return discoveryUpstreamDTO;
        }).collect(Collectors.toList());
        discoverySyncData.setUpstreamDataList(collect);
    }

    private DiscoverySyncData buildProxySelectorData(String key, String value) {
        List<DiscoveryUpstreamData> discoveryUpstreamDTOS = keyValueParser.parseValue(value);
        ProxySelectorData proxySelectorData = keyValueParser.parseKey(key);
        DiscoverySyncData data = new DiscoverySyncData();
        data.setUpstreamDataList(discoveryUpstreamDTOS);
        data.setProxySelectorData(proxySelectorData);
        return data;
    }

}
