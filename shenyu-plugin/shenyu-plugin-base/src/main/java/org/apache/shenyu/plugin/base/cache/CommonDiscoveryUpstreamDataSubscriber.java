package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class CommonDiscoveryUpstreamDataSubscriber implements DiscoveryUpstreamDataSubscriber {

    private final Map<String, DiscoveryUpstreamDataHandler> handlerMap;


    public CommonDiscoveryUpstreamDataSubscriber(final List<DiscoveryUpstreamDataHandler> discoveryUpstreamDataHandlers) {
        this.handlerMap = discoveryUpstreamDataHandlers.stream().collect(Collectors.toConcurrentMap(DiscoveryUpstreamDataHandler::pluginName, e -> e));
    }


    @Override
    public void onSubscribe(DiscoverySyncData upstreamDataList) {
        Optional.ofNullable(handlerMap.get(upstreamDataList.getPluginName()))
                .ifPresent(handler -> handler.handlerDiscoveryUpstreamData(upstreamDataList));
    }

    @Override
    public void unSubscribe(DiscoverySyncData upstreamDataList) {

    }
}
