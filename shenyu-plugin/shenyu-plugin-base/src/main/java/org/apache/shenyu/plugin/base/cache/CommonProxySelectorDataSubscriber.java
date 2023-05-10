package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.convert.selector.DiscoveryUpstream;
import org.apache.shenyu.plugin.base.handler.ProxySelectorDataHandler;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * CommonProxySelectorDataSubscriber.
 */
public class CommonProxySelectorDataSubscriber implements ProxySelectorDataSubscriber {

    private final Map<String, ProxySelectorDataHandler> handlerMap;

    public CommonProxySelectorDataSubscriber(final List<ProxySelectorDataHandler> proxySelectorDataHandlerList) {
        this.handlerMap = proxySelectorDataHandlerList.stream().collect(Collectors.toConcurrentMap(ProxySelectorDataHandler::name, e -> e));
    }

    @Override
    public void onSubscribe(ProxySelectorData proxySelectorData, List<DiscoveryUpstream> upstreamsList) {
        Optional.ofNullable(handlerMap.get(proxySelectorData.getName()))
                .ifPresent(handler -> handler.handlerProxySelector(proxySelectorData, upstreamsList));
    }

    @Override
    public void unSubscribe(ProxySelectorData proxySelectorData) {
        Optional.ofNullable(handlerMap.get(proxySelectorData.getName()))
                .ifPresent(handler -> handler.removeProxySelector(proxySelectorData.getName()));
    }

    @Override
    public void refresh() {
        ProxySelectorDataSubscriber.super.refresh();
    }
}
