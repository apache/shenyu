package org.apache.shenyu.plugin.sync.data.websocket.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;

import java.util.List;

public class DiscoveryUpstreamDataHandler extends AbstractDataHandler<DiscoverySyncData>{

    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;

    public DiscoveryUpstreamDataHandler(final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        this.discoveryUpstreamDataSubscribers = discoveryUpstreamDataSubscribers;
    }

    @Override
    protected List<DiscoverySyncData> convert(final String json) {
        return GsonUtils.getInstance().fromList(json, DiscoverySyncData.class);
    }

    @Override
    protected void doRefresh(final List<DiscoverySyncData> dataList) {
        dataList.forEach(data -> {
            discoveryUpstreamDataSubscribers.forEach(p -> {
                p.onSubscribe(data);
            });
        });
    }

    @Override
    protected void doUpdate(final List<DiscoverySyncData> dataList) {
        dataList.forEach(data -> {
            discoveryUpstreamDataSubscribers.forEach(p -> {
                p.onSubscribe(data);
            });
        });
    }

    @Override
    protected void doDelete(final List<DiscoverySyncData> dataList) {
        dataList.forEach(data -> {
            discoveryUpstreamDataSubscribers.forEach(p -> {
                p.unSubscribe(data);
            });
        });
    }
}
