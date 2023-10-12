package org.apache.shenyu.plugin.grpc.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.handler.DiscoveryUpstreamDataHandler;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.springframework.util.ObjectUtils;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * GrpcDiscoveryUpstreamDataHandler.
 */
public class GrpcDiscoveryUpstreamDataHandler implements DiscoveryUpstreamDataHandler {

    @Override
    public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
        if (Objects.isNull(discoverySyncData) || Objects.isNull(discoverySyncData.getSelectorId())) {
            return;
        }
        ApplicationConfigCache.getInstance().handlerUpstream(discoverySyncData.getSelectorId(), convertUpstreamList(discoverySyncData.getUpstreamDataList()));
    }

    private List<GrpcUpstream> convertUpstreamList(final List<DiscoveryUpstreamData> upstreamList) {
        if (ObjectUtils.isEmpty(upstreamList)) {
            return Collections.emptyList();
        }
        return upstreamList.stream().map(u -> GrpcUpstream.builder()
                .protocol(u.getProtocol())
                .upstreamUrl(u.getUrl())
                .weight(u.getWeight())
                .status(0 == u.getStatus())
                .timestamp(u.getDateUpdated().getTime())
                .build()).collect(Collectors.toList());
    }

    @Override
    public String pluginName() {
        return PluginEnum.GRPC.getName();
    }
}
