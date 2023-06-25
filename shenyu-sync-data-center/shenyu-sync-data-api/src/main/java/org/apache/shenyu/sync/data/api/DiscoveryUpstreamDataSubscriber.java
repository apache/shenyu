package org.apache.shenyu.sync.data.api;

import org.apache.shenyu.common.dto.DiscoverySyncData;


public interface DiscoveryUpstreamDataSubscriber {

    /**
     * On subscribe.
     *
     * @param upstreamDataList the discoveryUpstream data
     */
    void onSubscribe(DiscoverySyncData upstreamDataList);

    /**
     * Un subscribe.
     *
     * @param upstreamDataList the upstreamData data
     */
    void unSubscribe(DiscoverySyncData upstreamDataList);

    /**
     * Refresh.
     */
    default void refresh() {
    }

}
