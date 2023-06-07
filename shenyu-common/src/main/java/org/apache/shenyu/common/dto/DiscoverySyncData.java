package org.apache.shenyu.common.dto;

import java.util.List;

public class DiscoverySyncData {

    private ProxySelectorData proxySelectorData;

    private List<DiscoveryUpstreamData> upstreamDataList;

    /**
     * getProxySelectorData.
     *
     * @return proxySelectorData
     */
    public ProxySelectorData getProxySelectorData() {
        return proxySelectorData;
    }

    /**
     * setProxySelectorData.
     *
     * @param proxySelectorData proxySelectorData
     */
    public void setProxySelectorData(final ProxySelectorData proxySelectorData) {
        this.proxySelectorData = proxySelectorData;
    }

    /**
     * getUpstreamDataList.
     *
     * @return upstreamDataList
     */
    public List<DiscoveryUpstreamData> getUpstreamDataList() {
        return upstreamDataList;
    }

    /**
     * setUpstreamDataList.
     *
     * @param upstreamDataList upstreamDataList
     */
    public void setUpstreamDataList(List<DiscoveryUpstreamData> upstreamDataList) {
        this.upstreamDataList = upstreamDataList;
    }

}
