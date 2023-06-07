package org.apache.shenyu.admin.discovery;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.springframework.beans.BeanUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * DiscoveryProcessor.
 */
public interface DiscoveryProcessor {

    /**
     * createDiscovery.
     *
     * @param discoveryDO discoveryDO
     */
    void createDiscovery(DiscoveryDO discoveryDO);

    /**
     * createProxySelector.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @param proxySelectorDTO    proxySelectorDTO
     */
    void createProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO);

    /**
     * removeDiscovery.
     *
     * @param discoveryDO discoveryDO
     */
    void removeDiscovery(DiscoveryDO discoveryDO);

    /**
     * removeProxySelector.
     *
     * @param proxySelectorDTO proxySelectorDTO
     */
    void removeProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO);


    /**
     * only use in local mode to sync upstreamDTOS.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @param proxySelectorDTO    proxySelectorDTO
     * @param upstreamDTOS        upstreamDTOS
     */
    void changeUpstream(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO, List<DiscoveryUpstreamDTO> upstreamDTOS);

    default DiscoverySyncData covert(final ProxySelectorDTO proxySelectorDTO, List<DiscoveryUpstreamDTO> upstreamDTOS) {
        ProxySelectorData proxySelectorData = new ProxySelectorData();
        BeanUtils.copyProperties(proxySelectorDTO, proxySelectorData);
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setProxySelectorData(proxySelectorData);
        if (CollectionUtils.isNotEmpty(upstreamDTOS)) {
            List<DiscoveryUpstreamData> collect = upstreamDTOS.stream().map(up -> {
                DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
                BeanUtils.copyProperties(up, discoveryUpstreamData);
                return discoveryUpstreamData;
            }).collect(Collectors.toList());
            discoverySyncData.setUpstreamDataList(collect);
        }
        return discoverySyncData;
    }

}
