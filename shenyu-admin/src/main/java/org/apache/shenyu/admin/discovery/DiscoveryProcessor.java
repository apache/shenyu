package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;

import java.util.List;

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

}
