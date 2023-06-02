package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;

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
    void removeProxySelector(ProxySelectorDTO proxySelectorDTO);

}
