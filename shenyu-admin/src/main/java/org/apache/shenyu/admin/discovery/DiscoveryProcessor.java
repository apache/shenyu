package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;

public interface DiscoveryProcessor {

    /**
     * @param discoveryDO
     */
    void createDiscovery(DiscoveryDO discoveryDO);


    /**
     * @param discoveryHandlerDTO
     * @param proxySelectorDTO
     */
    void createProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO);

    /**
     * 当 discoveryDO 和 proxySelectorDTO 任意有新的 发生改变 都会触发
     * 当然proxySelectorDTO 改变 不会update discoveryDO 监听
     *
     * @param discoveryDO
     */
    void removeDiscovery(DiscoveryDO discoveryDO);

    /**
     * 直接推送下游
     *
     * @param proxySelectorDTO
     */
    void removeProxySelector(ProxySelectorDTO proxySelectorDTO);

}
