package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;

public interface DiscoveryHandler {

    /**
     * 当 discoveryDO 和 proxySelectorDTO 任意有新的 发生改变 都会触发
     * 当然proxySelectorDTO 改变 不会新增 discoveryDO 监听
     *
     * @param discoveryDO
     * @param proxySelectorDTO
     */
    void create(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO);


    /**
     * 当 discoveryDO 和 proxySelectorDTO 任意有新的 发生改变 都会触发
     * 当然proxySelectorDTO 改变 不会update discoveryDO 监听
     *
     * @param discoveryDO
     * @param proxySelectorDTO
     */
    void update(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO);


    /**
     * 当 discoveryDO 和 proxySelectorDTO 任意有新的 发生改变 都会触发
     * 当然proxySelectorDTO 改变 不会update discoveryDO 监听
     *
     * @param discoveryDO
     * @param proxySelectorDTO
     */
    void remove(DiscoveryDO discoveryDO, ProxySelectorDTO proxySelectorDTO);


}
