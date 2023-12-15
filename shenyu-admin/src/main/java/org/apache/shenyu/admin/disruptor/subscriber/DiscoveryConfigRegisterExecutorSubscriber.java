package org.apache.shenyu.admin.disruptor.subscriber;

import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;

import java.util.Collection;

public class DiscoveryConfigRegisterExecutorSubscriber implements ExecutorTypeSubscriber<DiscoveryConfigRegisterDTO> {


    private DiscoveryService discoveryService;


    /**
     * DiscoveryConfigRegisterExecutorSubscriber.
     *
     * @param discoveryService discoveryService
     */
    public DiscoveryConfigRegisterExecutorSubscriber(final DiscoveryService discoveryService) {
        this.discoveryService = discoveryService;
    }

    @Override
    public void executor(Collection<DiscoveryConfigRegisterDTO> discoveryConfigRegisterDTOS) {
        discoveryConfigRegisterDTOS.forEach(discoveryConfigRegisterDTO -> {
            discoveryService.registerDiscoveryConfig(discoveryConfigRegisterDTO);
        });
    }

    @Override
    public DataType getType() {
        return DataType.DISCOVERY_CONFIG;
    }
}
