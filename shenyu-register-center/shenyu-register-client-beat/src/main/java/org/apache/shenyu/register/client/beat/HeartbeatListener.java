package org.apache.shenyu.register.client.beat;

import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.web.servlet.context.ServletWebServerInitializedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.EventListener;

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class HeartbeatListener{

    private static final Logger LOG = LoggerFactory.getLogger(HeartbeatListener.class);

    private ScheduledThreadPoolExecutor executor;

    private final ShenyuClientRegisterRepository httpClientRegisterRepository;

    private final ShenyuConfig shenyuConfig;

    public HeartbeatListener(final ShenyuClientRegisterRepository httpClientRegisterRepository, final ShenyuConfig shenyuConfig, final ServerProperties serverProperties) {
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-task", false));
        this.httpClientRegisterRepository = httpClientRegisterRepository;
        this.shenyuConfig = shenyuConfig;
        LOG.info("Web server initialized on port {}, starting heartbeat reporter", serverProperties.getPort());
        //启动心跳任务
        executor.scheduleAtFixedRate(() -> {
                    InstanceBeatInfoDTO instanceBeatInfoDTO = new InstanceBeatInfoDTO();
                    instanceBeatInfoDTO.setInstancePort(serverProperties.getPort()+"");
                    instanceBeatInfoDTO.setInstanceIp(IpUtils.getHost());
                    instanceBeatInfoDTO.setNamespaceId(shenyuConfig.getNamespace());
                    instanceBeatInfoDTO.setInstanceInfo(SystemInfoUtils.getSystemInfo());
                    instanceBeatInfoDTO.setInstanceType(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO);
                    httpClientRegisterRepository.sendHeartbeat(instanceBeatInfoDTO);
                },
                0,
                5,
                TimeUnit.SECONDS
        );
    }

    @EventListener(ContextClosedEvent.class)
    public void onShutdown() {
        executor.shutdown();
        try {
            if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}