package org.apache.shenyu.admin.service.impl;

import com.google.common.collect.Lists;
import jakarta.annotation.PreDestroy;
import org.apache.shenyu.admin.model.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * This is the client check service.
 */
@Component
public class InstanceCheckService {

    private static final Logger LOG = LoggerFactory.getLogger(UpstreamCheckService.class);

    private ScheduledThreadPoolExecutor executor;

    private ScheduledFuture<?> scheduledFuture;

    private final int scheduledTime;

    private ConcurrentHashMap<String, InstanceInfoVO> instanceHealthBeatInfo;

    private long instanceHeartBeatTimeOut;

    private long deleteTimeout;

    private InstanceInfoService instanceInfoService;


    public InstanceCheckService(InstanceInfoService instanceInfoService) {
        this.scheduledTime = 500;
        this.instanceHealthBeatInfo = new ConcurrentHashMap<>();
        this.instanceHeartBeatTimeOut = 500;
        this.deleteTimeout = 500;
        this.instanceInfoService = instanceInfoService;
    }

    /**
     * Set up.
     */
    public void setup() {
        this.fetchInstanceData();
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-task", false));
        scheduledFuture = executor.scheduleWithFixedDelay(this::scheduled, 10, scheduledTime, TimeUnit.SECONDS);
    }

    /**
     * fetch instance status data from db.
     */
    public void fetchInstanceData() {
        List<InstanceInfoVO> list = instanceInfoService.list();
        list.forEach(instanceInfoVO -> {
            String instanceKey = getInstanceKey(instanceInfoVO);
            instanceHealthBeatInfo.put(instanceKey,instanceInfoVO);
        });
    }

    public String getInstanceKey(InstanceInfoVO instanceInfoVO){
        return instanceInfoVO.getInstanceIp()+":"+instanceInfoVO.getInstancePort()+"@"+instanceInfoVO.getInstanceType()+"#"+instanceInfoVO.getNamespaceId();
    }

    public String getInstanceKey(InstanceBeatInfoDTO instanceBeatInfoDTO){
        return instanceBeatInfoDTO.getInstanceIp()+":"+instanceBeatInfoDTO.getInstancePort()+"@"+instanceBeatInfoDTO.getInstanceType()+"#"+instanceBeatInfoDTO.getNamespaceId();
    }

    public InstanceInfoVO getInstanceHealthBeatInfo(InstanceBeatInfoDTO instanceBeatInfoDTO){
        return instanceHealthBeatInfo.get(getInstanceKey(instanceBeatInfoDTO));
    }

    public void handleBeatInfo(InstanceBeatInfoDTO instanceBeatInfoDTO){
        String instanceKey = getInstanceKey(instanceBeatInfoDTO);
        if (instanceHealthBeatInfo.contains(instanceKey)){
            InstanceInfoVO instanceInfoVO = instanceHealthBeatInfo.get(instanceKey);
            instanceInfoVO.setLastHeartBeatTime(System.currentTimeMillis());
        }else {
            InstanceInfoVO instanceInfoVO = new InstanceInfoVO();
            instanceInfoVO.setInstanceIp(instanceBeatInfoDTO.getInstanceIp());
            instanceInfoVO.setInstanceState(1);
            instanceInfoVO.setInstanceInfo(instanceBeatInfoDTO.getInstanceInfo());
            instanceInfoVO.setInstanceType(instanceBeatInfoDTO.getInstanceType());
            instanceInfoVO.setLastHeartBeatTime(System.currentTimeMillis());
            instanceInfoVO.setInstancePort(instanceBeatInfoDTO.getInstancePort());
            instanceInfoVO.setNamespaceId(instanceBeatInfoDTO.getNamespaceId());
            instanceHealthBeatInfo.put(instanceKey,instanceInfoVO);
        }
    }

    private void scheduled() {
        try {
            doCheck();
        } catch (Exception e) {
            LOG.error("upstream scheduled check error -------- ", e);
        }
    }

    private void doCheck() {
        instanceHealthBeatInfo.values().forEach(instance -> {
            if (System.currentTimeMillis() - instance.getLastHeartBeatTime() > instanceHeartBeatTimeOut) {
                if (1 == instance.getInstanceState()) {
                    LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} offline!", instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                    instance.setInstanceState(2);
                }
            }
            if (System.currentTimeMillis() - instance.getLastHeartBeatTime() > deleteTimeout) {
                if (2 == instance.getInstanceState()) {
                    LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} deleted!", instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                    instance.setInstanceState(0);
                }
            }
        });
    }

    public void syncDB(){
        instanceHealthBeatInfo.values().forEach(vo->{
            instanceInfoService.createOrUpdate(vo);
        });
    }
    /**
     * Close relative resource on container destroy.
     */
    @PreDestroy
    public void close() {
        syncDB();
        executor.shutdown();
    }
}
