/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.scale.scaler;

import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.admin.scale.config.ScaleProperties;
import org.apache.shenyu.admin.scale.monitor.subject.MetricsMonitor;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.apache.shenyu.admin.scale.scaler.dynamic.TaskSchedulerManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Comparator;


@Component
public class ScaleService {

    private static final String DYNAMIC_TASK_NAME = "DynamicScalingTask";

    private static final Logger LOG = LoggerFactory.getLogger(ScaleService.class);

    private final TaskSchedulerManager schedulerManager;

    private final MetricsMonitor metricsMonitor;

    private final ScalePolicyCache scalePolicyCache;

    private final ScaleProperties scaleProperties;

    private final KubernetesScaler kubernetesScaler;

    public ScaleService(final MetricsMonitor metricsMonitor,
                        final TaskSchedulerManager schedulerManager,
                        final ScalePolicyCache scalePolicyCache,
                        final ScaleProperties scaleProperties,
                        final KubernetesScaler kubernetesScaler) {
        this.schedulerManager = schedulerManager;
        this.metricsMonitor = metricsMonitor;
        this.scalePolicyCache = scalePolicyCache;
        this.scaleProperties = scaleProperties;
        this.kubernetesScaler = kubernetesScaler;
    }

    /**
     * execute scaling.
     */
    public void executeScaling() {
        ScalePolicyDO activePolicy = getActivePolicy();
        if (activePolicy != null) {
            switch (activePolicy.getId()) {
                case "1":
                    stopDynamicTask();
                    kubernetesScaler.scaleByNum(activePolicy.getNum());
                    break;
                case "2":
                    stopDynamicTask();
                    if (isWithinTimeRange(activePolicy)) {
                        kubernetesScaler.scaleByNum(activePolicy.getNum());
                    }
                    break;
                case "3":
                    startDynamicTask(scaleProperties.getMonitorInterval());
                    break;
                default:
                    stopDynamicTask();
                    throw new IllegalStateException("Unknown scaling policy: " + activePolicy.getId());
            }
        }
    }

    /**
     * get active policy.
     *
     * @return ScalePolicyDO
     */
    private ScalePolicyDO getActivePolicy() {
        return scalePolicyCache.getAllPolicies().stream()
                .filter(policy -> policy.getStatus() == 1)
                .min(Comparator.comparingInt(ScalePolicyDO::getSort))
                .orElseGet(() -> {
                    LOG.warn("No active scaling policy found.");
                    return null;
                });
    }

    /**
     * isWithinTimeRange.
     *
     * @param policy policy
     * @return boolean
     */
    private boolean isWithinTimeRange(final ScalePolicyDO policy) {
        Date now = new Date();
        return now.after(policy.getBeginTime()) && now.before(policy.getEndTime());
    }

    /**
     * execute dynamic scaling.
     */
    private void executeDynamicScaling() {
        try {
            metricsMonitor.monitorMetrics();
        } catch (Exception e) {
            LOG.error("Failed to execute dynamic scale. cause: {} ", e.getMessage());
        }
    }

    /**
     * startDynamicTask.
     *
     * @param interval interval
     */
    public void startDynamicTask(final long interval) {
        schedulerManager.registerMonitorTask(DYNAMIC_TASK_NAME, this::executeDynamicScaling, interval);
    }

    /**
     * stopDynamicTask.
     */
    public void stopDynamicTask() {
        schedulerManager.cancelMonitorTask(DYNAMIC_TASK_NAME);
    }
}
