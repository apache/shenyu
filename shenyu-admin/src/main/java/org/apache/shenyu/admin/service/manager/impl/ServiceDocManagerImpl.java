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

package org.apache.shenyu.admin.service.manager.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import javax.annotation.Resource;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.admin.service.manager.ServiceDocManager;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * ServiceDocManagerImpl.
 */
@Service
public class ServiceDocManagerImpl implements ServiceDocManager {
    private static final Logger LOG = LoggerFactory.getLogger(ServiceDocManagerImpl.class);

    private static final HttpUtils HTTP_UTILS = new HttpUtils();

    private static final Map<String, Long> CLUSTER_LASTSTARTUPTIME_MAP = new HashMap<>();

    private static final String SWAGGER_V2_PATH = "/v2/api-docs";

    @Resource
    private DocManager docManager;

    @Override
    public void pullApiDocument(final Set<UpstreamInstance> currentServices) {
        currentServices.forEach(this::pullApiDocument);
    }

    /**
     * pullApiDocument.
     *
     * @param instance UpstreamInstance.
     */
    @Override
    @SuppressWarnings("unchecked")
    public void pullApiDocument(final UpstreamInstance instance) {
        String clusterName = instance.getClusterName();
        if (!canPull(instance)) {
            LOG.info("api document has been pulled and cannot be pulled againl，instance={}", JsonUtils.toJson(instance));
            return;
        }
        String url = getSwaggerRequestUrl(instance);
        try {
            String body = HTTP_UTILS.get(url, Collections.EMPTY_MAP);
            docManager.addDocInfo(
                clusterName,
                body,
                callback -> LOG.info("load api document successful，clusterName={}, iPandPort={}",
                    clusterName, instance.getIp() + ":" + instance.getPort())
            );
            CLUSTER_LASTSTARTUPTIME_MAP.put(clusterName, instance.getStartupTime());
        } catch (Exception e) {
            LOG.error("add api document fail. url={} error={}", url, e);
        }
    }

    private boolean canPull(final UpstreamInstance instance) {
        boolean canPull = false;
        Long cacheLastStartUpTime = CLUSTER_LASTSTARTUPTIME_MAP.get(instance.getClusterName());
        if (Objects.isNull(cacheLastStartUpTime) || instance.getStartupTime() > cacheLastStartUpTime) {
            canPull = true;
        }
        return canPull;
    }

    private String getSwaggerRequestUrl(final UpstreamInstance instance) {
        return "http://" + instance.getIp() + ":" + instance.getPort() + SWAGGER_V2_PATH;

    }

}
