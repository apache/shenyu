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

package org.apache.shenyu.registry.polaris;

import com.tencent.polaris.api.config.Configuration;
import com.tencent.polaris.api.core.ConsumerAPI;
import com.tencent.polaris.api.core.ProviderAPI;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.GetHealthyInstancesRequest;
import com.tencent.polaris.api.rpc.InstanceRegisterRequest;
import com.tencent.polaris.api.rpc.InstancesResponse;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.factory.ConfigAPIFactory;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.Join;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The type polaris instance register repository.
 */
@Join
public class PolarisInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(PolarisInstanceRegisterRepository.class);

    private ConsumerAPI consumerAPI;

    private ProviderAPI providerAPI;

    private String namespace;

    /**
     * Init.
     * @param config the config
     */
    @Override
    public void init(final RegisterConfig config) {
        Configuration configuration = buildConfiguration(config);
        SDKContext sdkContext = SDKContext.initContextByConfig(configuration);

        this.consumerAPI = DiscoveryAPIFactory.createConsumerAPIByContext(sdkContext);
        this.providerAPI = DiscoveryAPIFactory.createProviderAPIByContext(sdkContext);
        this.namespace = config.getProps().getProperty(Constants.NAMESPACE, PolarisPathConstants.NAMESPACE);
    }

    private Configuration buildConfiguration(final RegisterConfig config) {
        String serverLists = config.getServerLists();
        return ConfigAPIFactory.createConfigurationByAddress(serverLists);
    }

    /**
     * Persist instance.
     * @param instance instance
     */
    @Override
    public void persistInstance(final InstanceEntity instance) {
        final InstanceRegisterRequest req = new InstanceRegisterRequest();
        req.setInstanceId(buildInstanceNodeName(instance));
        req.setWeight(1);
        req.setHost(instance.getHost());
        req.setPort(instance.getPort());
        req.setService(instance.getAppName());
        req.setNamespace(namespace);
        try {
            providerAPI.registerInstance(req);
        } catch (PolarisException e) {
            throw new ShenyuException(e);
        }
    }

    private String buildInstanceNodeName(final InstanceEntity instance) {
        String host = instance.getHost();
        int port = instance.getPort();
        return String.join(Constants.COLONS, host, Integer.toString(port));
    }

    /**
     * selectInstances.
     * @param selectKey selectKey
     * @return {@link List}
     */
    @Override
    public List<InstanceEntity> selectInstances(final String selectKey) {
        List<InstanceEntity> result = new ArrayList<>();
        final GetHealthyInstancesRequest req = new GetHealthyInstancesRequest();
        req.setService(selectKey);
        req.setNamespace(namespace);
        try {
            final InstancesResponse response = consumerAPI.getHealthyInstances(req);
            Arrays.stream(response.getInstances()).sequential().forEach(instance -> result.add(convertFromInstance(instance)));
        } catch (Exception e) {
            LOGGER.error("getInstanceRegisterDTOS error", e);
        }
        return result;
    }

    private InstanceEntity convertFromInstance(final Instance instance) {
        InstanceEntity instanceEntity = new InstanceEntity();
        instanceEntity.setPort(instance.getPort());
        instanceEntity.setHost(instance.getHost());
        instanceEntity.setAppName(instance.getService());
        instanceEntity.setUri(URI.create(String.format("%s://%s:%s", instance.getProtocol(), instance.getHost(), instance.getPort())));
        return instanceEntity;
    }

    /**
     * Close.
     */
    @Override
    public void close() {
        try {
            consumerAPI.close();
            providerAPI.close();
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }
}
