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

package org.apache.shenyu.register.instance.polaris;

import com.tencent.polaris.api.config.Configuration;
import com.tencent.polaris.api.core.ConsumerAPI;
import com.tencent.polaris.api.core.ProviderAPI;
import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.api.pojo.Instance;
import com.tencent.polaris.api.rpc.GetHealthyInstancesRequest;
import com.tencent.polaris.api.rpc.InstanceRegisterRequest;
import com.tencent.polaris.api.rpc.InstancesResponse;
import com.tencent.polaris.api.rpc.WatchServiceRequest;
import com.tencent.polaris.api.rpc.WatchServiceResponse;
import com.tencent.polaris.client.api.SDKContext;
import com.tencent.polaris.factory.ConfigAPIFactory;
import com.tencent.polaris.factory.api.DiscoveryAPIFactory;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import shade.polaris.io.netty.handler.codec.http.HttpScheme;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The type Polaris instance register repository.
 */
@Join
public class PolarisInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(PolarisInstanceRegisterRepository.class);

    /**
     * Polaris namespace.
     */
    private static final String NAMESPACE = "polarisNameSpace";

    /**
     * Polaris ttl.
     */
    private static final String DEFAULT_TTL = "5";

    private Properties props;

    private String namespace;

    private ProviderAPI providerAPI;

    private ConsumerAPI consumerAPI;

    /**
     * init repository.
     *
     * @param config the config
     */
    @Override
    public void init(final RegisterConfig config) {
        Configuration configuration = buildConfiguration(config);
        SDKContext sdkContext = SDKContext.initContextByConfig(configuration);
        this.providerAPI = DiscoveryAPIFactory.createProviderAPIByContext(sdkContext);
        this.consumerAPI = DiscoveryAPIFactory.createConsumerAPIByContext(sdkContext);
        this.props = config.getProps();
        this.namespace = props.getProperty("namespace", NAMESPACE);
    }

    /**
     * build polaris configuration.
     *
     * @param config config
     * @return Configuration
     */
    private Configuration buildConfiguration(final RegisterConfig config) {
        String serverLists = config.getServerLists();
        return ConfigAPIFactory.createConfigurationByAddress(serverLists);
    }

    /**
     * persist instance.
     *
     * @param instance instance
     */
    @Override
    public void persistInstance(final InstanceEntity instance) {

        final String token = props.getProperty("token", null);
        final String version = props.getProperty("version", null);
        final String weight = props.getProperty("weight", "0");
        final String ttl = props.getProperty("ttl", DEFAULT_TTL);

        // build register info
        InstanceRegisterRequest registerRequest = new InstanceRegisterRequest();
        registerRequest.setNamespace(namespace);
        registerRequest.setService(instance.getAppName());
        registerRequest.setHost(instance.getHost());
        registerRequest.setPort(instance.getPort());

        registerRequest.setProtocol(HttpScheme.HTTPS.toString());
        registerRequest.setTtl(Integer.valueOf(ttl));
        registerRequest.setToken(token);
        registerRequest.setVersion(version);
        registerRequest.setWeight(Integer.valueOf(weight));

        // register service
        try {
            providerAPI.registerInstance(registerRequest);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
        LOGGER.info("polaris client register instance success: {}", instance);
    }

    /**
     * select and watch instances.
     *
     * @param selectKey       selectKey
     * @param watcherListener watcherListener
     * @return List
     */
    @Override
    public List<InstanceEntity> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {

        final WatchServiceRequest watcherRequest = WatchServiceRequest.builder()
                .service(selectKey)
                .namespace(namespace)
                .listeners(Collections.singletonList(event -> watcherListener.listener(transformInstances(event.getAllInstances()))))
                .build();

        Instance[] instances;
        try {
            final WatchServiceResponse watchServiceResponse = consumerAPI.watchService(watcherRequest);
            watchServiceResponse.getResponse().getInstances();
            instances = watchServiceResponse.getResponse().getInstances();
        } catch (PolarisException e) {
            throw new ShenyuException(e);
        }
        if (Objects.isNull(instances)) {
            try {
                GetHealthyInstancesRequest req = new GetHealthyInstancesRequest();
                req.setService(selectKey);
                req.setNamespace(namespace);
                final InstancesResponse response = consumerAPI.getHealthyInstances(req);
                instances = response.getInstances();
            } catch (PolarisException e) {
                // ignore
                LOGGER.error("polaris client select instances failed: {}", e.getMessage());
            }
        }
        if (Objects.isNull(instances)) {
            return Collections.emptyList();
        }
        return transformInstances(Arrays.asList(instances));
    }

    /**
     * transform to instance entities.
     *
     * @param instances instances
     * @return List
     */
    private List<InstanceEntity> transformInstances(final List<Instance> instances) {
        if (CollectionUtils.isEmpty(instances)) {
            return Collections.emptyList();
        }
        return instances.stream()
                .map(instance -> InstanceEntity.builder()
                        .appName(instance.getService())
                        .host(instance.getHost())
                        .port(instance.getPort())
                        .build())
                .collect(Collectors.toList());
    }

    @Override
    public void close() {
        try {
            providerAPI.close();
            consumerAPI.close();
        } catch (PolarisException e) {
            throw new ShenyuException(e);
        }
    }
    
}
