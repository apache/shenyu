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

package org.apache.shenyu.register.instance.eureka;

import com.netflix.appinfo.ApplicationInfoManager;
import com.netflix.appinfo.DataCenterInfo;
import com.netflix.appinfo.EurekaInstanceConfig;
import com.netflix.appinfo.InstanceInfo;
import com.netflix.appinfo.MyDataCenterInfo;
import com.netflix.appinfo.MyDataCenterInstanceConfig;
import com.netflix.appinfo.providers.EurekaConfigBasedInstanceInfoProvider;
import com.netflix.config.ConfigurationManager;
import com.netflix.discovery.DefaultEurekaClientConfig;
import com.netflix.discovery.DiscoveryClient;
import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.shared.transport.EurekaHttpClient;
import com.netflix.discovery.shared.transport.EurekaHttpResponse;
import com.netflix.discovery.shared.transport.jersey.JerseyApplicationClient;
import com.sun.jersey.client.apache4.ApacheHttpClient4;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.register.instance.api.watcher.WatcherListener;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Join
public class EurekaInstanceRegisterRepository implements ShenyuInstanceRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(EurekaInstanceRegisterRepository.class);

    private EurekaClient eurekaClient;

    private EurekaHttpClient eurekaHttpClient;

    private final List<String> watcher = new ArrayList<>();

    @Override
    public void init(final RegisterConfig config) {
        ConfigurationManager.getConfigInstance().setProperty("eureka.client.service-url.defaultZone", config.getServerLists());
        ConfigurationManager.getConfigInstance().setProperty("eureka.serviceUrl.default", config.getServerLists());
        ApplicationInfoManager applicationInfoManager = initializeApplicationInfoManager(new MyDataCenterInstanceConfig());
        eurekaClient = new DiscoveryClient(applicationInfoManager, new DefaultEurekaClientConfig());
        eurekaHttpClient = new JerseyApplicationClient(new ApacheHttpClient4(), config.getServerLists(), null);
    }

    private ApplicationInfoManager initializeApplicationInfoManager(final EurekaInstanceConfig instanceConfig) {
        InstanceInfo instanceInfo = new EurekaConfigBasedInstanceInfoProvider(instanceConfig).get();
        return new ApplicationInfoManager(instanceConfig, instanceInfo);
    }

    @Override
    public void persistInstance(final InstanceEntity instance) {
        EurekaHttpResponse<Void> register = eurekaHttpClient.register(generateInstanceInfo(instance));
        LOGGER.info("eureka client register success: {}", register.getEntity());
    }

    private InstanceInfo generateInstanceInfo(final InstanceEntity instance) {
        return InstanceInfo.Builder.newBuilder()
                .setAppName(instance.getAppName())
                .setIPAddr(IpUtils.getHost())
                .setHostName(instance.getHost())
                .setPort(instance.getPort())
                .setDataCenterInfo(new MyDataCenterInfo(DataCenterInfo.Name.MyOwn))
                .build();
    }

    @Override
    public List<InstanceEntity> selectInstancesAndWatcher(final String selectKey, final WatcherListener watcherListener) {
        if (!watcher.contains(selectKey)) {
            synchronized (this) {
                if (!watcher.contains(selectKey)) {
                    watcher.add(selectKey);
                    eurekaClient.registerEventListener(event -> watcherListener.listener(getInstances(selectKey)));
                }
            }
        }
        return getInstances(selectKey);
    }

    private List<InstanceEntity> getInstances(final String selectKey) {
        List<InstanceInfo> instances = eurekaClient.getInstancesByVipAddressAndAppName(null, selectKey, true);
        return instances.stream()
                .map(i -> InstanceEntity.builder()
                        .appName(i.getAppName()).host(i.getHostName()).port(i.getPort())
                        .build()
                ).collect(Collectors.toList());
    }

    @Override
    public void close() {
        eurekaClient.shutdown();
    }
}
