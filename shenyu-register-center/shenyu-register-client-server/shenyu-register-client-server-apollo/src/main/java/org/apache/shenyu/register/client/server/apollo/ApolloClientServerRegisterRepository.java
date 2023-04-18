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

package org.apache.shenyu.register.client.server.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigService;
import com.ctrip.framework.apollo.core.ConfigConsts;
import com.ctrip.framework.apollo.model.ConfigChange;
import com.google.common.collect.Lists;
import org.apache.shenyu.common.constant.ApolloPathConstants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.spi.Join;

import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * apollo register center.
 */
@Join
public class ApolloClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private static final String APOLLO_CLUSTER = "apollo.cluster";

    private static final String PROP_APP_ID = "app.id";

    private static final String PROP_APOLLO_META = "apollo.meta";

    private static final String APOLLO_NAMESPACE = "apollo.bootstrap.namespace";

    private ShenyuClientServerRegisterPublisher publisher;

    private Config config;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher,
                     final ShenyuRegisterCenterConfig config) {
        this.publisher = publisher;
        Properties properties = config.getProps();
        String meta = config.getServerLists();
        String appId = properties.getProperty("appId");
        String clusterName = properties.getProperty("clusterName", ConfigConsts.CLUSTER_NAME_DEFAULT);
        String namespace = properties.getProperty("namespace", ConfigConsts.NAMESPACE_APPLICATION);
        Optional.ofNullable(appId).ifPresent(x -> System.setProperty(PROP_APP_ID, x));
        Optional.ofNullable(meta).ifPresent(x -> System.setProperty(PROP_APOLLO_META, x));
        Optional.ofNullable(clusterName).ifPresent(x -> System.setProperty(APOLLO_CLUSTER, x));
        Optional.ofNullable(namespace).ifPresent(x -> System.setProperty(APOLLO_NAMESPACE, x));
        this.config = ConfigService.getAppConfig();
        this.initSubscribe();
    }

    private void initSubscribe() {
        this.config.addChangeListener(changeEvent -> {
            for (String changedKey : changeEvent.changedKeys()) {
                ConfigChange configChange = changeEvent.getChange(changedKey);
                if (changedKey.startsWith(ApolloPathConstants.REGISTER_METADATA_ID)) {
                    this.publishMetadata(configChange.getNewValue());

                } else if (changedKey.startsWith(ApolloPathConstants.REGISTER_URI_ID)) {
                    this.publishRegisterURI(configChange.getNewValue());
                }
            }
        });
    }

    private void publishMetadata(final String metadata) {
        publisher.publish(Lists.newArrayList(GsonUtils.getInstance().fromJson(metadata, MetaDataRegisterDTO.class)));
    }

    @SuppressWarnings("unchecked")
    private void publishRegisterURI(final String uriMetadata) {
        List<String> metadataList = GsonUtils.getInstance().fromJson(uriMetadata, List.class);
        List<URIRegisterDTO> registerDTOList = metadataList.stream()
                .map(metadata -> GsonUtils.getInstance().fromJson(metadata, URIRegisterDTO.class))
                .collect(Collectors.toList());
        publisher.publish(registerDTOList);
    }

    @Override
    public void close() {
        this.publisher.close();
    }
}
