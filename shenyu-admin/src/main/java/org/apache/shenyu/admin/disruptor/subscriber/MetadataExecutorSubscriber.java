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

package org.apache.shenyu.admin.disruptor.subscriber;

import org.apache.shenyu.admin.service.register.ClientRegisterProvisioner;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * The type Metadata executor subscriber.
 */
public class MetadataExecutorSubscriber implements ExecutorTypeSubscriber<MetaDataRegisterDTO> {

    private static final Logger LOG = LoggerFactory.getLogger(MetadataExecutorSubscriber.class);

    private final Map<String, ShenyuClientRegisterService> shenyuClientRegisterService;

    private final List<ClientRegisterProvisioner> clientRegisterProvisioners;

    public MetadataExecutorSubscriber(final Map<String, ShenyuClientRegisterService> shenyuClientRegisterService) {
        this(shenyuClientRegisterService, Collections.emptyList());
    }

    public MetadataExecutorSubscriber(final Map<String, ShenyuClientRegisterService> shenyuClientRegisterService,
                                      final List<ClientRegisterProvisioner> clientRegisterProvisioners) {
        this.shenyuClientRegisterService = shenyuClientRegisterService;
        this.clientRegisterProvisioners = Objects.isNull(clientRegisterProvisioners)
                ? Collections.emptyList() : clientRegisterProvisioners;
    }

    @Override
    public DataType getType() {
        return DataType.META_DATA;
    }

    @Override
    public void executor(final Collection<MetaDataRegisterDTO> metaDataRegisterDTOList) {
        metaDataRegisterDTOList.forEach(meta -> Optional.ofNullable(this.shenyuClientRegisterService.get(meta.getRpcType()))
                .ifPresent(shenyuClientRegisterService -> {
                    synchronized (shenyuClientRegisterService) {
                        shenyuClientRegisterService.register(meta);
                    }
                    runProvisioners(meta);
                }));
    }

    private void runProvisioners(final MetaDataRegisterDTO metaDataRegisterDTO) {
        for (ClientRegisterProvisioner provisioner : clientRegisterProvisioners) {
            try {
                if (provisioner.supports(metaDataRegisterDTO)) {
                    provisioner.provision(metaDataRegisterDTO);
                }
            } catch (Exception ex) {
                LOG.warn("client register provisioner [{}] failed: {}",
                        provisioner.pluginName(), ex.getMessage(), ex);
            }
        }
    }
}
