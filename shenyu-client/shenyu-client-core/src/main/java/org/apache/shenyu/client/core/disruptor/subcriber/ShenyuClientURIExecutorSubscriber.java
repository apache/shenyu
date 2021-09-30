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

package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;

/**
 * The type Shenyu client uri executor subscriber.
 */
public class ShenyuClientURIExecutorSubscriber implements ExecutorTypeSubscriber<URIRegisterDTO> {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClientMetadataExecutorSubscriber.class);
    
    private final ShenyuClientRegisterRepository shenyuClientRegisterRepository;
    
    /**
     * Instantiates a new Shenyu client uri executor subscriber.
     *
     * @param shenyuClientRegisterRepository the shenyu client register repository
     */
    public ShenyuClientURIExecutorSubscriber(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        this.shenyuClientRegisterRepository = shenyuClientRegisterRepository;
    }
    
    @Override
    public DataType getType() {
        return DataType.URI;
    }
    
    @Override
    public void executor(final Collection<URIRegisterDTO> dataList) {
        for (URIRegisterDTO uriRegisterDTO : dataList) {
            LOG.error("register rui is host:{}, port:{} ", uriRegisterDTO.getHost(), uriRegisterDTO.getPort());
            shenyuClientRegisterRepository.persistURI(uriRegisterDTO);
        }
    }
}
