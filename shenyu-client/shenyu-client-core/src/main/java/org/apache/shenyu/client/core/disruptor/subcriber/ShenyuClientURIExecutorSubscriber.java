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

import com.google.common.base.Stopwatch;
import org.apache.shenyu.client.core.shutdown.ShenyuClientShutdownHook;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.Socket;
import java.util.Collection;
import java.util.concurrent.TimeUnit;

/**
 * The type Shenyu client uri executor subscriber.
 */
public class ShenyuClientURIExecutorSubscriber implements ExecutorTypeSubscriber<URIRegisterDTO> {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClientURIExecutorSubscriber.class);
    
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
            Stopwatch stopwatch = Stopwatch.createStarted();
            while (true) {
                try (Socket ignored = new Socket(uriRegisterDTO.getHost(), uriRegisterDTO.getPort())) {
                    break;
                } catch (IOException e) {
                    long sleepTime = 1000;
                    // maybe the port is delay exposed
                    if (stopwatch.elapsed(TimeUnit.SECONDS) > 5) {
                        LOG.error("host:{}, port:{} connection failed, will retry",
                                uriRegisterDTO.getHost(), uriRegisterDTO.getPort());
                        // If the connection fails for a long time, Increase sleep time
                        if (stopwatch.elapsed(TimeUnit.SECONDS) > 180) {
                            sleepTime = 10000;
                        }
                    }
                    try {
                        TimeUnit.MILLISECONDS.sleep(sleepTime);
                    } catch (InterruptedException ex) {
                        LOG.error("interrupted when sleep", ex);
                    }
                }
            }
            ShenyuClientShutdownHook.delayOtherHooks();
            shenyuClientRegisterRepository.persistURI(uriRegisterDTO);
        }
    }
}
