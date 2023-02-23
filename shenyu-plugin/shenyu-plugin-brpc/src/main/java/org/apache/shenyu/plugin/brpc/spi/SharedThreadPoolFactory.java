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

package org.apache.shenyu.plugin.brpc.spi;

import com.baidu.cloud.starlight.api.common.URI;
import com.baidu.cloud.starlight.api.rpc.RpcService;
import com.baidu.cloud.starlight.api.rpc.config.ServiceConfig;
import com.baidu.cloud.starlight.api.rpc.threadpool.NamedThreadFactory;
import com.baidu.cloud.starlight.api.rpc.threadpool.ThreadPoolFactory;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *  shared threadpool factory.
 */
public class SharedThreadPoolFactory implements ThreadPoolFactory {
    private static final Logger LOGGER = LoggerFactory.getLogger(SharedThreadPoolFactory.class);

    private ThreadPoolExecutor defaultThreadPool;

    private final Map<RpcService, ThreadPoolExecutor> threadPoolMap = new ConcurrentHashMap();

    public SharedThreadPoolFactory() {
    }

    @Override
    public void initDefaultThreadPool(final URI uri, final String threadPrefix) {
        this.defaultThreadPool = SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class);
    }

    @Override
    public ThreadPoolExecutor getThreadPool(final RpcService rpcService) {
        if (rpcService == null) {
            return this.defaultThreadPool;
        } else if (this.threadPoolMap.get(rpcService) != null) {
            return (ThreadPoolExecutor) this.threadPoolMap.get(rpcService);
        } else {
            ServiceConfig serviceConfig = rpcService.getServiceConfig();
            if (serviceConfig == null) {
                return this.defaultThreadPool;
            } else if (serviceConfig.getCustomizeThreadPool() != null && serviceConfig.getCustomizeThreadPool()) {
                Integer corePoolSize = serviceConfig.getThreadPoolSize();
                Integer maxThreadPoolSize = serviceConfig.getMaxThreadPoolSize();
                Integer keepAliveTime = serviceConfig.getIdleThreadKeepAliveSecond();
                Integer maxQueueSize = serviceConfig.getMaxRunnableQueueSize();

                try {
                    ThreadPoolExecutor threadPool;
                    synchronized (this) {
                        if (this.threadPoolMap.get(rpcService) != null) {
                            return (ThreadPoolExecutor) this.threadPoolMap.get(rpcService);
                        }

                        threadPool = new ThreadPoolExecutor(corePoolSize, maxThreadPoolSize, (long) keepAliveTime,
                                TimeUnit.SECONDS, new LinkedBlockingQueue(maxQueueSize),
                                new NamedThreadFactory("service-biz-work"));
                        this.threadPoolMap.put(rpcService, threadPool);
                    }

                    return threadPool;
                } catch (Exception e) {
                    LOGGER.warn("Create service thread pool failed, will use default thread pool");
                    return this.defaultThreadPool;
                }
            } else {
                return this.defaultThreadPool;
            }
        }
    }

    @Override
    public ThreadPoolExecutor defaultThreadPool() {
        return this.defaultThreadPool;
    }

    @Override
    public void close() {
        Iterator var1 = this.threadPoolMap.values().iterator();

        while (var1.hasNext()) {
            ThreadPoolExecutor threadPool = (ThreadPoolExecutor) var1.next();
            if (!threadPool.isShutdown()) {
                threadPool.shutdown();
            }
        }

        this.threadPoolMap.clear();
        if (this.defaultThreadPool != null) {
            this.defaultThreadPool.shutdown();
        }

    }
}
