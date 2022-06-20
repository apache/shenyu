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

package org.apache.shenyu.integrated.test.combination.controller;

import org.apache.dubbo.common.extension.ExtensionLoader;
import org.apache.dubbo.common.threadpool.ThreadPool;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.grpc.client.GrpcClientBuilder;
import org.apache.shenyu.plugin.motan.proxy.MotanProxyService;
import org.apache.shenyu.plugin.sofa.cache.ApplicationConfigCache;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;
import java.util.Optional;

@RestController
@RequestMapping(value = "/shenyu", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
public class SharedThreadPoolController {
    
    /**
     * get the shared thread pool from spring container.
     *
     * @return the shared thread pool
     */
    @GetMapping("/getFromSpring")
    public String getFromSpring() {
        return SpringBeanUtils.getInstance().getBean(ShenyuThreadPoolExecutor.class).toString();
    }
    
    /**
     * get the shared thread pool from apache dubbo.
     *
     * @return the shared thread pool
     */
    @GetMapping("/getFromDubbo")
    public String getFromDubbo() {
        return Optional.ofNullable(ExtensionLoader.getExtensionLoader(ThreadPool.class))
                .map(loader -> loader.getExtension("shared"))
                .map(shared -> shared.getExecutor(null))
                .map(Object::toString)
                .orElse("");
    }
    
    /**
     * get the shared thread pool from grpc.
     *
     * @return the shared thread pool
     */
    @GetMapping("/getFromGrpc")
    public String getFromGrpc() {
        return Optional.ofNullable(GrpcClientBuilder.buildExecutor())
                .map(Object::toString)
                .orElse("");
    }
    
    /**
     * get the shared thread pool from motan.
     *
     * @return the shared thread pool
     */
    @GetMapping("/getFromMotan")
    public String getFromMotan() {
        return Optional.ofNullable(SpringBeanUtils.getInstance().getBean(MotanProxyService.class).getThreadPool())
                .map(Objects::toString)
                .orElse("");
    }
    
    /**
     * get the shared thread pool from sofa.
     *
     * @return the shared thread pool
     */
    @GetMapping("/getFromSofa")
    public String getFromSofa() {
        return Optional.ofNullable(ApplicationConfigCache.getInstance().getThreadPool())
                .map(Objects::toString)
                .orElse("");
    }
}
