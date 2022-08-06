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

package org.apache.shenyu.client.brpc.common.annotation;

import com.baidu.brpc.spring.annotation.NamingOption;
import com.baidu.brpc.spring.annotation.RpcExporter;
import org.springframework.core.annotation.AliasFor;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The merged @interface of {@link ShenyuBrpcClient} and {@link RpcExporter}.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@ShenyuBrpcClient
@RpcExporter
public @interface ShenyuRpcExporter {
    
    /**
     * Value string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuBrpcClient.class)
    String value() default "";
    
    /**
     * Path string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuBrpcClient.class)
    String path();
    
    /**
     * Rule name string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuBrpcClient.class)
    String ruleName() default "";
    
    /**
     * Desc string.
     *
     * @return String string
     */
    @AliasFor(annotation = ShenyuBrpcClient.class)
    String desc() default "";
    
    /**
     * Enabled boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = ShenyuBrpcClient.class)
    boolean enabled() default true;
    
    /**
     * RPC server port to publish.
     *
     * @return the string
     */
    @AliasFor(annotation = RpcExporter.class)
    String port() default "8080";
    
    /**
     * bean name of RPC server options bean type must be {@link com.baidu.brpc.server.RpcServerOptions}.
     *
     * @return the string
     */
    @AliasFor(annotation = RpcExporter.class)
    String rpcServerOptionsBeanName() default "";
    
    /**
     * bean name of RPC interceptor bean type must be {@link com.baidu.brpc.interceptor.Interceptor}.
     *
     * @return the string
     */
    @AliasFor(annotation = RpcExporter.class)
    String interceptorBeanNames() default "";
    
    /**
     * Group for naming service.
     *
     * @return group
     */
    @AliasFor(annotation = RpcExporter.class)
    String group() default "normal";
    
    /**
     * Version for naming service.
     *
     * @return version
     */
    @AliasFor(annotation = RpcExporter.class)
    String version() default "1.0.0";
    
    /**
     * ignore it when failed to register naming service.
     *
     * @return true, ignore
     */
    @AliasFor(annotation = RpcExporter.class)
    boolean ignoreFailOfNamingService() default false;
    
    /**
     * attention here - it is not global share thread pool between multi RpcClient/RpcServer ,
     * if you want to use global thread pool , see rpc options.
     *
     * @return true: use the shared thread pool; false: create individual thread pool for register service
     */
    @AliasFor(annotation = RpcExporter.class)
    boolean useServiceSharedThreadPool() default true;
    
    /**
     * Extra naming options. This option is effective on service-scope.
     *
     * <p> This config may have different behavior depending on which NamingService is used,
     * consult documentation of the specific {@link com.baidu.brpc.naming.NamingService} for detailed usage.
     *
     * @return Extra naming options
     */
    @AliasFor(annotation = RpcExporter.class)
    NamingOption[] extraOptions() default {};
}
