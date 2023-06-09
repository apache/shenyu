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

import com.baidu.cloud.starlight.springcloud.server.annotation.RpcService;
import org.springframework.core.annotation.AliasFor;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Shenyu brpc service annotation.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Inherited
@RpcService
@ShenyuBrpcClient
public @interface ShenyuBrpcService {

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
    String path() default "";

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
     * service id.
     *
     * @return the string
     */
    @AliasFor(annotation = RpcService.class)
    String serviceId() default "";

    /**
     * protocol.
     *
     * @return the string
     */
    @AliasFor(annotation = RpcService.class)
    String protocol() default "";

    /**
     * filters.
     *
     * @return the string
     */
    @AliasFor(annotation = RpcService.class)
    String filters() default "";
}
