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

package org.apache.shenyu.client.apache.dubbo.annotation;

import org.apache.dubbo.common.constants.ClusterRules;
import org.apache.dubbo.common.constants.LoadbalanceRules;
import org.apache.dubbo.config.annotation.Method;
import org.apache.dubbo.config.annotation.DubboService;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.springframework.core.annotation.AliasFor;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import static org.apache.dubbo.common.constants.CommonConstants.DEFAULT_CALLBACK_INSTANCES;
import static org.apache.dubbo.common.constants.CommonConstants.DEFAULT_RETRIES;


/**
 * The interface shenyu service.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@DubboService
@Inherited
@ShenyuDubboClient
public @interface ShenyuDubboService {

    /**
     * Alias for {@link ShenyuDubboClient#value()} .
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuDubboClient.class)
    String value() default "";

    /**
     * Alias for {@link ShenyuDubboClient#path()}.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuDubboClient.class)
    String path() default "";

    /**
     * Alias for {@link ShenyuDubboClient#ruleName()} .
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuDubboClient.class)
    String ruleName() default "";

    /**
     * Alias for {@link ShenyuDubboClient#desc()} .
     *
     * @return String string
     */
    @AliasFor(annotation = ShenyuDubboClient.class)
    String desc() default "";

    /**
     * Alias for {@link ShenyuDubboClient#enabled()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = ShenyuDubboClient.class)
    boolean enabled() default true;

    /**
     * Alias for {@link DubboService#interfaceClass()}.
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    Class<?> interfaceClass() default void.class;

    /**
     * Alias for {@link DubboService#interfaceName()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String interfaceName() default "";

    /**
     * Alias for {@link DubboService#version()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String version() default "";

    /**
     * Alias for {@link DubboService#group()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String group() default "";

    /**
     * Alias for {@link DubboService#path()}.
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class, attribute = "path")
    String dubboPath() default "";

    /**
     * Alias for {@link DubboService#export()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = DubboService.class)
    boolean export() default true;

    /**
     * Alias for {@link DubboService#token()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String token() default "";

    /**
     * Alias for {@link DubboService#deprecated()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = DubboService.class)
    boolean deprecated() default false;

    /**
     * Alias for {@link DubboService#dynamic()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = DubboService.class)
    boolean dynamic() default true;

    /**
     * Alias for {@link DubboService#accesslog()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String accesslog() default "";

    /**
     * Alias for {@link DubboService#executes()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int executes() default 0;

    /**
     * Alias for {@link DubboService#register()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = DubboService.class)
    boolean register() default true;

    /**
     * Alias for {@link DubboService#weight()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int weight() default 0;

    /**
     * Alias for {@link DubboService#document()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String document() default "";

    /**
     * Alias for {@link DubboService#delay()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int delay() default 0;

    /**
     * Alias for {@link DubboService#local()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String local() default "";

    /**
     * Alias for {@link DubboService#stub()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String stub() default "";

    /**
     * Alias for {@link DubboService#cluster()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String cluster() default ClusterRules.EMPTY;

    /**
     * Alias for {@link DubboService#proxy()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String proxy() default "";

    /**
     * Alias for {@link DubboService#connections()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int connections() default 0;

    /**
     * Alias for {@link DubboService#callbacks()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int callbacks() default DEFAULT_CALLBACK_INSTANCES;

    /**
     * Alias for {@link DubboService#onconnect()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String onconnect() default "";

    /**
     * Alias for {@link DubboService#ondisconnect()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String ondisconnect() default "";

    /**
     * Alias for {@link DubboService#owner()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String owner() default "";

    /**
     * Alias for {@link DubboService#layer()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String layer() default "";

    /**
     *Alias for {@link DubboService#retries()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int retries() default DEFAULT_RETRIES;

    /**
     * Alias for {@link DubboService#loadbalance()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String loadbalance() default LoadbalanceRules.RANDOM;

    /**
     * Alias for {@link DubboService#async()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = DubboService.class)
    boolean async() default false;

    /**
     * Alias for {@link DubboService#actives()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int actives() default 0;

    /**
     * Alias for {@link DubboService#sent()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = DubboService.class)
    boolean sent() default false;

    /**
     * Alias for {@link DubboService#mock()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String mock() default "";

    /**
     * Alias for {@link DubboService#validation()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String validation() default "";

    /**
     * Alias for {@link DubboService#timeout()} .
     *
     * @return the int
     */
    @AliasFor(annotation = DubboService.class)
    int timeout() default 0;

    /**
     * Alias for {@link DubboService#cache()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String cache() default "";

    /**
     * Alias for {@link DubboService#filter()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = DubboService.class)
    String[] filter() default {};

    /**
     * Alias for {@link DubboService#listener()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = DubboService.class)
    String[] listener() default {};

    /**
     * Alias for {@link DubboService#parameters()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = DubboService.class)
    String[] parameters() default {};

    /**
     * Alias for {@link DubboService#application()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String application() default "";

    /**
     * Alias for {@link DubboService#module()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String module() default "";

    /**
     * Alias for {@link DubboService#provider()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String provider() default "";

    /**
     * Alias for {@link DubboService#protocol()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = DubboService.class)
    String[] protocol() default {};

    /**
     * Alias for {@link DubboService#monitor()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String monitor() default "";

    /**
     * Alias for {@link DubboService#registry()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = DubboService.class)
    String[] registry() default {};

    /**
     * Alias for {@link DubboService#tag()} .
     *
     * @return the string
     */
    @AliasFor(annotation = DubboService.class)
    String tag() default "";

    /**
     * Alias for {@link DubboService#methods()} .
     *
     * @return the Method[]
     */
    @AliasFor(annotation = DubboService.class)
    Method[] methods() default {};
}
