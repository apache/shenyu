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

package org.apache.shenyu.client.alibaba.dubbo.annotation;

import com.alibaba.dubbo.config.annotation.Method;
import com.alibaba.dubbo.config.annotation.Service;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.springframework.core.annotation.AliasFor;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * The interface shenyu service.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Service
@Inherited
@ShenyuDubboClient
public @interface ShenyuService {

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
     * Alias for {@link Service#interfaceClass()}.
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    Class<?> interfaceClass() default void.class;

    /**
     * Alias for {@link Service#interfaceName()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String interfaceName() default "";

    /**
     * Alias for {@link Service#version()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String version() default "";

    /**
     * Alias for {@link Service#group()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String group() default "";

    /**
     * Alias for {@link Service#path()}.
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class, attribute = "path")
    String dubboPath() default "";

    /**
     * Alias for {@link Service#export()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = Service.class)
    boolean export() default false;

    /**
     * Alias for {@link Service#token()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String token() default "";

    /**
     * Alias for {@link Service#deprecated()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = Service.class)
    boolean deprecated() default false;

    /**
     * Alias for {@link Service#dynamic()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = Service.class)
    boolean dynamic() default false;

    /**
     * Alias for {@link Service#accesslog()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String accesslog() default "";

    /**
     * Alias for {@link Service#executes()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int executes() default 0;

    /**
     * Alias for {@link Service#register()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = Service.class)
    boolean register() default false;

    /**
     * Alias for {@link Service#weight()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int weight() default 0;

    /**
     * Alias for {@link Service#document()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String document() default "";

    /**
     * Alias for {@link Service#delay()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int delay() default 0;

    /**
     * Alias for {@link Service#local()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String local() default "";

    /**
     * Alias for {@link Service#stub()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String stub() default "";

    /**
     * Alias for {@link Service#cluster()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String cluster() default "";

    /**
     * Alias for {@link Service#proxy()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String proxy() default "";

    /**
     * Alias for {@link Service#connections()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int connections() default 0;

    /**
     * Alias for {@link Service#callbacks()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int callbacks() default 0;

    /**
     * Alias for {@link Service#onconnect()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String onconnect() default "";

    /**
     * Alias for {@link Service#ondisconnect()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String ondisconnect() default "";

    /**
     * Alias for {@link Service#owner()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String owner() default "";

    /**
     * Alias for {@link Service#layer()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String layer() default "";

    /**
     *Alias for {@link Service#retries()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int retries() default 0;

    /**
     * Alias for {@link Service#loadbalance()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String loadbalance() default "";

    /**
     * Alias for {@link Service#async()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = Service.class)
    boolean async() default false;

    /**
     * Alias for {@link Service#actives()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int actives() default 0;

    /**
     * Alias for {@link Service#sent()} .
     *
     * @return the boolean
     */
    @AliasFor(annotation = Service.class)
    boolean sent() default false;

    /**
     * Alias for {@link Service#mock()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String mock() default "";

    /**
     * Alias for {@link Service#validation()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String validation() default "";

    /**
     * Alias for {@link Service#timeout()} .
     *
     * @return the int
     */
    @AliasFor(annotation = Service.class)
    int timeout() default 0;

    /**
     * Alias for {@link Service#cache()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String cache() default "";

    /**
     * Alias for {@link Service#filter()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = Service.class)
    String[] filter() default {};

    /**
     * Alias for {@link Service#listener()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = Service.class)
    String[] listener() default {};

    /**
     * Alias for {@link Service#parameters()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = Service.class)
    String[] parameters() default {};

    /**
     * Alias for {@link Service#application()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String application() default "";

    /**
     * Alias for {@link Service#module()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String module() default "";

    /**
     * Alias for {@link Service#provider()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String provider() default "";

    /**
     * Alias for {@link Service#protocol()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = Service.class)
    String[] protocol() default {};

    /**
     * Alias for {@link Service#monitor()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String monitor() default "";

    /**
     * Alias for {@link Service#registry()} .
     *
     * @return the string[]
     */
    @AliasFor(annotation = Service.class)
    String[] registry() default {};

    /**
     * Alias for {@link Service#tag()} .
     *
     * @return the string
     */
    @AliasFor(annotation = Service.class)
    String tag() default "";

    /**
     * Alias for {@link Service#methods()} .
     *
     * @return the Method[]
     */
    @AliasFor(annotation = Service.class)
    Method[] methods() default {};
}
