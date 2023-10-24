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

package org.apache.shenyu.client.motan.common.annotation;

import com.weibo.api.motan.config.springsupport.annotation.MotanService;
import org.springframework.core.annotation.AliasFor;

import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The interface shenyu client.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Inherited
@MotanService
@ShenyuMotanClient
public @interface ShenyuMotanService {

    /**
     * interface class.
     *
     * @return class
     */
    @AliasFor(annotation = MotanService.class)
    Class<?> interfaceClass() default void.class;

    /**
     *  basic service string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String basicService() default "";

    /**
     * export string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String export() default "";

    /**
     * export string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String host() default "";

    /**
     * protocol string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String protocol() default "";

    /**
     * methods strings.
     *
     * @return strings
     */
    @AliasFor(annotation = MotanService.class)
    String[] methods() default {};

    /**
     * registry string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String registry() default "";

    /**
     * extConfig string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String extConfig() default "";

    /**
     * application string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String application() default "";

    /**
     * module string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String module() default "";

    /**
     * group string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String group() default "";

    /**
     * version string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String version() default "";

    /**
     * proxy string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String proxy() default "";

    /**
     * filter string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String filter() default "";

    /**
     * actives int.
     *
     * @return the int
     */
    @AliasFor(annotation = MotanService.class)
    int actives() default 0;

    /**
     * async boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = MotanService.class)
    boolean async() default false;

    /**
     * mock string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String mock() default "";

    /**
     * shareChannel boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = MotanService.class)
    boolean shareChannel() default false;

    /**
     * throwException boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = MotanService.class)
    boolean throwException() default false;

    /**
     * requestTimeout int.
     *
     * @return the int
     */
    @AliasFor(annotation = MotanService.class)
    int requestTimeout() default 0;

    /**
     * register boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = MotanService.class)
    boolean register() default false;

    /**
     * accessLog boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = MotanService.class)
    boolean accessLog() default false;

    /**
     * check boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = MotanService.class)
    boolean check() default false;

    /**
     * retries int.
     *
     * @return the int
     */
    @AliasFor(annotation = MotanService.class)
    int retries() default 0;

    /**
     * usegz boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = MotanService.class)
    boolean usegz() default false;

    /**
     * mingzSize int.
     *
     * @return the int
     */
    @AliasFor(annotation = MotanService.class)
    int mingzSize() default 0;

    /**
     * codec string.
     *
     * @return the string
     */
    @AliasFor(annotation = MotanService.class)
    String codec() default "";

    /**
     * Path string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuMotanClient.class)
    String value() default "";

    /**
     * Path string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuMotanClient.class)
    String path() default "";
    /**
     * Rule name string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuMotanClient.class)
    String ruleName() default "";

    /**
     * Desc string.
     *
     * @return String string
     */
    @AliasFor(annotation = ShenyuMotanClient.class)
    String desc() default "";

    /**
     * Enabled boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = ShenyuMotanClient.class)
    boolean enabled() default true;
}
