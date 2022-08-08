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

package org.apache.shenyu.client.springmvc.annotation;

import org.springframework.core.annotation.AliasFor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This is a convenience annotation that is equivalent to
 * declaring {@code @PostMapping} and {@code @ShenyuSpringMvcClient}.
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@RequestMapping(
        method = {RequestMethod.POST}
)
@ShenyuSpringMvcClient
public @interface ShenyuPostMapping {

    /**
     * Path string.
     *
     * @return the string
     */
    @AliasFor(
            attribute = "path"
    )
    String value() default "";

    /**
     * Path string.
     *
     * @return the string
     */
    @AliasFor(
            attribute = "value"
    )
    String path() default "";

    /**
     * Rule name string.
     *
     * @return the string
     */
    @AliasFor(
            annotation = ShenyuSpringMvcClient.class
    )
    String ruleName() default "";

    /**
     * Desc string.
     *
     * @return String string
     */
    @AliasFor(
            annotation = ShenyuSpringMvcClient.class
    )
    String desc() default "";

    /**
     * Enabled boolean.
     *
     * @return the boolean
     */
    @AliasFor(
            annotation = ShenyuSpringMvcClient.class
    )
    boolean enabled() default true;

    /**
     * Register meta data boolean.
     *
     * @return the boolean
     */
    @AliasFor(
            annotation = ShenyuSpringMvcClient.class
    )
    boolean registerMetaData() default true;
}


