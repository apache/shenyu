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

package org.apache.shenyu.client.springcloud.annotation;

import org.springframework.core.annotation.AliasFor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This is a convenience annotation that is equivalent to
 * declaring {@code @RequestMapping} and {@code @ShenyuSpringCloudClient}.
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@RequestMapping
@ShenyuSpringCloudClient
public @interface ShenyuRequestMapping {

    /**
     * Alias for {@link RequestMapping#value}, {@link ShenyuSpringCloudClient#value}.
     *
     * @return the string
     */
    @AliasFor(attribute = "path")
    String value() default "";

    /**
     * Alias for {@link RequestMapping#path}, {@link ShenyuSpringCloudClient#path}.
     *
     * @return the string
     */
    @AliasFor(attribute = "value")
    String path() default "";

    /**
     * Alias for {@link ShenyuSpringCloudClient#ruleName}.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuSpringCloudClient.class)
    String ruleName() default "";

    /**
     * Alias for {@link ShenyuSpringCloudClient#desc}.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuSpringCloudClient.class)
    String desc() default "";

    /**
     * Alias for {@link ShenyuSpringCloudClient#enabled}.
     *
     * @return the boolean
     */
    @AliasFor(annotation = ShenyuSpringCloudClient.class)
    boolean enabled() default true;

    /**
     * Alias for {@link RequestMapping#params}.
     *
     * @return the string[]
     */
    @AliasFor(annotation = RequestMapping.class)
    String[] params() default {};

    /**
     * Alias for {@link RequestMapping#headers}.
     *
     * @return the string[]
     */
    @AliasFor(annotation = RequestMapping.class)
    String[] headers() default {};

    /**
     * Alias for {@link RequestMapping#consumes}.
     *
     * @return the string[]
     */
    @AliasFor(annotation = RequestMapping.class)
    String[] consumes() default {};

    /**
     * Alias for {@link RequestMapping#produces}.
     *
     * @return the string[]
     */
    @AliasFor(annotation = RequestMapping.class)
    String[] produces() default {};

    /**
     * Alias for {@link RequestMapping#method}.
     *
     * @return the RequestMethod[]
     */
    @AliasFor(annotation = RequestMapping.class)
    RequestMethod[] method() default {};
}


