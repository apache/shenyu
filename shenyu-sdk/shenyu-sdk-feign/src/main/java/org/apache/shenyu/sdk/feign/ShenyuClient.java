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

package org.apache.shenyu.sdk.feign;

import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.cloud.openfeign.FeignClientsConfiguration;
import org.springframework.core.annotation.AliasFor;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation for interfaces declaring that a REST client with that interface should be
 * created (e.g. for autowiring into another component). If SC LoadBalancer is available
 * it will be used to load balance the backend requests, and the load balancer can be
 * configured using the same name (i.e. value) as the Shenyu client.
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@FeignClient
public @interface ShenyuClient {

    /**
     * The name of the service with optional protocol prefix. Synonym for {@link #name()
     * name}. A name must be specified for all clients, whether or not a url is provided.
     * Can be specified as property key, eg: ${propertyKey}.
     * @return the name of the service with optional protocol prefix
     */
    @AliasFor(value = "value", annotation = FeignClient.class)
    String value() default "";

    /**
     * This will be used as the bean name instead of name if present, but will not be used
     * as a service id.
     * @return bean name instead of name if present
     */
    @AliasFor(value = "contextId", annotation = FeignClient.class)
    String contextId() default "";

    /**
     * an absolute URL or resolvable hostname.
     * @return an absolute URL or resolvable hostname (the protocol is optional).
     */
    @AliasFor(value = "url", annotation = FeignClient.class)
    String url() default "";

    /**
     * name.
     * @return The service id with optional protocol prefix. Synonym for {@link #value()
     * value}.
     */
    @AliasFor(value = "name", annotation = FeignClient.class)
    String name() default "";

    /**
     * qualifiers.
     * @return the @Qualifiers value for the Shenyu client to the default = contextId + "ShenyuClient"
     */
    @AliasFor(value = "qualifiers", annotation = FeignClient.class)
    String[] qualifiers() default {};

    /**
     * dismiss 404 or not.
     * @return whether 404s should be dismissed instead of throwing FeignExceptions
     */
    @AliasFor(value = "dismiss404", annotation = FeignClient.class)
    boolean dismiss404() default false;

    /**
     * A custom configuration class for the feign client. Can contain override
     * <code>@Bean</code> definition for the pieces that make up the client, for instance
     * {@link feign.codec.Decoder}, {@link feign.codec.Encoder}, {@link feign.Contract}.
     *
     * @see FeignClientsConfiguration for the defaults
     * @return list of configurations for feign client
     */
    @AliasFor(value = "configuration", annotation = FeignClient.class)
    Class<?>[] configuration() default {};

    /**
     * Fallback class for the specified Shenyu client interface. The fallback class must
     * implement the interface annotated by this annotation and be a valid spring bean.
     * @return fallback class for the specified Shenyu client interface
     */
    @AliasFor(value = "fallback", annotation = FeignClient.class)
    Class<?> fallback() default void.class;

    /**
     * Define a fallback factory for the specified Shenyu client interface. The fallback
     * factory must produce instances of fallback classes that implement the interface
     * annotated by {@link ShenyuClient}. The fallback factory must be a valid spring bean.
     *
     * @see FallbackFactory for details.
     * @return fallback factory for the specified Shenyu client interface
     */
    @AliasFor(value = "fallbackFactory", annotation = FeignClient.class)
    Class<?> fallbackFactory() default void.class;

    /**
     * path.
     * @return path prefix to be used by all method-level mappings.
     */
    @AliasFor(value = "path", annotation = FeignClient.class)
    String path() default "";

    /**
     * primary.
     *
     * @return {@link boolean}
     */
    @AliasFor(value = "primary", annotation = FeignClient.class)
    boolean primary() default true;

}
