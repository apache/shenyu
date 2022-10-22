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

package org.apache.shenyu.client.spring.websocket.annotation;

import org.springframework.core.annotation.AliasFor;

import javax.websocket.Decoder;
import javax.websocket.Encoder;
import javax.websocket.server.ServerEndpointConfig;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The interface shenyu client.
 *
 * <p>Combined annotation of @ShenyuSpringWebSocketClient and @ServerEndpoint. This annotation can
 * only be used in projects using @ServerEndpoint, if you are using native websocket or reactor websocket,
 * please use @ShenyuSpringWebSocketClient instead of this.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@ShenyuSpringWebSocketClient
public @interface ShenyuServerEndpoint {

    /**
     * Path string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuSpringWebSocketClient.class)
    String path() default "";

    /**
     * Rule name string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuSpringWebSocketClient.class)
    String ruleName() default "";

    /**
     * Desc string.
     *
     * @return String string
     */
    @AliasFor(annotation = ShenyuSpringWebSocketClient.class)
    String desc() default "";

    /**
     * Enabled boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = ShenyuSpringWebSocketClient.class)
    boolean enabled() default true;

    /**
     * Register meta data boolean.
     *
     * @return the boolean
     */
    @AliasFor(annotation = ShenyuSpringWebSocketClient.class)
    boolean registerMetaData() default false;

    /**
     * ServerEndpoint value string.
     *
     * @return the string
     */
    @AliasFor(annotation = ShenyuSpringWebSocketClient.class)
    String value() default "";

    /**
     * ServerEndpoint subprotocols array.
     *
     * @return the array
     */
    String[] subprotocols() default {};

    /**
     * ServerEndpoint decoders class array.
     *
     * @return the array
     */
    Class<? extends Decoder>[] decoders() default {};

    /**
     * ServerEndpoint encoders class array.
     *
     * @return the array
     */
    Class<? extends Encoder>[] encoders() default {};

    /**
     * ServerEndpoint configurator class.
     *
     * @return the class
     */
    Class<? extends ServerEndpointConfig.Configurator> configurator()
            default ServerEndpointConfig.Configurator.class;

}
