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

package org.apache.shenyu.e2e.engine.annotation;

import org.apache.shenyu.e2e.engine.ShenYuExtension;
import org.apache.shenyu.e2e.engine.ShenYuLogExtension;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.Mode;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.ServiceType;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.extension.ExtendWith;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@ExtendWith({ShenYuExtension.class, ShenYuLogExtension.class})
@TestInstance(Lifecycle.PER_CLASS)
public @interface ShenYuTest {
    
    /**
     * Indices whether the depended on services run under ShenYu e2e engine managed.
     * </p>
     * Mode.DOCKER: under ShenYu e2e engine managed.
     * </p>
     * Mode.HOST: ShenYu e2e engine unmanaged the services.
     * </p>
     *
     * {@see Mode}
     */
    @ShenYuValue("{shenyu.e2e.mode}")
    Mode mode() default Mode.DOCKER;
    
    ServiceConfigure[] services() default {};
    
    String sharedKey() default "global";
    
    @ShenYuValue("{shenyu.e2e.docker-compose}")
    String dockerComposeFile() default "docker-compose.yml";
    
    @Target(ElementType.TYPE)
    @Retention(RetentionPolicy.RUNTIME)
    @interface ServiceConfigure {
        
        /**
         * Indices the service's name.
         */
        @ShenYuValue("{shenyu.e2e.services[].serviceName}")
        String serviceName();
        
        /**
         * Indices the HTTP schema to access to service.
         * </p>
         * That is available for {@link Mode#DOCKER}
         */
        @ShenYuValue("{shenyu.e2e.services[].schema}")
        String schema() default "http";
    
        /**
         * Indices the port of service.
         * </p>
         * That is available for {@link Mode#DOCKER}
         */
        @ShenYuValue("{shenyu.e2e.services[].port}")
        int port() default -1; // TODO to support multi ports for service
    
        /**
         * Indices the baseUrl of service.
         * </p>
         * That is available for {@link Mode#HOST}
         */
        @ShenYuValue("{shenyu.e2e.services[].baseUrl}")
        String baseUrl() default "";
    
        @ShenYuValue("{shenyu.e2e.services[].type}")
        ServiceType type() default ServiceType.SHENYU_ADMIN;
    
        /**
         * Indices more configures about connection.
         */
        Parameter[] parameters() default {};
    }
    
    @Target(ElementType.TYPE)
    @Retention(RetentionPolicy.RUNTIME)
    @interface Parameter {
        
        String key();
        
        String value();
    }
    
}
