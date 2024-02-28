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

import org.apache.shenyu.e2e.engine.ShenYuLogExtension;
import org.apache.shenyu.e2e.enums.ServiceTypeEnum;
import org.apache.shenyu.e2e.engine.ShenYuExtension;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.extension.ExtendWith;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@ExtendWith({ShenYuExtension.class, ShenYuLogExtension.class})
@TestInstance(Lifecycle.PER_CLASS)
public @interface ShenYuTest {
    
    /**
     * services configs.
     *
     * @return ServiceConfigure[]
     *
     */
    Environment[] environments() default {};
    
    @Target(ElementType.TYPE)
    @Retention(RetentionPolicy.RUNTIME)
    @interface Environment {
        
        String serviceName();
        
        ShenYuTest.ServiceConfigure service();
    }
    
    @Target(ElementType.TYPE)
    @Retention(RetentionPolicy.RUNTIME)
    @interface ServiceConfigure {
        
        /**
         * the service module name.
         *
         * @return module name
         */
        String moduleName() default "";
        
        String schema() default "http";
        
        int port() default -1;
        
        /**
         * the service base url, example: <a href="http://localhost:9095">http://localhost:9095</a>.
         *
         * @return the base url
         */
        String baseUrl() default "";
        
        /**
         * the service type.
         *
         * @return the service type.
         */
        ServiceTypeEnum type() default ServiceTypeEnum.SHENYU_ADMIN;
        
        /**
         * Indices more configures about connection.
         * @return Parameter[]
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
