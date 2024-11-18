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

package org.apache.shenyu.admin;

import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.ldap.LdapAutoConfiguration;
import org.springframework.core.env.Environment;

/**
 * shenyu admin start.
 */
@SpringBootApplication(exclude = {LdapAutoConfiguration.class})
public class ShenyuAdminBootstrap {

    private static final Logger log = LoggerFactory.getLogger(ShenyuAdminBootstrap.class);

    /**
     * Main entrance.
     *
     * @param args startup arguments
     */
    public static void main(final String[] args) {
        SpringApplication.run(ShenyuAdminBootstrap.class, args);
        final Environment environment = SpringBeanUtils.getInstance().getBean(Environment.class);
        log.info("ShenyuAdminBootstrap started shenyu.sync.http.enabled {}", environment.getProperty("shenyu.sync.http.enabled"));
    }

}
