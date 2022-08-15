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

package org.apache.shenyu.examples.motan.service.config;

import com.weibo.api.motan.config.springsupport.ProtocolConfigBean;
import org.apache.shenyu.springboot.starter.client.motan.property.ShenyuMotanProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ShenyuMotanConfig {

    /**
     * define a bean with name demoMotan of ProtocolConfigBean.
     *
     * @param properties shenyu motan properties
     * @return demoMotan
     */
    @Bean("demoMotan")
    public ProtocolConfigBean protocolConfig(final ShenyuMotanProperties properties) {
        ProtocolConfigBean config = new ProtocolConfigBean();
        config.setDefault(true);
        config.setName(properties.getProtocolConfig().getName());
        config.setMaxContentLength(properties.getProtocolConfig().getMaxContentLength());
        return config;
    }
}
