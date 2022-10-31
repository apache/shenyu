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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * The type shenyu client controller.
 */
@RequestMapping("/shenyu-client")
@Join
public class ShenyuClientHttpRegistryController implements ShenyuClientServerRegisterRepository {

    private ShenyuClientServerRegisterPublisher publisher;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
        this.publisher = publisher;
    }

    @Override
    public void close() {
        publisher.close();
    }

    /**
     * Register metadata string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/register-metadata")
    @ResponseBody
    public String registerMetadata(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        publisher.publish(metaDataRegisterDTO);
        return ShenyuResultMessage.SUCCESS;
    }
    
    
    /**
     * Register uri string.
     *
     * @param uriRegisterDTO the uri register dto
     * @return the string
     */
    @PostMapping("/register-uri")
    @ResponseBody
    public String registerURI(@RequestBody final URIRegisterDTO uriRegisterDTO) {
        publisher.publish(uriRegisterDTO);
        return ShenyuResultMessage.SUCCESS;
    }
    
}
