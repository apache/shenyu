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

import jakarta.annotation.Resource;

import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.register.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * The type shenyu client controller.
 */
@RestController
@RequestMapping("/shenyu-client")
public class ShenyuClientHttpRegistryController {

    @Resource
    private ShenyuClientServerRegisterPublisher publisher;

    @Resource
    private NamespaceService namespaceService;

    /**
     * Register metadata string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping("/register-metadata")
    @ResponseBody
    public String registerMetadata(@RequestBody final MetaDataRegisterDTO metaDataRegisterDTO) {
        checkClientNamespaceExist(metaDataRegisterDTO.getNamespaceId());
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
        checkClientNamespaceExist(uriRegisterDTO.getNamespaceId());
        publisher.publish(uriRegisterDTO);
        return ShenyuResultMessage.SUCCESS;
    }

    /**
     * registerApiDoc.
     *
     * @param apiDocRegisterDTO apiDocRegisterDTO
     * @return String
     */
    @PostMapping("/register-apiDoc")
    @ResponseBody
    public String registerApiDoc(@RequestBody final ApiDocRegisterDTO apiDocRegisterDTO) {
        publisher.publish(apiDocRegisterDTO);
        return ShenyuResultMessage.SUCCESS;
    }

    /**
     * registerDiscoveryConfig.
     *
     * @param discoveryConfigRegisterDTO discoveryConfigRegisterDTO
     * @return String
     */
    @PostMapping("/register-discoveryConfig")
    @ResponseBody
    public String registerDiscoveryConfig(@RequestBody final DiscoveryConfigRegisterDTO discoveryConfigRegisterDTO) {
        checkClientNamespaceExist(discoveryConfigRegisterDTO.getNamespaceId());
        publisher.publish(discoveryConfigRegisterDTO);
        return ShenyuResultMessage.SUCCESS;
    }

    /**
     * Offline result string.
     *
     * @param offlineDTO the offline dto
     * @return the string
     */
    @PostMapping("/offline")
    @ResponseBody
    public String offline(@RequestBody final URIRegisterDTO offlineDTO) {
        checkClientNamespaceExist(offlineDTO.getNamespaceId());
        publisher.publish(offlineDTO);
        return ShenyuResultMessage.SUCCESS;
    }

    public void checkClientNamespaceExist(final String namespaceId) {
        NamespaceVO namespaceVO = namespaceService.findByNamespaceId(namespaceId);
        if (Objects.isNull(namespaceVO)) {
            throw new IllegalArgumentException("namespaceId is not exist");
        }
    }
}
