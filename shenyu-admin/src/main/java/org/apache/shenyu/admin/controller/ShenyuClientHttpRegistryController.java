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
import jakarta.validation.Valid;

import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.register.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.admin.service.NamespaceService;
import org.apache.shenyu.admin.validation.validator.ShenyuClientRegistryValidator;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The type shenyu client controller.
 */
@RestController
@RequestMapping
public class ShenyuClientHttpRegistryController {

    @Resource
    private ShenyuClientServerRegisterPublisher publisher;

    @Resource
    private NamespaceService namespaceService;

    /**
     * Registers HTTP request validation without adding framework dependencies to protocol DTOs.
     *
     * @param binder the request binder
     */
    @InitBinder
    public void initBinder(final WebDataBinder binder) {
        binder.addValidators(new ShenyuClientRegistryValidator());
    }

    /**
     * Returns HTTP 400 for invalid registrations before any event is published.
     *
     * @param exception the validation failure
     * @return the registration error
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public ShenyuAdminResult handleInvalidRegistration(final MethodArgumentNotValidException exception) {
        String message = exception.getBindingResult().getFieldErrors().stream()
                .map(error -> error.getField() + ": " + error.getDefaultMessage())
                .collect(Collectors.joining("| "));
        return ShenyuAdminResult.error(String.format("Request error! invalid argument [%s]", message));
    }

    /**
     * Register metadata string.
     *
     * @param metaDataRegisterDTO the meta data register dto
     * @return the string
     */
    @PostMapping(RegistrationProtocol.META_PATH)
    @ResponseBody
    public String registerMetadata(@RequestBody @Valid final MetaDataRegisterDTO metaDataRegisterDTO) {
        if (Objects.isNull(metaDataRegisterDTO.getNamespaceId())) {
            metaDataRegisterDTO.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        }
        publisher.publish(metaDataRegisterDTO);
        return RegistrationProtocol.SUCCESS;
    }

    /**
     * Register uri string.
     *
     * @param uriRegisterDTO the uri register dto
     * @return the string
     */
    @PostMapping(RegistrationProtocol.URI_PATH)
    @ResponseBody
    public String registerURI(@RequestBody @Valid final URIRegisterDTO uriRegisterDTO) {
        if (Objects.isNull(uriRegisterDTO.getNamespaceId())) {
            uriRegisterDTO.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        }
        publisher.publish(uriRegisterDTO);
        return RegistrationProtocol.SUCCESS;
    }

    /**
     * registerApiDoc.
     *
     * @param apiDocRegisterDTO apiDocRegisterDTO
     * @return String
     */
    @PostMapping(RegistrationProtocol.API_DOC_PATH)
    @ResponseBody
    public String registerApiDoc(@RequestBody @Valid final ApiDocRegisterDTO apiDocRegisterDTO) {
        publisher.publish(apiDocRegisterDTO);
        return RegistrationProtocol.SUCCESS;
    }

    /**
     * registerDiscoveryConfig.
     *
     * @param discoveryConfigRegisterDTO discoveryConfigRegisterDTO
     * @return String
     */
    @PostMapping(RegistrationProtocol.DISCOVERY_CONFIG_PATH)
    @ResponseBody
    public String registerDiscoveryConfig(@RequestBody @Valid final DiscoveryConfigRegisterDTO discoveryConfigRegisterDTO) {
        if (Objects.isNull(discoveryConfigRegisterDTO.getNamespaceId())) {
            discoveryConfigRegisterDTO.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        }
        publisher.publish(discoveryConfigRegisterDTO);
        return RegistrationProtocol.SUCCESS;
    }

    /**
     * registerMcpTools.
     *
     * @param mcpToolsRegisterDTO mcpToolsRegisterDTO
     * @return String
     */
    @PostMapping(RegistrationProtocol.MCP_TOOLS_PATH)
    @ResponseBody
    public String registerMcpTools(@RequestBody @Valid final McpToolsRegisterDTO mcpToolsRegisterDTO) {
        if (Objects.isNull(mcpToolsRegisterDTO.getNamespaceId())) {
            mcpToolsRegisterDTO.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        }
        publisher.publish(mcpToolsRegisterDTO);
        return RegistrationProtocol.SUCCESS;
    }

    /**
     * Offline result string.
     *
     * @param offlineDTO the offline dto
     * @return the string
     */
    @PostMapping(RegistrationProtocol.OFFLINE_PATH)
    @ResponseBody
    public String offline(@RequestBody @Valid final URIRegisterDTO offlineDTO) {
        if (Objects.isNull(offlineDTO.getNamespaceId())) {
            offlineDTO.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        }
        publisher.publish(offlineDTO);
        return RegistrationProtocol.SUCCESS;
    }

    public void checkClientNamespaceExist(final String namespaceId) {
        NamespaceVO namespaceVO = namespaceService.findByNamespaceId(namespaceId);
        if (Objects.isNull(namespaceVO)) {
            throw new IllegalArgumentException("namespaceId is not exist");
        }
    }
}
