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

import org.apache.shenyu.admin.model.dto.ProxyGatewayDTO;
import org.apache.shenyu.admin.service.SandboxService;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.IOException;

/**
 * Sandbox environment.
 */
@RestController
@RequestMapping("/sandbox")
public class SandboxController {

    private final SandboxService sandboxService;

    public SandboxController(final SandboxService sandboxService) {
        this.sandboxService = sandboxService;
    }

    /**
     * proxy Gateway.
     *
     * @param proxyGatewayDTO proxyGatewayDTO
     * @param request         request
     * @param response        response
     * @throws IOException throw io exception
     */
    @PostMapping(path = "/proxyGateway")
    public void proxyGateway(@RequestBody @Valid final ProxyGatewayDTO proxyGatewayDTO,
                            final HttpServletRequest request,
                            final HttpServletResponse response) throws IOException {
        sandboxService.requestProxyGateway(proxyGatewayDTO, request, response);
    }
}
