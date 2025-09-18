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

package org.apache.shenyu.controller;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.servers.Server;
import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpHeader;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpRequestConfig;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpTool;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpToolParam;
import org.apache.shenyu.dto.OrderDTO;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * test controller.
 */
@RestController
@RequestMapping("/order")
@ShenyuMcpTool(
        definition = @OpenAPIDefinition(
                servers = @Server(url = "http://localhost:8150")
        ), toolName = "order"

)
@ApiModule(value = "order")
public class OrderController {

    /**
     * Find by id order dto.
     *
     * @param id the id
     * @return the order dto
     */
    @GetMapping("/findById")
    @ShenyuMcpTool(
            operation = @Operation(
                    method = "Get", description = "find order by id"
            ),
            requestConfig = @ShenyuMcpRequestConfig(
                    bodyJson = "shenyu",
                    headers = {
                            @ShenyuMcpHeader(key = "aaa", value = "bbb")
                    }
            ),
            enabled = true
    )
    @ApiDoc(desc = "findById")
    public OrderDTO findById(@ShenyuMcpToolParam(
            parameter = @Parameter(
                    name = "id",
                    in = ParameterIn.PATH,
                    description = "the id of order",
                    required = true,
                    schema = @Schema(
                            type = "string",
                            defaultValue = "1"
                    )
            )
    ) @RequestParam("id") final String id) {
        OrderDTO dto = new OrderDTO();
        dto.setId(id);
        return dto;
    }
}
