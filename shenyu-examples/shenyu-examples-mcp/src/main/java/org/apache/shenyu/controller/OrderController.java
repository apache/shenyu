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

import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.mcp.common.annotation.Header;
import org.apache.shenyu.client.mcp.common.annotation.OpenApiConfig;
import org.apache.shenyu.client.mcp.common.annotation.OpenApiInfo;
import org.apache.shenyu.client.mcp.common.annotation.OpenApiParameter;
import org.apache.shenyu.client.mcp.common.annotation.OpenApiPath;
import org.apache.shenyu.client.mcp.common.annotation.OpenApiServer;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpClient;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpRequestConfig;
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
@ShenyuMcpClient()
@ApiModule(value = "order")
public class OrderController {

    /**
     * Find by id order dto.
     *
     * @param id the id
     * @return the order dto
     */
    @GetMapping("/findById")
    @ShenyuMcpClient(
            openApi = @OpenApiConfig(
                    info = @OpenApiInfo(title = "findOrder", description = "find order by id"),
                    server = @OpenApiServer(url = "http://localhost:8150"),
                    path = @OpenApiPath(
                            path = "/order/findById",
                            parameter = @OpenApiParameter(
                                    name = "id"
                            )
                    )
            ),
            requestConfig = @ShenyuMcpRequestConfig(
                    headers = {
                            @Header(key = "aaa", value = "bbb")
                    }
            )
    )
    @ApiDoc(desc = "findById")
    public OrderDTO findById(@RequestParam("id") final String id) {
        OrderDTO dto = new OrderDTO();
        dto.setId(id);
        return dto;
    }
}
