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

package org.apache.shenyu.examples.sdk.springcloud.consumer.controller;

import org.apache.shenyu.examples.sdk.springcloud.consumer.api.ShenyuSpringCloudClientApi;
import org.apache.shenyu.examples.sdk.springcloud.consumer.dto.OrderDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class ShenyuSdkSpringCloudConsumerController {

    @Autowired
    private ShenyuSpringCloudClientApi clientApi;

    /**
     * save.
     *
     * @param orderDTO orderDto
     * @return OrderDTO
     */
    @PostMapping("/save")
    public OrderDTO save(@RequestBody final OrderDTO orderDTO) {
        return clientApi.save(orderDTO);
    }

    /**
     * findById.
     *
     * @param id id
     * @return OrderDTO
     */
    @GetMapping("/findById")
    OrderDTO findById(@RequestParam("id") final String id) {
        return clientApi.findById(id);
    }

    /**
     * getPathVariable.
     *
     * @param id   id
     * @param name name
     * @return OrderDTO
     */
    @GetMapping("/path/{id}/{name}")
    OrderDTO getPathVariable(@PathVariable("id") final String id, @PathVariable("name") final String name) {
        return clientApi.getPathVariable(id, name);
    }

    /**
     * testRestFul.
     *
     * @param id id
     * @return OrderDTO
     */
    @GetMapping("/path/{id}/name")
    OrderDTO testRestFul(@PathVariable("id") final String id) {
        return clientApi.testRestFul(id);
    }

}
