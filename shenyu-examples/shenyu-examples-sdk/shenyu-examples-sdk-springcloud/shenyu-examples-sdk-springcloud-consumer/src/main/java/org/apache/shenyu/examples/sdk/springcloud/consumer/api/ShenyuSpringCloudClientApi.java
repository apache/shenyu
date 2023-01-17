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

package org.apache.shenyu.examples.sdk.springcloud.consumer.api;

import org.apache.shenyu.examples.sdk.springcloud.consumer.dto.OrderDTO;
import org.apache.shenyu.examples.sdk.springcloud.consumer.impl.ShenyuSpringCloudClientApiFallbackFactory;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@ShenyuClient(value = "shenyu-gateway", fallbackFactory = ShenyuSpringCloudClientApiFallbackFactory.class)
public interface ShenyuSpringCloudClientApi {

    /**
     * save.
     *
     * @param orderDTO OrderDTO
     * @return OrderDTO
     */
    @PostMapping("/springcloud/order/save")
    OrderDTO save(@RequestBody OrderDTO orderDTO);

    /**
     * Find by id order dto.
     *
     * @param id the id
     * @return the order dto
     */
    @GetMapping("/springcloud/order/findById")
    OrderDTO findById(@RequestParam("id") String id);

    /**
     * Gets path variable.
     *
     * @param id   the id
     * @param name the name
     * @return the path variable
     */
    @GetMapping("/springcloud/order/path/{id}/{name}")
    OrderDTO getPathVariable(@PathVariable("id") String id, @PathVariable("name") String name);

    /**
     * Test rest ful order dto.
     *
     * @param id the id
     * @return the order dto
     */
    @GetMapping("/springcloud/order/path/{id}/name")
    OrderDTO testRestFul(@PathVariable("id") String id);

}
