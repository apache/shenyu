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

package org.apache.shenyu.examples.sdk.springcloud.consumer.impl;

import org.apache.shenyu.examples.sdk.springcloud.consumer.api.ShenyuSpringCloudClientApi;
import org.apache.shenyu.examples.sdk.springcloud.consumer.dto.OrderDTO;
import org.apache.shenyu.sdk.spring.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class ShenyuSpringCloudClientApiFallbackFactory implements FallbackFactory<ShenyuSpringCloudClientApi> {

    @Override
    public ShenyuSpringCloudClientApi create(final Throwable cause) {

        return new ShenyuSpringCloudClientApi() {
            @Override
            public OrderDTO save(final OrderDTO orderDTO) {
                return null;
            }

            @Override
            public OrderDTO findById(final String id) {

                OrderDTO orderDTO = new OrderDTO();
                orderDTO.setId("1");
                orderDTO.setName("fallback factory");
                return orderDTO;
            }

            @Override
            public OrderDTO getPathVariable(final String id, final String name) {
                return null;
            }

            @Override
            public OrderDTO testRestFul(final String id) {
                return null;
            }
        };
    }
}
