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

package org.apache.shenyu.integrated.test.http;

import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.apache.shenyu.integratedtest.common.dto.OAuth2DTO;
import org.apache.shenyu.integratedtest.common.dto.OrderDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

public final class OrderControllerTest extends AbstractTest {
    
    @Test
    public void testSave() throws IOException {
        OrderDTO orderDTO = new OrderDTO("123", "Tom");
        orderDTO = HttpHelper.INSTANCE.postGateway("/http/order/save", orderDTO, OrderDTO.class);
        assertEquals("hello world save order", orderDTO.getName());
    }
    
    @Test
    public void testFindById() throws IOException {
        OrderDTO orderDTO = HttpHelper.INSTANCE.getFromGateway("/http/order/findById?id=1", OrderDTO.class);
        assertEquals("1", orderDTO.getId());
        assertEquals("hello world findById", orderDTO.getName());
    }
    
    @Test
    public void testGetPathVariable() throws IOException {
        OrderDTO orderDTO = HttpHelper.INSTANCE.getFromGateway("/http/order/path/1/order-test", OrderDTO.class);
        assertEquals("1", orderDTO.getId());
        assertEquals("hello world restful: order-test", orderDTO.getName());
    }
    
    @Test
    public void testRestFul() throws IOException {
        OrderDTO orderDTO = HttpHelper.INSTANCE.getFromGateway("/http/order/path/1/name", OrderDTO.class);
        assertEquals("1", orderDTO.getId());
        assertEquals("hello world restful inline 1", orderDTO.getName());
    }
    
    @Test
    public void testRestFulOauth2NoAuthorization() throws IOException {
        OAuth2DTO oAuth2DTO = HttpHelper.INSTANCE.getFromGateway("/http/order/oauth2/test", OAuth2DTO.class);
        assertEquals("no authorization", oAuth2DTO.getToken());
    }
    
    @Test
    public void testRestFulOauth2WithToken() throws IOException {
        Map<String, Object> headers = new HashMap<>(2, 1);
        headers.put("Authorization", "order-test");
        OAuth2DTO oAuth2DTO = HttpHelper.INSTANCE.getFromGateway("/http/order/oauth2/test", headers, OAuth2DTO.class);
        assertEquals("order-test", oAuth2DTO.getToken());
    }
}
