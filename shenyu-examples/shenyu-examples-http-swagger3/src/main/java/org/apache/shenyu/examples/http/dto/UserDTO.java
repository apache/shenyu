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

package org.apache.shenyu.examples.http.dto;

import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

import java.util.List;
import java.util.Map;

/**
 * The type User dto.
 */
public class UserDTO {

    @Schema(name = "user id", requiredMode = Schema.RequiredMode.REQUIRED, example = "100000")
    private String userId;

    @Schema(name = "user name", example = "shenyu")
    private String userName;

    @Schema(name = "main order", example = "")
    private OrderDTO mainOrder;

    @ArraySchema(schema = @Schema(description = "order list", requiredMode = Schema.RequiredMode.REQUIRED, example = ""))
    private List<OrderDTO> orderDetailList;

    @Schema(name = "order map", example = "")
    private Map<String, OrderDTO> orderDetailMap;

    /**
     * Get userId.
     *
     * @return userId
     */
    public String getUserId() {
        return userId;
    }

    /**
     * Set userId.
     *
     * @param userId userId
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }

    /**
     * Get userName.
     *
     * @return userName
     */
    public String getUserName() {
        return userName;
    }

    /**
     * Set userName.
     *
     * @param userName userName
     */
    public void setUserName(final String userName) {
        this.userName = userName;
    }

    /**
     * getMainOrder.
     *
     * @return OrderDTO
     */
    public OrderDTO getMainOrder() {
        return mainOrder;
    }

    /**
     * setMainOrder.
     *
     * @param mainOrder mainOrder
     */
    public void setMainOrder(final OrderDTO mainOrder) {
        this.mainOrder = mainOrder;
    }

    /**
     * getOrderDetailList.
     *
     * @return List
     */
    public List<OrderDTO> getOrderDetailList() {
        return orderDetailList;
    }

    /**
     * setOrderDetailList.
     *
     * @param orderDetailList orderDetailList
     */
    public void setOrderDetailList(final List<OrderDTO> orderDetailList) {
        this.orderDetailList = orderDetailList;
    }

    /**
     * getOrderDetailMap.
     *
     * @return Map
     */
    public Map<String, OrderDTO> getOrderDetailMap() {
        return orderDetailMap;
    }

    /**
     * setOrderDetailMap.
     *
     * @param orderDetailMap orderDetailMap
     */
    public void setOrderDetailMap(final Map<String, OrderDTO> orderDetailMap) {
        this.orderDetailMap = orderDetailMap;
    }

    @Override
    public String toString() {
        return "UserDTO{"
            + "userId='"
            + userId
            + '\''
            + ", userName='"
            + userName
            + '\''
            + ", mainOrder="
            + mainOrder
            + ", orderDetailList="
            + orderDetailList
            + ", orderDetailMap="
            + orderDetailMap
            + '}';
    }
}
