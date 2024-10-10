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

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.model.dto.BatchNamespaceCommonDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.AlertReceiverQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.AlertReceiverService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.PathVariable;

/**
 * Alert Receiver Controller.
 */
@RestApi("/alert/receiver")
public class AlertReceiverController {

    @Autowired
    private AlertReceiverService alertReceiverService;
    
    /**
     * add new alert receiver.
     *
     * @param alertReceiverDTO alertReceiverDTO
     * @return result
     */
    @PostMapping
    public ShenyuAdminResult addReceiver(@Valid @RequestBody final AlertReceiverDTO alertReceiverDTO) {
        alertReceiverService.addReceiver(alertReceiverDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS);
    }
    
    /**
     * update alert receiver.
     *
     * @param alertReceiverDTO alertReceiverDTO
     * @return result
     */
    @PutMapping
    public ShenyuAdminResult editReceiver(@Valid @RequestBody final AlertReceiverDTO alertReceiverDTO) {
        alertReceiverService.updateReceiver(alertReceiverDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS);
    }

    /**
     * delete alert receiver.
     *
     * @param batchNamespaceCommonDTO batchNamespaceCommonDTO.
     * @return result
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteReceiver(@Valid @RequestBody final BatchNamespaceCommonDTO batchNamespaceCommonDTO) {
        alertReceiverService.deleteByNamespaceId(batchNamespaceCommonDTO.getIds(), batchNamespaceCommonDTO.getNamespaceId());
        return ShenyuAdminResult.success(ShenyuResultMessage.DELETE_SUCCESS);
    }
    
    /**
     * delete alert receiver.
     *
     * @param id alertReceiver ID
     * @param namespaceId namespaceId.
     * @return result
     */
    @GetMapping("/{id}/{namespaceId}")
    public ShenyuAdminResult getReceiverDetail(@PathVariable("id") final String id,
                                               @PathVariable("namespaceId") @Valid
                                               @Existed(provider = NamespaceMapper.class,
                                               message = "namespaceId is not existed")
        final String namespaceId) {
        AlertReceiverDTO receiverDTO = alertReceiverService.detail(id, namespaceId);
        return ShenyuAdminResult.success(receiverDTO);
    }
    
    /**
     * query receiver.
     * @param currentPage current page
     * @param pageSize page size
     * @param namespaceId namespaceId.
     * @return receiver
     */
    @GetMapping
    public ShenyuAdminResult getReceivers(@RequestParam @NotNull final Integer currentPage,
                                          @RequestParam @NotNull final Integer pageSize,
                                          @Valid @Existed(message = "namespaceId is not existed",
                                                  provider = NamespaceMapper.class) final String namespaceId
    ) {
        CommonPager<AlertReceiverDTO> commonPager = alertReceiverService.listByPage(new AlertReceiverQuery(new PageParameter(currentPage, pageSize), namespaceId));
        return ShenyuAdminResult.success(commonPager);
    }
    
    /**
     * send test message to receiver.
     * @param alertReceiverDTO receiver
     * @return send result
     */
    @PostMapping(path = "/test")
    public ShenyuAdminResult sendTestMsg(@Valid @RequestBody final AlertReceiverDTO alertReceiverDTO) {
        try {
            boolean sendFlag = alertReceiverService.sendTestMsg(alertReceiverDTO);
            if (sendFlag) {
                return ShenyuAdminResult.success();
            } else {
                return ShenyuAdminResult.error("Notify service not available, please check config!");
            }
        } catch (Exception e) {
            return ShenyuAdminResult.error("Notify service error: " + e.getMessage());
        }
    }
    
}
