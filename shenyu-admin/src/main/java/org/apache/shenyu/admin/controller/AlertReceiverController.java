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

import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.AlertReceiverService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * Alert Receiver Controller.
 */
@Validated
@RestController
@RequestMapping("/alert/receiver")
public class AlertReceiverController {

    @Autowired
    private AlertReceiverService alertReceiverService;

    /**
     * add new alert receiver.
     * @param alertReceiverDTO alertReceiverDTO
     * @return row int
     */
    @PostMapping
    public ShenyuAdminResult addReceiver(@Valid @RequestBody final AlertReceiverDTO alertReceiverDTO) {
        alertReceiverService.addReceiver(alertReceiverDTO);
        return ShenyuAdminResult.success(ShenyuResultMessage.CREATE_SUCCESS);
    }
	
	@GetMapping("/getAll")
	public ShenyuAdminResult getAllReceiver() {
		List<AlertReceiverDTO> receiverDTOS = alertReceiverService.getAll();
		return ShenyuAdminResult.success(receiverDTOS);
	}
	
}
