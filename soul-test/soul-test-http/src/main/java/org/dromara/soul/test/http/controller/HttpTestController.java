/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.test.http.controller;

import org.dromara.soul.client.common.annotation.SoulClient;
import org.dromara.soul.test.http.dto.UserDTO;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * TestController.
 *
 * @author xiaoyu
 */
@RestController
@RequestMapping("/test")
public class HttpTestController {

    /**
     * Post user dto.
     *
     * @param userDTO the user dto
     * @return the user dto
     */
    @PostMapping("/payment")
    @SoulClient(path = "/test/payment", desc = "支付接口")
    public UserDTO post(@RequestBody final UserDTO userDTO) {
        return userDTO;
    }

    /**
     * Find by user id string.
     *
     * @param userId the user id
     * @return the string
     */
    @GetMapping("/findByUserId")
    @SoulClient(path = "/test/findByUserId", desc = "获取用户id")
    public String findByUserId(@RequestParam("userId") final String userId) {
        return "hello :" + userId;
    }

    /**
     * Gets path variable.
     *
     * @param id   the id
     * @param name the name
     * @return the path variable
     */
    @GetMapping("/path/{id}")
    public String getPathVariable(@PathVariable("id") final String id, @RequestParam("name") final String name) {
        return id + "_" + name;
    }

    /**
     * Put path variable and body string.
     *
     * @param id      the id
     * @param userDTO the user dto
     * @return the string
     */
    @PutMapping("/putPathBody/{id}")
    public String putPathVariableAndBody(@PathVariable("id") final String id, @RequestBody final UserDTO userDTO) {
        return id + "_" + userDTO.getUserName();
    }

}
