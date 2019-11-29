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

package org.dromara.soul.test.springcloud.controller;

import org.dromara.soul.client.common.annotation.SoulClient;
import org.dromara.soul.test.springcloud.dto.UserDTO;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
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
public class TestController {


    /**
     * Post string.
     *
     * @param userDTO the user dto
     * @return the string
     */
    @PostMapping("/post")
    @SoulClient(path = "/test/post", desc = "测试post")
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
    @SoulClient(path = "/test/findByUserId", desc = "测试findByUserId")
    public String findByUserId(@RequestParam("userId") final String userId) {
        return "helloWorld:" + userId;
    }


}
