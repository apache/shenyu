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

package org.apache.shenyu.examples.springcloud.controller;

import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
import org.apache.shenyu.examples.springcloud.dto.EntityResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * AllController.
 */
@RestController
@ShenyuSpringCloudClient
public class AllController {
    
    /**
     * [class annotation] Do not use shenyu annotation path. used post mapping path.
     *
     * @return result
     */
    @PostMapping("/class/annotation/post")
    public EntityResult postMappingUrl() {
        return new EntityResult(200, "[class annotation] Do not use shenyu annotation path. used post mapping path");
    }
    
    /**
     * [class annotation] Do not use shenyu annotation path. used post mapping path.
     *
     * @return result
     */
    @GetMapping("/class/annotation/get")
    public EntityResult getMappingUrl() {
        return new EntityResult(200, "[class annotation] Do not use shenyu annotation path. used get mapping path");
    }
}
