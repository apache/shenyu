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

import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
import org.apache.shenyu.examples.springcloud.dto.EntityResult;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * NewFeatureController.
 */
@RestController
@RequestMapping("new/feature")
@ApiModule(value = "newFeatureController")
public class NewFeatureController {
    
    /**
     * no support gateway access api.
     *
     * @return result
     */
    @RequestMapping("/gateway/not")
    @ApiDoc(desc = "gateway/not")
    public EntityResult noSupportGateway() {
        return new EntityResult(200, "no support gateway access");
    }
    
    /**
     * Do not use shenyu annotation path. used request mapping path.
     *
     * @return result
     */
    @RequestMapping("/requst/mapping/path")
    @ShenyuSpringCloudClient
    @ApiDoc(desc = "requst/mapping/path")
    public EntityResult requestMappingUrl() {
        return new EntityResult(200, "Do not use shenyu annotation path. used request mapping path");
    }
    
    /**
     * Do not use shenyu annotation path. used post mapping path.
     *
     * @return result
     */
    @PostMapping("/post/mapping/path")
    @ShenyuSpringCloudClient
    @ApiDoc(desc = "post/mapping/path")
    public EntityResult postMappingUrl() {
        return new EntityResult(200, "Do not use shenyu annotation path. used post mapping path");
    }
    
    /**
     * Do not use shenyu annotation path. used post mapping path.
     *
     * @return result
     */
    @GetMapping("/get/mapping/path")
    @ShenyuSpringCloudClient
    @ApiDoc(desc = "get/mapping/path")
    public EntityResult getMappingUrl() {
        return new EntityResult(200, "Do not use shenyu annotation path. used get mapping path");
    }
    
    /**
     * Do not use shenyu annotation path. used put mapping path.
     *
     * @return result
     */
    @PutMapping("/put/mapping/path")
    @ShenyuSpringCloudClient
    @ApiDoc(desc = "put/mapping/path")
    public EntityResult putMappingUrl() {
        return new EntityResult(200, "Do not use shenyu annotation path. used put mapping path");
    }
    
    /**
     * Do not use shenyu annotation path. used put mapping path.
     *
     * @return result
     */
    @DeleteMapping("/delete/mapping/path")
    @ShenyuSpringCloudClient
    @ApiDoc(desc = "delete/mapping/path")
    public EntityResult deleteMappingUrl() {
        return new EntityResult(200, "Do not use shenyu annotation path. used delete mapping path");
    }
}
