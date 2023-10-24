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

package org.apache.shenyu.examples.http.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * SpringMvcMappingPathController.
 */
@RestController
@ShenyuSpringMvcClient(desc = "spring annotation register")
@Api(value = "springMvcMappingPathController")
public class SpringMvcMappingPathController {

    private static final String HELLO_SUFFIX = "I'm Shenyu-Gateway System. Welcome!";

    /**
     * hello.
     *
     * @return result
     */
    @RequestMapping("hello")
    @ApiOperation(value = "hello", notes = "say hello.")
    public String hello() {
        return "hello! " + HELLO_SUFFIX;
    }

    /**
     * hi.
     *
     * @param name name
     * @return result
     */
    @RequestMapping("hi")
    @ApiOperation(value = "hi", notes = "say hello to name.")
    public String hello(final String name) {
        return "hi! " + name + "! " + HELLO_SUFFIX;
    }

    /**
     * hi.
     *
     * @param name name
     * @return result
     */
    @PostMapping("post/hi")
    @ApiOperation(value = "postHi", notes = "post hi.")
    public String post(final String name) {
        return "[post method result]:hi! " + name + "! " + HELLO_SUFFIX;
    }
}
