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

import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * ShenyuClientPathController.
 */
@RestController
@ApiModule(value = "shenyuClientPathController")
public class ShenyuClientPathController {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClientPathController.class);

    private static final String HELLO_SUFFIX = "I'm Shenyu-Gateway System. Welcome!";

    /**
     * hello.
     *
     * @return result
     */
    @RequestMapping("shenyu/client/hello")
    @ShenyuSpringMvcClient("shenyu/client/hello")
    @ApiDoc(desc = "shenyu/client/hello")
    public String hello() {
        return "hello! " + HELLO_SUFFIX;
    }

    /**
     * hello. br
     * no support gateway.
     *
     * @param name say hello user name
     * @return result
     */
    @RequestMapping("shenyu/client/hi")
    @ShenyuSpringMvcClient("shenyu/client/hi")
    @ApiDoc(desc = "shenyu/client/hi")
    public String hello(final String name) {
        return "hi! " + name + "! " + HELLO_SUFFIX;
    }

    /**
     * timeout.
     *
     * @return result
     */
    @RequestMapping("shenyu/client/timeout")
    @ShenyuSpringMvcClient("shenyu/client/timeout")
    @ApiDoc(desc = "shenyu/client/timeout")
    public String timeout() {
        LOG.info("timeout");
        try {
            Thread.sleep(10000);
        } catch (InterruptedException ignored) {
        }
        return "hello! " + HELLO_SUFFIX;
    }
    
    /**
     * hi.
     *
     * @param name name
     * @return result
     */
    @PostMapping("shenyu/client/post/hi")
    @ShenyuSpringMvcClient(desc = "shenyu client annotation register")
    @ApiDoc(desc = "shenyu/client/post/hi")
    public String post(final String name) {
        return "[post method result]:hi! " + name + "! " + HELLO_SUFFIX;
    }
}
