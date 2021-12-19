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

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * ShenyuClientPathController
 */
@RestController
public class ShenyuClientPathController { // 客户端支持mvc注解中的path，并向后兼容
    
    private static final String HELLO_SUFFIX = "I'm Shenyu-Gateway System. Welcome!";
    
    /**
     * hello
     *
     * @return result
     */
    @RequestMapping("shenyu/client/hello")
    @ShenyuSpringMvcClient(path = "shenyu/client/hello", desc = "shenyu client annotation register")
    public ResponseEntity<String> hello() {
        return ResponseEntity.ok("hello! " + HELLO_SUFFIX);
    }
    
    /**
     * hello. <br>
     * no support gateway.
     *
     * @param name say hello user name
     * @return result
     */
    @RequestMapping("shenyu/client/hi")
    public ResponseEntity<String> hello(final String name) {
        return ResponseEntity.ok("hi! " + name + "! " + HELLO_SUFFIX);
    }
    
    /**
     * hi.
     *
     * @param name name
     * @return result
     */
    @PostMapping("shenyu/client/post/hi")
    @ShenyuSpringMvcClient(path = "shenyu/client/post/hi", desc = "shenyu client annotation register")
    public ResponseEntity<String> post(final String name) {
        return ResponseEntity.ok("[post method result]:hi! " + name + "! " + HELLO_SUFFIX);
    }
}
