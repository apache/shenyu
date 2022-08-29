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

package org.apache.shenyu.examples.springmvc.controller;

import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * UploadController.
 */
@RestController
@RequestMapping("/upload")
@ShenyuSpringMvcClient("/upload/**")
public class UploadController {

    /**
     * mvc upload single file.
     * @param file file.
     * @return  response
     */
    @PostMapping(value = "/singleFile")
    public String singleFile(@RequestParam("file") final MultipartFile file) {
        return file.getOriginalFilename();
    }

    /**
     * mvc upload multiple files.
     * @param files files.
     * @return  response
     */
    @PostMapping(value = "/files")
    public String files(@RequestParam("files") final MultipartFile[] files) {
        return Arrays.stream(files).map(MultipartFile::getOriginalFilename).collect(Collectors.joining(","));
    }
}
