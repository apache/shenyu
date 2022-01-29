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
import org.apache.shenyu.examples.http.dto.FileUploadResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import javax.servlet.http.HttpServletResponse;
import java.io.BufferedOutputStream;
import java.io.OutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

@RestController
@RequestMapping("/file")
@ShenyuSpringMvcClient(path = "/file/**")
public class FileController {

    private static final Logger LOG = LoggerFactory.getLogger(FileController.class);

    private static final String TEST_FILE_PATH = "shenyu-examples/shenyu-examples-http/src/main/resources/test_file";

    @PostMapping("/uploadFile")
    public FileUploadResponse uploadFile(@RequestParam("file") MultipartFile file) throws IOException {
        LOG.info("get upload file: {}", new String(file.getBytes(), StandardCharsets.UTF_8));
        return new FileUploadResponse(file.getName(), file.getContentType(), file.getSize());
    }

    @GetMapping("/downloadFile")
    @SuppressWarnings("ResultOfMethodCallIgnored")
    public void downloadFile(HttpServletResponse response) throws IOException {
        FileInputStream inputStream = new FileInputStream(TEST_FILE_PATH);
        byte[] buffer = new byte[inputStream.available()];
        inputStream.read(buffer);
        inputStream.close();

        response.reset();
        response.addHeader("Content-Disposition", "attachment;filename=test_file");

        OutputStream outputStream = new BufferedOutputStream(response.getOutputStream());
        outputStream.write(buffer);
        outputStream.flush();
        outputStream.close();
    }
}
