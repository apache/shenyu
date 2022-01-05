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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.BufferedInputStream;
import java.io.OutputStream;
import java.net.URLEncoder;

/**
 * FileController
 */
@RestController
@RequestMapping("/file")
@ShenyuSpringMvcClient(path="/file/**")
public class FileController {

    Logger logger= LoggerFactory.getLogger(FileController.class);

    /**
     * Post fileUploadWay1.
     *
     * @param file the file
     * @param filePath the filePath
     * @return the String
     * @throws Exception the Exception
     */
    @RequestMapping("/uploadWay1")
    public String fileUploadWay1(@RequestParam("file") MultipartFile file, @RequestParam("filePath") String filePath) throws IOException {
        String fileName=file.getOriginalFilename();
        FileOutputStream out;
        try {
            out = new FileOutputStream(filePath + fileName);
            out.write(file.getBytes());
            out.flush();
        } catch (IOException e) {
            return "UploadFailed";
        }
        return "UploadSucceeded";
    }

    /**
     * Post fileUploadWay2.
     *
     * @param file the file
     * @param filePath the filePath
     * @param fileName the fileName
     * @return the String
     * @throws Exception the Exception
     */
    @RequestMapping("/uploadWay2")
    public String fileUploadWay2(@RequestParam("file") byte[] file, @RequestParam("filePath") String filePath,@RequestParam("fileName") String fileName) throws IOException {
        FileOutputStream out ;
        try {
            out = new FileOutputStream(filePath + fileName);
            out.write(file);
            out.flush();
        } catch (IOException e) {
            return "UploadFailed";
        }
        return "UploadSucceeded";
    }

    /**
     * Get fileDownload.
     *
     * @return the String
     * @throws Exception the Exception
     */
    @RequestMapping("/download")
    public String fileDownload(HttpServletResponse response) throws IOException {
        File file=new File("shenyu-integrated-test/shenyu-integrated-test-http/src/main/resources/test.txt");
        String  fileName= file.getName();
        logger.info(file.getAbsolutePath());
        response.setContentType("application/octet-stream");
        response.setHeader("content-type", "application/octet-stream");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Content-Disposition", "attachment;fileName=" + URLEncoder.encode(fileName,"UTF-8"));
        FileInputStream fis;
        BufferedInputStream bis;
        OutputStream os;
        byte[] buf=new byte[1024];
        fis=new FileInputStream(file);
        bis=new BufferedInputStream(fis);
        os=response.getOutputStream();
        int len=bis.read(buf);
        while(len!= -1) {
            os.write(buf, 0, len);
            len = bis.read(buf);
        }
        return null;
    }

}
