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

package org.apache.shenyu.integrated.test.http;


import io.grpc.LoadBalancer;
import org.apache.shenyu.examples.http.controller.FileController;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.Test;
import org.springframework.web.multipart.MultipartFile;
import static org.junit.Assert.assertEquals;
import java.io.File;
import java.io.FileInputStream;

public class FileControllerTest {
    @Test
    public void TestFileUploadWay1() throws Exception {
         File file=new File("D:/文件/测试文件.txt");
         FileInputStream input = new FileInputStream(file);
         MultipartFile multipartFile = new org.springframework.mock.web.MockMultipartFile("file", file.getName(), "text/plain", org.apache.commons.io.IOUtils.toByteArray(input));
         FileController fileController1=new FileController();
         FileController fileController2= HttpHelper.INSTANCE.postGateway("/http/file/uploadWay1",fileController1,FileController.class);
         assertEquals("上传成功",fileController2.fileUploadWay1(multipartFile,"D:/image"));
    }

}
