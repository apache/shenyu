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

import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.RequestBody;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class UploadControllerTest {

    private static final String FILE_PATH_NOE = "1.txt";

    private static final String FILE_PATH_TWO = "2.txt";

    private static final Logger LOG = LoggerFactory.getLogger(UploadControllerTest.class);

    @BeforeAll
    public static void setup() throws IOException {
        Path pathOne = Paths.get(FILE_PATH_NOE);
        Path pathTwo = Paths.get(FILE_PATH_TWO);
        if (!Files.exists(pathOne)) {
            Files.createFile(pathOne);
        }
        if (!Files.exists(pathTwo)) {
            Files.createFile(pathTwo);
        }
        try {
            BufferedWriter bufferedWriterOne = Files.newBufferedWriter(pathOne);
            bufferedWriterOne.write("111");
            bufferedWriterOne.flush();
            bufferedWriterOne.close();
            BufferedWriter bufferedWriterTwo = Files.newBufferedWriter(pathTwo);
            bufferedWriterTwo.write("222");
            bufferedWriterTwo.flush();
            bufferedWriterTwo.close();
        } catch (IOException e) {
            LOG.error("write file fail", e);
        }
    }

    @Test
    public void testWebFluxSingle() throws IOException {
        File fileOne = new File(FILE_PATH_NOE);
        RequestBody fileBodyOne = RequestBody.create(MediaType.parse("multipart/form-data"), fileOne);
        MultipartBody requestBody = new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart("file", FILE_PATH_NOE, fileBodyOne)
                .build();
        final String response = HttpHelper.INSTANCE.postGateway("/http/upload/webFluxSingle", requestBody, String.class);
        assertEquals(response, FILE_PATH_NOE);
    }

    @Test
    public void testWebFluxFiles() throws IOException {
        File fileOne = new File(FILE_PATH_NOE);
        File fileTwo = new File(FILE_PATH_TWO);
        RequestBody fileBodyOne = RequestBody.create(MediaType.parse("multipart/form-data"), fileOne);
        RequestBody fileBodyTwo = RequestBody.create(MediaType.parse("multipart/form-data"), fileTwo);
        MultipartBody requestBody = new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart("files", FILE_PATH_NOE, fileBodyOne)
                .addFormDataPart("files", FILE_PATH_TWO, fileBodyTwo)
                .build();
        final String response = HttpHelper.INSTANCE.postGateway("/http/upload/webFluxFiles", requestBody, String.class);
        assertEquals(response, FILE_PATH_NOE + "," + FILE_PATH_TWO);
    }

    @AfterAll
    public static void clean() throws IOException {
        Files.delete(Paths.get(FILE_PATH_NOE));
        Files.delete(Paths.get(FILE_PATH_TWO));
    }

}
