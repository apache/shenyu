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

package org.apache.shenyu.integratedtest.springcloud;

import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.RequestBody;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class SpringCloudPluginUploadTest extends AbstractPluginDataInit {

    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudPluginUploadTest.class);

    private static final String FILE_PATH = "1.txt";

    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.SPRING_CLOUD.getName(), "");
        assertThat(pluginResult, is("success"));

        Path filePath = Paths.get(FILE_PATH);
        if (!Files.exists(filePath)) {
            Files.createFile(filePath);
        }
        try {
            BufferedWriter bufferedWriterOne = Files.newBufferedWriter(filePath);
            bufferedWriterOne.write("111");
            bufferedWriterOne.flush();
            bufferedWriterOne.close();
        } catch (IOException e) {
            LOG.error("write file fail", e);
        }
    }

    @Test
    public void testUploadFile() throws IOException {
        File file = new File(FILE_PATH);
        RequestBody fileBody = RequestBody.create(MediaType.parse("multipart/form-data"), file);
        MultipartBody requestBody = new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart("file", FILE_PATH, fileBody)
                .build();
        final String response = HttpHelper.INSTANCE.postGateway("/springcloud/upload/file", requestBody, String.class);
        assertEquals(FILE_PATH, response);
    }

    @AfterAll
    public static void clean() throws IOException {
        Files.delete(Paths.get(FILE_PATH));
    }
}
