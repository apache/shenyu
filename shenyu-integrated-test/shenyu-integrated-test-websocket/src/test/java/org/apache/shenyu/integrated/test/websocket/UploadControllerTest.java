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

package org.apache.shenyu.integrated.test.websocket;

import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.RequestBody;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

public class UploadControllerTest extends AbstractPluginDataInit {

    private static final String FILE_PATH = "1.bin";

    @BeforeAll
    public static void setup() throws IOException {
        Path pathOne = Paths.get(FILE_PATH);
        if (!Files.exists(pathOne)) {
            Files.createFile(pathOne);
        }
        BufferedWriter bufferedWriterOne = Files.newBufferedWriter(pathOne);
        bufferedWriterOne.write("111");
        bufferedWriterOne.flush();
        bufferedWriterOne.close();
    }

    @Test
    public void testWebsocketUpLoad() throws IOException {
        File fileOne = new File(FILE_PATH);
        RequestBody fileBodyOne = RequestBody.create(MediaType.parse("multipart/form-data"), fileOne);
        MultipartBody requestBody = new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart("file", FILE_PATH, fileBodyOne)
                .build();
        final String response = HttpHelper.INSTANCE.postGateway("/ws-native/ws/upload", requestBody, String.class);
        assertEquals(response, "ok");
    }

}
