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

import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.MediaType;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import java.io.File;
import java.io.IOException;

public class FileControllerTest {
    @Test
    public void testFileUploadWay1() throws Exception {
        File file1 = new File("src/main/resources/test.txt");
        File file2 = new File("src/main/resources/test");
        String filePath = "shenyu-examples/shenyu-examples-http/src/main/resources/static";
        assertEquals("Test successful", post(file1, filePath));
        assertEquals("Test failed", post(file2, filePath));
    }

    @Test
    public void testFileUploadWay2() throws Exception {
        String res1 = HttpHelper.INSTANCE.postGateway("/http/file/uploadWay2?file=测试成功&fileName=test.txt&filePath=shenyu-examples/shenyu-examples-http/src/main/resources/static/",
                java.lang.String.class);
        assertEquals("Upload succeeded", res1);
        String res2 = HttpHelper.INSTANCE.postGateway("/http/file/uploadWay2?file=测试成功&fileName=test.txt&filePath=shenyu/image/ss/", java.lang.String.class);
        assertEquals("Upload failed", res2);
    }

    @Test
    public void testFileDownload() throws Exception {
        String res = HttpHelper.INSTANCE.postGateway("/http/file/download", java.lang.String.class);
        assertEquals("Test successful", res);
    }

    /**
     * Post file.
     *
     * @param file the file
     * @param filePath the filePath
     * @return the String
     * @throws IOException the IOException
     */
    public String post(final File file, final String filePath) throws IOException {
        OkHttpClient client = new OkHttpClient();
        RequestBody fileBody = RequestBody.create(MediaType.parse("multipart/form-data"), file);
        RequestBody requestBody = new MultipartBody.Builder()
                .setType(MultipartBody.FORM)
                .addFormDataPart("file", file.getName(), fileBody)
                .addFormDataPart("filePath", filePath)
                .build();
        Request request = new Request.Builder()
                .url("http://localhost:9195/http/file/uploadWay1")
                .post(requestBody)
                .build();
        try {
            Response response = client.newCall(request).execute();
        } catch (IOException e) {
            return "Test failed";
        }
        return "Test successful";
    }
}
