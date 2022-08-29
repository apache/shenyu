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
import okhttp3.Response;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.apache.shenyu.integratedtest.common.dto.UserDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.integratedtest.common.result.ResultBean;
import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.springframework.http.MediaType.APPLICATION_OCTET_STREAM_VALUE;
import static org.springframework.http.MediaType.MULTIPART_FORM_DATA_VALUE;

public final class HttpTestControllerTest extends AbstractTest {

    @Test
    public void testPayment() throws IOException {
        UserDTO user = new UserDTO("1", "http-test");
        user = HttpHelper.INSTANCE.postGateway("/http/test/payment", user, UserDTO.class);
        assertEquals("1", user.getUserId());
        assertEquals("http-test", user.getUserName());
    }

    @Test
    public void testFindByUserId() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/findByUserId?userId=1", UserDTO.class);
        assertEquals("1", user.getUserId());
        assertEquals("hello world", user.getUserName());
    }

    @Test
    public void testFindByUserIdSepcChar0() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/findByUserId?userId=1%25", UserDTO.class);
        assertEquals("1%", user.getUserId());
        assertEquals("hello world", user.getUserName());
    }

    @Test
    public void testFindByUserIdSepcChar1() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/findByUserId?userId=1%201", UserDTO.class);
        assertEquals("1 1", user.getUserId());
        assertEquals("hello world", user.getUserName());
    }

    @Test
    public void testFindByUserIdSepcChar3() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/findByUserId?userId=1%231", UserDTO.class);
        assertEquals("1#1", user.getUserId());
        assertEquals("hello world", user.getUserName());
    }

    @Test
    public void testFindByUserIdName() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/findByUserIdName?userId=1%231&name=shenyu%20666", UserDTO.class);
        assertEquals("1#1", user.getUserId());
        assertEquals("shenyu 666", user.getUserName());
    }

    @Test
    public void testFindByUserIdName1() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/findByUserIdName?userId=1%231&name=[shenyu%20666]", UserDTO.class);
        assertEquals("1#1", user.getUserId());
        assertEquals("[shenyu 666]", user.getUserName());
    }

    @Test
    public void testFindByPage() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/findByPage?keyword=http-test&page=1&pageSize=10", UserDTO.class);
        assertEquals("http-test", user.getUserId());
        assertEquals("hello world keyword is http-test page is 1 pageSize is 10", user.getUserName());
    }

    @Test
    public void testGetPathVariable() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/path/1?name=http-test", UserDTO.class);
        assertEquals("1", user.getUserId());
        assertEquals("http-test", user.getUserName());
    }

    @Test
    public void testRestFul() throws IOException {
        UserDTO user = HttpHelper.INSTANCE.getFromGateway("/http/test/path/1/name", UserDTO.class);
        assertEquals("1", user.getUserId());
        assertEquals("hello world", user.getUserName());
    }

    @Test
    public void testPutPathVariableAndBody() throws IOException {
        UserDTO userDTO = new UserDTO();
        UserDTO user = HttpHelper.INSTANCE.putGateway("/http/test/putPathBody/1", userDTO, UserDTO.class);
        assertEquals("1", user.getUserId());
        assertEquals("hello world", user.getUserName());
    }

    @Test
    public void testPass() throws IOException {
        ResultBean response = HttpHelper.INSTANCE.postGateway("/http/test/waf/pass", ResultBean.class);
        assertEquals(Integer.valueOf(200), response.getCode());
        assertEquals("pass", response.getMsg());
    }

    @Test
    public void testDeny() throws IOException {
        ResultBean response = HttpHelper.INSTANCE.postGateway("/http/test/waf/deny", ResultBean.class);
        assertEquals(Integer.valueOf(403), response.getCode());
        assertEquals("deny", response.getMsg());
    }

    @Test
    public void testRequestParameter() throws IOException {
        ResultBean response = HttpHelper.INSTANCE.getFromGateway("/http/test/request/parameter/pass?requestParameter=http-test", ResultBean.class);
        assertEquals(Integer.valueOf(200), response.getCode());
        assertEquals("pass", response.getMsg());
        assertEquals("http-test", ((Map<?, ?>) response.getData()).get("requestParameter"));
    }

    @Test
    public void testRequestHeader() throws IOException {
        Map<String, Object> headers = new HashMap<>(2, 1);
        headers.put("requestHeader", "http-test");
        ResultBean response = HttpHelper.INSTANCE.getFromGateway("/http/test/request/header/pass", headers, ResultBean.class);
        assertEquals(Integer.valueOf(200), response.getCode());
        assertEquals("pass", response.getMsg());
        assertEquals("http-test", ((Map<?, ?>) response.getData()).get("requestHeader"));
    }

    @Test
    public void testRequestCookie() throws IOException {
        Map<String, Object> headers = new HashMap<>(2, 1);
        headers.put("Cookie", "cookie=http-test");
        ResultBean response = HttpHelper.INSTANCE.getFromGateway("/http/test/request/cookie/pass", headers, ResultBean.class);
        assertEquals(Integer.valueOf(200), response.getCode());
        assertEquals("pass", response.getMsg());
        assertTrue(response.getData() instanceof Map);
        assertEquals("http-test", ((Map<?, ?>) response.getData()).get("cookie"));
    }

    @Test
    public void testSentinelPass() throws IOException {
        ResultBean response = HttpHelper.INSTANCE.postGateway("/http/test/sentinel/pass", ResultBean.class);
        assertEquals(Integer.valueOf(200), response.getCode());
        assertEquals("pass", response.getMsg());
    }

    @Test
    public void testModifyResponse() throws IOException {
        Map<String, Object> headers = new HashMap<>();
        Response response = HttpHelper.INSTANCE.getResponseFromGateway("/http/test/modifyResponse", headers);
        assertEquals(true, Boolean.valueOf(response.header("useByModifyResponse")));
        assertEquals(true, Boolean.valueOf(response.header("setHeadersExist")));
        assertEquals(true, Boolean.valueOf(response.header("replaceHeaderKeys")));
        assertEquals(true, Boolean.valueOf(response.header("removeHeaderKeys")));
        Map<String, Object> result = GsonUtils.getInstance().toObjectMap(Objects.requireNonNull(response.body()).string());
        assertEquals(true, result.get("originReplaceBodyKeys"));
        assertEquals(true, result.get("removeBodyKeys"));
    }

    @Test
    public void testDownload() throws IOException {
        Map<String, Object> headers = new HashMap<>();
        headers.put("Connection", "close");
        String content = "testDownload";
        Response response = HttpHelper.INSTANCE.getResponseFromGateway("/http/test/download?body=" + content, headers);
        try (BufferedReader inputStream = new BufferedReader(new InputStreamReader(response.body().byteStream()))) {
            String line = inputStream.readLine();
            assertEquals(line, content);
        }
    }

    @Test
    public void testUpload() throws IOException {
        File uploadFile = File.createTempFile(String.valueOf(System.currentTimeMillis()), "-uploadFile.txt");
        try (FileOutputStream fos = new FileOutputStream(uploadFile)) {
            fos.write("testContent".getBytes(StandardCharsets.UTF_8));
        }
        RequestBody requestBody = new MultipartBody.Builder()
                .setType(MediaType.parse(MULTIPART_FORM_DATA_VALUE))
                .addFormDataPart("file", uploadFile.getName(), RequestBody.create(MediaType.parse(APPLICATION_OCTET_STREAM_VALUE), uploadFile))
                .build();
        String ret = HttpHelper.INSTANCE.postGateway("/http/test/upload", requestBody, String.class);
        assertEquals(ret, "OK");
    }

    @Test
    public void testResponseBodyIsNull() throws IOException {
        Object result = HttpHelper.INSTANCE.getFromGateway("/http/test/nullResponse", null);
        assertNull(result);
    }

    @Test
    public void testBigRequestBody() throws IOException {
        UserDTO userDTO = new UserDTO();
        String id = RandomStringUtils.randomAlphanumeric(2048);
        userDTO.setUserId(id);
        String name = RandomStringUtils.randomAlphanumeric(2048);
        userDTO.setUserName(name);
        ResultBean resultBean = HttpHelper.INSTANCE.postGateway("/http/test/bigRequestBody", userDTO, ResultBean.class);
        UserDTO userDTORet = GsonUtils.getInstance().fromJson(String.valueOf(resultBean.getData()), UserDTO.class);
        assertEquals(id, userDTORet.getUserId());
        assertEquals(name, userDTORet.getUserName());
    }
}
