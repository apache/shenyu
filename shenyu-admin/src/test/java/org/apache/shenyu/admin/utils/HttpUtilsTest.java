package org.apache.shenyu.admin.utils;

import okhttp3.FormBody;
import okhttp3.HttpUrl;
import okhttp3.Request;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * HTTP request tool test {@link HttpUtils}
 *
 * @author: hdd
 * @create: 2023/04/11
 */
public class HttpUtilsTest {


    private static final String TEST_URL = "http://127.0.0.1/";

    private static final String ACTUAL_PARAM_URL = "http://127.0.0.1/?param-1=123&param-2=456";

    private Map<String, Object> formMap = new HashMap<>();

    {
        formMap.put("param-1", "123");
        formMap.put("param-2", 456);
    }


    @Test
    public void buildRequestBuilderForGETTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.GET);
        Assert.assertNotNull(builder);
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.GET.value());
        Assert.assertEquals(builder.build().url().toString(), ACTUAL_PARAM_URL);
    }

    @Test
    public void buildRequestBuilderForPOSTTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.POST);
        Assert.assertNotNull(builder);
        Assert.assertNotNull(builder.build().body());
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.POST.value());
        Assert.assertEquals(builder.build().url().toString(), TEST_URL);
    }

    @Test
    public void buildRequestBuilderForDELETETest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.DELETE);
        Assert.assertNotNull(builder);
        Assert.assertNotNull(builder.build().body());
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.DELETE.value());
        Assert.assertEquals(builder.build().url().toString(), TEST_URL);
    }

    @Test
    public void buildRequestBuilderForPUTTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.PUT);
        Assert.assertNotNull(builder);
        Assert.assertNotNull(builder.build().body());
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.PUT.value());
        Assert.assertEquals(builder.build().url().toString(), TEST_URL);
    }

    @Test
    public void buildRequestBuilderForHEADTest() {
        Request.Builder builder = HttpUtils.buildRequestBuilder(TEST_URL, formMap, HttpUtils.HTTPMethod.HEAD);
        Assert.assertNotNull(builder);
        Assert.assertEquals(builder.build().method(), HttpUtils.HTTPMethod.HEAD.value());
        Assert.assertEquals(builder.build().url().toString(), ACTUAL_PARAM_URL);
    }

    @Test
    public void buildHttpUrlTest() {
        HttpUrl httpUrl = HttpUtils.buildHttpUrl(TEST_URL, formMap);
        Assert.assertNotNull(httpUrl);
    }

    @Test
    public void buildFormBodyTest() {
        FormBody formBody = HttpUtils.buildFormBody(formMap);
        Assert.assertNotNull(formBody);
    }

    @Test
    public void fileUtilsToBytesByFileExistsTest() {
        String path = HttpUtilsTest.class.getClassLoader().getResource("./").getPath();
        File file = new File(path + "application.yml");
        Assertions.assertDoesNotThrow(() -> HttpUtils.FileUtils.toBytes(file));
    }


    @Test
    public void fileUtilsToBytesByFileNotExistsTest() {
        File file = new File("");
        Assertions.assertThrows(IOException.class, () ->HttpUtils.FileUtils.toBytes(file));
    }
}
