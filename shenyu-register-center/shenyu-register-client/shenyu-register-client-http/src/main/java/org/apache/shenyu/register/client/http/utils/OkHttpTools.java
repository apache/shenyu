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

package org.apache.shenyu.register.client.http.utils;

import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Headers;
import okhttp3.HttpUrl;
import org.apache.shenyu.common.constant.Constants;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * OkHttpTools.
 */
public final class OkHttpTools {

    /**
     * The constant JSON.
     */
    private static final MediaType JSON = MediaType.parse("application/json; charset=utf-8");

    private static final OkHttpTools OK_HTTP_TOOLS = new OkHttpTools();

    private final OkHttpClient client;

    private OkHttpTools() {
        OkHttpClient.Builder builder = new OkHttpClient.Builder();
        builder.connectTimeout(10, TimeUnit.SECONDS);
        builder.readTimeout(10, TimeUnit.SECONDS);
        builder.writeTimeout(10, TimeUnit.SECONDS);
        client = builder.build();
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static OkHttpTools getInstance() {
        return OK_HTTP_TOOLS;
    }

    /**
     * Post string.
     *
     * @param url  the url
     * @param json the json
     * @return the string
     * @throws IOException the io exception
     */
    public String post(final String url, final String json) throws IOException {
        RequestBody body = RequestBody.create(JSON, json);
        Request request = new Request.Builder()
                .url(url)
                .post(body)
                .build();
        return client.newCall(request).execute().body().string();
    }

    /**
     * Post string.
     *
     * @param url     the url
     * @param json    the json
     * @param headers the headers
     * @return the string
     * @throws IOException the io exception
     */
    public String post(final String url, final String json, final Headers headers) throws IOException {
        RequestBody body = RequestBody.create(JSON, json);
        Request request = new Request.Builder()
                .headers(headers)
                .url(url)
                .post(body)
                .build();
        return client.newCall(request).execute().body().string();
    }

    /**
     * Get string.
     *
     * @param url   the url
     * @param query the query
     * @return the http result
     * @throws IOException the io exception
     */
    public String get(final String url, final Map<String, Object> query) throws IOException {
        Request.Builder reqBuild = new Request.Builder();
        HttpUrl.Builder urlBuilder = HttpUrl.parse(url).newBuilder();
        query.forEach((K, V) -> urlBuilder.addQueryParameter(K, String.valueOf(V)));
        reqBuild.url(urlBuilder.build());
        Request request = reqBuild.build();
        return client.newCall(request).execute().body().string();
    }

    /**
     * Get string by username and password.
     * @param url   the url
     * @param userName the userName
     * @param passWord the passWord
     * @return the http result
     * @throws IOException the io exception
     */
    public String get(final String url, final String userName, final String passWord) throws IOException {
        Request.Builder reqBuild = new Request.Builder();
        HttpUrl.Builder urlBuilder = HttpUrl.parse(url).newBuilder();
        urlBuilder.addQueryParameter(Constants.USER_NAME, userName);
        urlBuilder.addQueryParameter(Constants.PASS_WORD, passWord);
        reqBuild.url(urlBuilder.build());
        Request request = reqBuild.build();
        return client.newCall(request).execute().body().string();
    }

}
