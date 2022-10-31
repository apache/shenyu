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

package org.apache.shenyu.integratedtest.common.helper;

import com.google.gson.Gson;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import org.apache.shenyu.common.constant.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Objects;

/**
 * The type Http helper.
 */
public class HttpHelper {

    /**
     * The constant INSTANCE.
     */
    public static final HttpHelper INSTANCE = new HttpHelper();

    /**
     * The constant GATEWAY_END_POINT.
     */
    public static final String GATEWAY_END_POINT = "http://localhost:9195";

    /**
     * The constant JSON.
     */
    public static final MediaType JSON = MediaType.parse("application/json");

    private static final Logger LOG = LoggerFactory.getLogger(HttpHelper.class);

    private static final Gson GSON = new Gson();

    private final OkHttpClient client = new OkHttpClient.Builder().build();
    
    private final String localKey = "123456";

    /**
     * Send a post http request to shenyu gateway.
     *
     * @param <S>      type of response object
     * @param <Q>      type of request object
     * @param path     path
     * @param req      request body as an object
     * @param respType response type passed to {@link Gson#fromJson(String, Type)}
     * @return response s
     * @throws IOException IO exception
     */
    public <S, Q> S postGateway(final String path, final Q req, final Type respType) throws IOException {
        return postGateway(path, null, req, respType);
    }

    /**
     * Send a post http request to shenyu gateway.
     *
     * @param <S>      type of response object
     * @param <Q>      type of request object
     * @param path     path
     * @param headers  http header
     * @param req      request body as an object
     * @param respType response type passed to {@link Gson#fromJson(String, Type)}
     * @return response s
     * @throws IOException IO exception
     */
    public <S, Q> S postGateway(final String path, final Map<String, Object> headers, final Q req, final Type respType) throws IOException {
        String respBody = post(path, headers, req);
        LOG.info("postGateway({}) resp({})", path, respBody);
        try {
            return GSON.fromJson(respBody, respType);
        } catch (Exception e) {
            return (S) respBody;
        }
    }

    /**
     * Send a post http request to shenyu gateway.
     *
     * @param <S>      type of response object
     * @param <Q>      type of request object
     * @param path     path
     * @param respType response type passed to {@link Gson#fromJson(String, Type)}
     * @return response s
     * @throws IOException IO exception
     */
    public <S, Q> S postGateway(final String path, final Type respType) throws IOException {
        return postGateway(path, "", respType);
    }

    /**
     * Send a post http request to shenyu gateway.
     *
     * @param <S>      type of response object
     * @param <Q>      type of request object
     * @param path     path
     * @param req      request body as an object
     * @param respType response type passed to {@link Gson#fromJson(String, Class)}
     * @return response s
     * @throws IOException IO exception
     */
    public <S, Q> S postGateway(final String path, final Q req, final Class<S> respType) throws IOException {
        return postGateway(path, null, req, respType);
    }

    /**
     * Send a post http request to shenyu gateway with header.
     *
     * @param <S>      type of response object
     * @param <Q>      type of request object
     * @param path     path
     * @param headers  http header
     * @param req      request body as an object
     * @param respType response type passed to {@link Gson#fromJson(String, Class)}
     * @return response s
     * @throws IOException IO exception
     */
    public <S, Q> S postGateway(final String path, final Map<String, Object> headers, final Q req, final Class<S> respType) throws IOException {
        String respBody = post(path, headers, req);
        LOG.info("postGateway({}) resp({})", path, respBody);
        try {
            return GSON.fromJson(respBody, respType);
        } catch (Exception e) {
            return (S) respBody;
        }
    }

    /**
     * Send a post http request to shenyu gateway with custom requestBody.
     *
     * @param <S>         type of response object
     * @param path        path
     * @param requestBody request Body
     * @param respType    response type passed to {@link Gson#fromJson(String, Class)}
     * @return response s
     * @throws IOException IO exception
     */
    public <S> S postGateway(final String path, final RequestBody requestBody, final Class<S> respType) throws IOException {
        Request.Builder requestBuilder = new Request.Builder().post(requestBody).url(GATEWAY_END_POINT + path).addHeader(Constants.LOCAL_KEY, localKey);
        Response response = client.newCall(requestBuilder.build()).execute();
        String respBody = Objects.requireNonNull(response.body()).string();
        try {
            return GSON.fromJson(respBody, respType);
        } catch (Exception e) {
            return (S) respBody;
        }
    }

    private <Q> String post(final String path, final Map<String, Object> headers, final Q req) throws IOException {
        Request.Builder requestBuilder = new Request.Builder().post(RequestBody.create(GSON.toJson(req), JSON)).url(GATEWAY_END_POINT + path).addHeader(Constants.LOCAL_KEY, localKey);
        if (!CollectionUtils.isEmpty(headers)) {
            headers.forEach((key, value) -> requestBuilder.addHeader(key, String.valueOf(value)));
        }
        Response response = client.newCall(requestBuilder.build()).execute();
        return Objects.requireNonNull(response.body()).string();
    }

    /**
     * Send a put http request to shenyu gateway.
     *
     * @param <S>      type of response object
     * @param <Q>      type of request object
     * @param path     path
     * @param req      request body as an object
     * @param respType response type passed to {@link Gson#fromJson(String, Class)}
     * @return response s
     * @throws IOException IO exception
     */
    public <S, Q> S putGateway(final String path, final Q req, final Class<S> respType) throws IOException {
        Request request = new Request.Builder().put(RequestBody.create(GSON.toJson(req), JSON)).url(GATEWAY_END_POINT + path).addHeader(Constants.LOCAL_KEY, localKey).build();
        Response response = client.newCall(request).execute();
        String respBody = Objects.requireNonNull(response.body()).string();
        LOG.info("postGateway({}) resp({})", path, respBody);
        try {
            return GSON.fromJson(respBody, respType);
        } catch (Exception e) {
            return (S) respBody;
        }
    }

    /**
     * Send a get http request to shenyu gateway without headers.
     *
     * @param <S>  response type
     * @param path path
     * @param type type of response passed to {@link Gson#fromJson(String, Type)}
     * @return response from gateway
     * @throws IOException IO exception
     */
    public <S> S getFromGateway(final String path, final Type type) throws IOException {
        return this.getFromGateway(path, null, type);
    }

    /**
     * Send a get http request to shenyu gateway with headers.
     *
     * @param <S>     response type
     * @param path    path
     * @param headers headers
     * @param type    type of response passed to {@link Gson#fromJson(String, Type)}
     * @return response from gateway
     * @throws IOException IO exception
     */
    public <S> S getFromGateway(final String path, final Map<String, Object> headers, final Type type) throws IOException {
        Response response = getHttpService(GATEWAY_END_POINT + path, headers);
        String respBody = Objects.requireNonNull(response.body()).string();
        LOG.info("getFromGateway({}) resp({})", path, respBody);
        try {
            return GSON.fromJson(respBody, type);
        } catch (Exception e) {
            return (S) respBody;
        }
    }

    /**
     * Send a get http request to shenyu gateway with headers.
     *
     * @param path    path
     * @param headers headers
     * @return response from gateway
     * @throws IOException IO exception
     */
    public Response getResponseFromGateway(final String path, final Map<String, Object> headers) throws IOException {
        return getHttpService(GATEWAY_END_POINT + path, headers);
    }

    /**
     * Send a get http request to http service with headers.
     *
     * @param url     url
     * @param headers headers
     * @return response
     * @throws IOException IO exception
     */
    public Response getHttpService(final String url, final Map<String, Object> headers) throws IOException {
        Request.Builder requestBuilder = new Request.Builder().url(url).addHeader(Constants.LOCAL_KEY, localKey);
        if (!CollectionUtils.isEmpty(headers)) {
            headers.forEach((key, value) -> requestBuilder.addHeader(key, String.valueOf(value)));
        }
        Request request = requestBuilder.build();
        return client.newCall(request).execute();
    }

    /**
     * Send a get http request to shenyu gateway .
     *
     * @param <S>     response type
     * @param headers headers
     * @param path    path
     * @param type    type of response passed to {@link Gson#fromJson(String, Type)}
     * @return response from gateway
     * @throws IOException IO exception
     */
    public <S> S getHttpService(final String path, final Map<String, Object> headers, final Type type) throws IOException {
        Response response = getHttpService(path, headers);
        String respBody = Objects.requireNonNull(response.body()).string();
        LOG.info("getHttpService({}) resp({})", path, respBody);
        try {
            return GSON.fromJson(respBody, type);
        } catch (Exception e) {
            return (S) respBody;
        }
    }
}
