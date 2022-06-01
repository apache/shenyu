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

package org.apache.shenyu.admin.utils;

import okhttp3.Cookie;
import okhttp3.CookieJar;
import okhttp3.FormBody;
import okhttp3.HttpUrl;
import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.commons.codec.digest.DigestUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * HTTP request tool, based on okhttp3.
 */
public class HttpUtils {
    private static final MediaType MEDIA_TYPE_JSON = MediaType.parse("application/json; charset=utf-8");

    private Map<String, List<Cookie>> cookieStore = new HashMap<String, List<Cookie>>();

    private OkHttpClient httpClient;

    /**
     * HttpUtils.
     */
    public HttpUtils() {
        this(new HttpToolConfig());
    }

    /**
     * HttpUtils.
     *
     * @param httpToolConfig httpToolConfig
     */
    public HttpUtils(final HttpToolConfig httpToolConfig) {
        this.initHttpClient(httpToolConfig);
    }

    /**
     * buildRequestBuilder.
     *
     * @param url    url
     * @param form   form
     * @param method method
     * @return Request
     */
    public static Request.Builder buildRequestBuilder(final String url, final Map<String, ?> form,
        final HTTPMethod method) {
        switch (method) {
            case GET:
                return new Request.Builder()
                    .url(buildHttpUrl(url, form))
                    .get();
            case HEAD:
                return new Request.Builder()
                    .url(buildHttpUrl(url, form))
                    .head();
            case PUT:
                return new Request.Builder()
                    .url(buildHttpUrl(url))
                    .put(buildFormBody(form));
            case DELETE:
                return new Request.Builder()
                    .url(buildHttpUrl(url))
                    .delete(buildFormBody(form));
            default:
                return new Request.Builder()
                    .url(buildHttpUrl(url))
                    .post(buildFormBody(form));
        }
    }

    /**
     * buildHttpUrl.
     *
     * @param url  url
     * @return HttpUrl
     */
    public static HttpUrl buildHttpUrl(final String url) {
        return buildHttpUrl(url, null);
    }

    /**
     * buildHttpUrl.
     *
     * @param url  url
     * @param form form
     * @return HttpUrl
     */
    public static HttpUrl buildHttpUrl(final String url, final Map<String, ?> form) {
        HttpUrl.Builder urlBuilder = HttpUrl.parse(url).newBuilder();
        if (Objects.nonNull(form) && !form.isEmpty()) {
            for (Map.Entry<String, ?> entry : form.entrySet()) {
                urlBuilder.addQueryParameter(entry.getKey(), String.valueOf(entry.getValue()));
            }
        }
        return urlBuilder.build();
    }

    /**
     * buildFormBody.
     *
     * @param form form
     * @return FormBody
     */
    public static FormBody buildFormBody(final Map<String, ?> form) {
        FormBody.Builder paramBuilder = new FormBody.Builder(StandardCharsets.UTF_8);
        for (Map.Entry<String, ?> entry : form.entrySet()) {
            paramBuilder.add(entry.getKey(), String.valueOf(entry.getValue()));
        }
        return paramBuilder.build();
    }

    protected void initHttpClient(final HttpToolConfig httpToolConfig) {
        httpClient = new OkHttpClient.Builder()
            .connectTimeout(httpToolConfig.connectTimeoutSeconds, TimeUnit.SECONDS)
            .readTimeout(httpToolConfig.readTimeoutSeconds, TimeUnit.SECONDS)
            .writeTimeout(httpToolConfig.writeTimeoutSeconds, TimeUnit.SECONDS)
            .cookieJar(new CookieJar() {
                @Override
                public void saveFromResponse(final HttpUrl httpUrl, final List<Cookie> list) {
                    cookieStore.put(httpUrl.host(), list);
                }

                @Override
                public List<Cookie> loadForRequest(final HttpUrl httpUrl) {
                    List<Cookie> cookies = cookieStore.get(httpUrl.host());
                    return cookies != null ? cookies : new ArrayList<Cookie>();
                }
            }).build();
    }

    /**
     * get request.
     *
     * @param url    url
     * @param header header
     * @return String
     * @throws IOException IOException
     */
    public String get(final String url, final Map<String, String> header) throws IOException {
        Request.Builder builder = new Request.Builder().url(url).get();
        addHeader(builder, header);

        Request request = builder.build();
        Response response = httpClient.newCall(request).execute();
        return response.body().string();
    }

    /**
     * Submit the form.
     *
     * @param url    url
     * @param form   param
     * @param header header
     * @param method http method
     * @return String
     * @throws IOException IOException
     */
    public String request(final String url, final Map<String, ?> form, final Map<String, String> header,
        final HTTPMethod method) throws IOException {
        Request.Builder requestBuilder = buildRequestBuilder(url, form, method);
        addHeader(requestBuilder, header);

        Request request = requestBuilder.build();
        Response response = httpClient
            .newCall(request)
            .execute();
        try {
            return response.body().string();
        } finally {
            response.close();
        }
    }

    /**
     * request json data，contentType=application/json.
     *
     * @param url    url
     * @param json   json
     * @param header header
     * @return String
     * @throws IOException IOException
     */
    public String requestJson(final String url, final String json,
        final Map<String, String> header) throws IOException {
        RequestBody body = RequestBody.create(MEDIA_TYPE_JSON, json);
        Request.Builder requestBuilder = new Request.Builder()
            .url(url)
            .post(body);
        addHeader(requestBuilder, header);

        Request request = requestBuilder.build();
        Response response = httpClient
            .newCall(request)
            .execute();
        try {
            return response.body().string();
        } finally {
            response.close();
        }
    }

    /**
     * requestFileString.
     *
     * @param url    url
     * @param form   form
     * @param header header
     * @param files  files
     * @return String
     * @throws IOException IOException
     */
    public String requestFileString(final String url, final Map<String, ?> form, final Map<String, String> header,
        final List<UploadFile> files) throws IOException {
        return requestFile(url, form, header, files).body().string();
    }

    /**
     * Submit the form and upload the file.
     *
     * @param url    url
     * @param form   form
     * @param header header
     * @param files  files
     * @return Response
     * @throws IOException IOException
     */
    public Response requestFile(final String url, final Map<String, ?> form, final Map<String, String> header,
        final List<UploadFile> files)
        throws IOException {
        MultipartBody.Builder bodyBuilder = new MultipartBody.Builder();
        bodyBuilder.setType(MultipartBody.FORM);

        for (UploadFile uploadFile : files) {
            bodyBuilder.addFormDataPart(uploadFile.getName(),
                // The name of the file, which is used by the server for parsing.
                uploadFile.getFileName(),
                // Create the requestbody and put the uploaded file into the.
                RequestBody.create(null, uploadFile.getFileData())
            );
        }

        for (Map.Entry<String, ?> entry : form.entrySet()) {
            bodyBuilder.addFormDataPart(entry.getKey(), String.valueOf(entry.getValue()));
        }

        RequestBody requestBody = bodyBuilder.build();
        Request.Builder builder = new Request.Builder().url(buildHttpUrl(url)).post(requestBody);
        addHeader(builder, header);

        Request request = builder.build();
        return httpClient.newCall(request).execute();
    }

    /**
     * request.
     *
     * @param url    url
     * @param form   form
     * @param header header
     * @param method method
     * @param files  files
     * @return Response
     * @throws IOException IOException
     */
    public Response requestCall(final String url, final Map<String, ?> form, final Map<String, String> header,
        final HTTPMethod method, final List<UploadFile> files) throws IOException {
        if (Objects.nonNull(files) && files.size() > 0) {
            return requestFile(url, form, header, files);
        } else {
            return requestForResponse(url, form, header, method);
        }
    }

    /**
     * request data.
     *
     * @param url    request url
     * @param form   request data
     * @param header header
     * @param method method
     * @return Response Response
     * @throws IOException IOException
     */
    public Response requestForResponse(final String url, final Map<String, ?> form, final Map<String, String> header,
        final HTTPMethod method) throws IOException {
        Request.Builder requestBuilder = buildRequestBuilder(url, form, method);
        addHeader(requestBuilder, header);
        Request request = requestBuilder.build();
        return httpClient
            .newCall(request)
            .execute();
    }

    /**
     * download file.
     *
     * @param url    request url
     * @param form   request data
     * @param header header
     * @return InputStream
     * @throws IOException IOException
     */
    public InputStream downloadFile(final String url, final Map<String, ?> form,
        final Map<String, String> header) throws IOException {
        Request.Builder requestBuilder = buildRequestBuilder(url, form, HTTPMethod.GET);
        addHeader(requestBuilder, header);

        Request request = requestBuilder.build();
        Response response = httpClient
            .newCall(request)
            .execute();
        if (response.isSuccessful()) {
            ResponseBody body = response.body();
            return body == null ? null : body.byteStream();
        }
        return null;
    }

    /**
     * setCookieStore.
     *
     * @param cookieStore cookieStore
     */
    public void setCookieStore(final Map<String, List<Cookie>> cookieStore) {
        this.cookieStore = cookieStore;
    }

    /**
     * setHttpClient.
     *
     * @param httpClient httpClient
     */
    public void setHttpClient(final OkHttpClient httpClient) {
        this.httpClient = httpClient;
    }

    private void addHeader(final Request.Builder builder, final Map<String, String> header) {
        if (header != null) {
            Set<Map.Entry<String, String>> entrySet = header.entrySet();
            for (Map.Entry<String, String> entry : entrySet) {
                builder.addHeader(entry.getKey(), String.valueOf(entry.getValue()));
            }
        }
    }

    public enum HTTPMethod {
        GET,
        POST,
        PUT,
        HEAD,
        DELETE;

        HTTPMethod() {
        }

        /**
         * fromValue.
         *
         * @param v v
         * @return HTTPMethod
         */
        public static HTTPMethod fromValue(final String v) {
            return valueOf(v.toUpperCase());
        }

        /**
         * value().
         *
         * @return String
         */
        public String value() {
            return this.name();
        }
    }

    public static class HttpToolConfig {

        private int connectTimeoutSeconds = 10;

        private int readTimeoutSeconds = 10;

        private int writeTimeoutSeconds = 10;

        /**
         * Request timeout.
         *
         * @return int
         */
        public int getConnectTimeoutSeconds() {
            return connectTimeoutSeconds;
        }

        /**
         * setConnectTimeoutSeconds.
         *
         * @param connectTimeoutSeconds connectTimeoutSeconds
         */
        public void setConnectTimeoutSeconds(final int connectTimeoutSeconds) {
            this.connectTimeoutSeconds = connectTimeoutSeconds;
        }

        /**
         * HTTP read timeout.
         *
         * @return int
         */
        public int getReadTimeoutSeconds() {
            return readTimeoutSeconds;
        }

        /**
         * setReadTimeoutSeconds.
         *
         * @param readTimeoutSeconds readTimeoutSeconds
         */
        public void setReadTimeoutSeconds(final int readTimeoutSeconds) {
            this.readTimeoutSeconds = readTimeoutSeconds;
        }

        /**
         * HTTP write timeout.
         *
         * @return int
         */
        public int getWriteTimeoutSeconds() {
            return writeTimeoutSeconds;
        }

        /**
         * setWriteTimeoutSeconds.
         *
         * @param writeTimeoutSeconds writeTimeoutSeconds
         */
        public void setWriteTimeoutSeconds(final int writeTimeoutSeconds) {
            this.writeTimeoutSeconds = writeTimeoutSeconds;
        }
    }

    /**
     * Upload File bean.
     */
    public static class UploadFile implements Serializable {

        private static final long serialVersionUID = -1100614660944996398L;

        private String name;

        private String fileName;

        private byte[] fileData;

        private String md5;

        /**
         * Upload File.
         * @param name The form name cannot be duplicate.
         * @param file file
         * @throws IOException IOException
         */
        public UploadFile(final String name, final File file) throws IOException {
            this(name, file.getName(), FileUtils.toBytes(file));
        }

        /**
         * UploadFile.
         *
         * @param name     The form name cannot be duplicate.
         * @param fileName fileName
         * @param input    inputStream
         * @throws IOException IOException
         */
        public UploadFile(final String name, final String fileName, final InputStream input) throws IOException {
            this(name, fileName, FileUtils.toBytes(input));
        }

        /**
         * The form name cannot be duplicate.
         *
         * @param name     The form name cannot be duplicate.
         * @param fileName fileName
         * @param fileData fileData
         */
        public UploadFile(final String name, final String fileName, final byte[] fileData) {
            super();
            this.name = name;
            this.fileName = fileName;
            this.fileData = fileData;
            this.md5 = DigestUtils.md5Hex(fileData);
        }

        /**
         * getName.
         *
         * @return String
         */
        public String getName() {
            return name;
        }

        /**
         * setName.
         *
         * @param name name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * getFileName.
         *
         * @return String
         */
        public String getFileName() {
            return fileName;
        }

        /**
         * setFileName.
         *
         * @param fileName fileName
         */
        public void setFileName(final String fileName) {
            this.fileName = fileName;
        }

        /**
         * getFileData.
         *
         * @return byte[]
         */
        public byte[] getFileData() {
            return fileData;
        }

        /**
         * setFileData.
         *
         * @param fileData fileData
         */
        public void setFileData(final byte[] fileData) {
            this.fileData = fileData;
        }

        /**
         * getMd5.
         *
         * @return String
         */
        public String getMd5() {
            return md5;
        }

        /**
         * setMd5.
         *
         * @param md5 md5
         */
        public void setMd5(final String md5) {
            this.md5 = md5;
        }
    }

    public static class FileUtils {

        /**
         * The default buffer size to use.
         */
        private static final int DEFAULT_BUFFER_SIZE = 1024 * 4;

        private static final int EOF = -1;

        /**
         * InputStream to byte[].
         *
         * @param input input
         * @return byte
         * @throws IOException IOException
         */
        public static byte[] toBytes(final InputStream input) throws IOException {
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            int n = 0;
            byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];

            while (EOF != (n = input.read(buffer))) {
                output.write(buffer, 0, n);
            }
            return output.toByteArray();
        }

        /**
         * file to bytes.
         *
         * @param file file
         * @return byte
         * @throws IOException IOException
         */
        public static byte[] toBytes(final File file) throws IOException {
            if (file.exists()) {
                if (file.isDirectory()) {
                    throw new IOException("File '" + file + "' exists but is a directory");
                }
                if (!file.canRead()) {
                    throw new IOException("File '" + file + "' cannot be read");
                }
            } else {
                throw new FileNotFoundException("File '" + file + "' does not exist");
            }
            InputStream input = null;
            try {
                input = new FileInputStream(file);
                return toBytes(input);
            } finally {
                try {
                    if (input != null) {
                        input.close();
                    }
                } catch (IOException ioe) {
                    System.err.println(ioe);
                }
            }
        }
    }
}
