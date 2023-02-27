package org.apache.shenyu.register.client.polaris.common;

import org.apache.shenyu.register.client.polaris.model.ResponseResult;
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.classic.methods.HttpPut;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.core5.http.*;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.message.BasicClassicHttpRequest;
import org.apache.hc.core5.net.URIBuilder;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class HttpOperator {

    private static final int DEFAULT_HTTP_TIMEOUT = 5000;

    private final CloseableHttpClient httpClient;

    public HttpOperator() {
        this.httpClient = HttpClientBuilder.create()
                .setDefaultRequestConfig(RequestConfig.custom()
                        .setConnectionRequestTimeout(DEFAULT_HTTP_TIMEOUT, TimeUnit.MILLISECONDS)
                        .setResponseTimeout(DEFAULT_HTTP_TIMEOUT, TimeUnit.MILLISECONDS).build())
                .build();
    }

    public ResponseResult send(BasicClassicHttpRequest request) throws IOException {

        return httpClient.execute(request, response -> {
            ResponseResult responseResult = new ResponseResult();
            responseResult.setStatus(response.getCode());
            responseResult.setData(EntityUtils.toString(response.getEntity()));
            response.close();
            return responseResult;
        });
    }

    public BasicClassicHttpRequest buildReqOnBody(String url, Method method, List<Header> headers,String body) throws IOException {
        URI uri = URI.create(url);

        if (Method.POST.equals(method)) {
            HttpPost httpPost = new HttpPost(uri);
            for (Header header : headers) {
                httpPost.setHeader(header);
            }
            httpPost.setEntity(new StringEntity(body));
            return httpPost;
        }

        if (Method.PUT.equals(method)) {
            HttpPut httpPut = new HttpPut(uri);
            for (Header header : headers) {
                httpPut.setHeader(header);
            }
            httpPut.setEntity(new StringEntity(body));
            return httpPut;
        }
        throw new RuntimeException("Unsupported request method");
    }

    public BasicClassicHttpRequest buildReqOnUrlParam(String url, Method method, List<Header> headers, List<NameValuePair> param) throws URISyntaxException {
        URIBuilder uriBuilder=new URIBuilder(url);
        URI build = uriBuilder.addParameters(param).build();
        if (Method.GET.equals(method)) {
            HttpGet httpGet = new HttpGet(build);
            for (Header header : headers) {
                httpGet.setHeader(header);
            }
            return httpGet;
        }
        throw new RuntimeException("Unsupported request method");
    }


}
