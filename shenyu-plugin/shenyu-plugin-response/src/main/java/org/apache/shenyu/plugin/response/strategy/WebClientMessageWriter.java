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

package org.apache.shenyu.plugin.response.strategy;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.utils.ResponseUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.reactive.function.BodyExtractors;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.StringJoiner;

/**
 * The type Web client message writer.
 */
public class WebClientMessageWriter implements MessageWriter {

    /**
     * the common binary media type regex.
     */
    private static final String COMMON_BIN_MEDIA_TYPE_REGEX;

    /**
     * the cross headers.
     */
    private static final Set<String> CORS_HEADERS = new HashSet<String>() {
        {
            add(HttpHeaders.ACCESS_CONTROL_ALLOW_METHODS);
            add(HttpHeaders.ACCESS_CONTROL_MAX_AGE);
            add(HttpHeaders.ACCESS_CONTROL_ALLOW_HEADERS);
            add(HttpHeaders.ACCESS_CONTROL_EXPOSE_HEADERS);
        }
    };

    @Override
    public Mono<Void> writeWith(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange).then(Mono.defer(() -> {
            ServerHttpResponse response = exchange.getResponse();
            ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
            if (Objects.isNull(clientResponse)) {
                Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SERVICE_RESULT_ERROR);
                return WebFluxResultUtils.result(exchange, error);
            }
            this.redrawResponseHeaders(response, clientResponse);
            // image, pdf or stream does not do format processing.
            if (clientResponse.headers().contentType().isPresent()) {
                final String media = clientResponse.headers().contentType().get().toString().toLowerCase();
                if (media.matches(COMMON_BIN_MEDIA_TYPE_REGEX)) {
                    return response.writeWith(clientResponse.body(BodyExtractors.toDataBuffers()))
                            .doOnCancel(() -> clean(exchange));
                }
            }
            clientResponse = ResponseUtils.buildClientResponse(response, clientResponse.body(BodyExtractors.toDataBuffers()));
            return clientResponse.bodyToMono(byte[].class)
                    .flatMap(originData -> WebFluxResultUtils.result(exchange, originData))
                    .doOnCancel(() -> clean(exchange));
        }));
    }
    
    @Override
    public List<String> supportTypes() {
        return Lists.newArrayList(RpcTypeEnum.HTTP.getName(), RpcTypeEnum.SPRING_CLOUD.getName());
    }
    
    private void redrawResponseHeaders(final ServerHttpResponse response,
                                       final ClientResponse clientResponse) {
        response.getCookies().putAll(clientResponse.cookies());
        HttpHeaders httpHeaders = clientResponse.headers().asHttpHeaders();
        // if the client response has cors header remove cors header from response that crossfilter put
        if (CORS_HEADERS.stream().anyMatch(httpHeaders::containsKey)) {
            CORS_HEADERS.forEach(header -> response.getHeaders().remove(header));
        }
        // shenyu transfer the cookies so the withCredentials from request is the true,
        // the Access-Control-Allow-Origin cannot use "*", so use the shenyu crossFilter pushed data
        // and the ACCESS_CONTROL_ALLOW_CREDENTIALS must set true
        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials
        if (httpHeaders.containsKey(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN)) {
            HttpHeaders temp = new HttpHeaders();
            temp.putAll(httpHeaders);
            temp.remove(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN);
            httpHeaders = temp;
        }
        response.getHeaders().putAll(httpHeaders);
    }

    private void clean(final ServerWebExchange exchange) {
        ClientResponse clientResponse = exchange.getAttribute(Constants.CLIENT_RESPONSE_ATTR);
        if (Objects.nonNull(clientResponse)) {
            clientResponse.bodyToMono(Void.class).subscribe();
        }
    }

    static {
        // https://www.iana.org/assignments/media-types/media-types.xhtml
        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
        // https://en.wikipedia.org/wiki/Media_type
        // image => .png .jpg .jpeg .gif .webp
        // audio => .mp2 .mp3
        // video => .avi .mp4
        // application/ogg => ogg
        // zip => .zip .tar .gz
        // rar => .rar
        // word => .doc
        // excel => .xls
        // csv => .csv
        // powerpoint => .ppt
        // openxmlformats-officedocument => .pptx .xlsx .docx
        // binary => .bin
        // pdf => .pdf
        // octet-stream => octet-stream
        // force-download => force-download
        Set<String> commonBinaryTypes = new HashSet<String>() {
            {
                add("image");
                add("audio");
                add("video");
                add("ogg");
                add("zip");
                add("rar");
                add("word");
                add("excel");
                add("csv");
                add("powerpoint");
                add("openxmlformats-officedocument");
                add("binary");
                add("pdf");
                add("octet-stream");
                add("force-download");
            }
        };
        StringJoiner regexBuilder = new StringJoiner("|");
        commonBinaryTypes.forEach(t -> regexBuilder.add(String.format(".*%s.*", t)));
        COMMON_BIN_MEDIA_TYPE_REGEX = regexBuilder.toString();
    }
}
