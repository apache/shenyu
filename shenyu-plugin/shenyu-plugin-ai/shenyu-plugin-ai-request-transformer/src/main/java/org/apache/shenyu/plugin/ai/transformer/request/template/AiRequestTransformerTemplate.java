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

package org.apache.shenyu.plugin.ai.transformer.request.template;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;


import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * this is the aiRequestTransformerTemplate.
 */
public class AiRequestTransformerTemplate {

    /**
     * sysContent.
     */
    public static final String SYS_CONTENT = "You are an expert in HTTP/1.1 protocol. Your response should contain only standard HTTP/1.1 message content.  \n"
            + "Please return a complete HTTP/1.1 request or response message, including:  \n"
            + "- Start line  \n"
            + "- Multiple headers (one header per line, format: Header-Name: Header-Value)  \n"
            + "- A blank line  \n"
            + "- A JSON-formatted body (the body must be valid JSON text)  \n"
            + "Do not include any extra explanations, comments, or text.  \n"
            + "Example:\n"
            + "\n"
            + "HTTP/1.1 200 OK\n"
            + "Content-Type: application/json\n"
            + "Authorization: Bearer xxx\n"
            + "\n"
            + "{\"name\":\"Alice\",\"age\":30}";

    /**
     * userContent.
     */
    private String userContent;

    /**
     * originalRequest.
     */
    private ServerHttpRequest originalRequest;

    private final ObjectMapper objectMapper = new ObjectMapper();

    public AiRequestTransformerTemplate(final String userContent, final ServerHttpRequest originalRequest) {
        this.userContent = userContent;
        this.originalRequest = originalRequest;
    }

    /**
     * headersToJson.
     *
     * @param headers headers
     * @return headersJsonNode
     */
    private JsonNode headersToJson(final HttpHeaders headers) {
        ObjectNode headersNode = objectMapper.createObjectNode();
        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            String joinedValues = String.join(",", entry.getValue());
            headersNode.put(entry.getKey(), joinedValues);
        }
        return headersNode;
    }

    /**
     * bodyToString.
     *
     * @param body body
     * @return requestBody
     */
    private Mono<String> bodyToString(final Flux<DataBuffer> body) {
        return DataBufferUtils.join(body)
                .map(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    DataBufferUtils.release(dataBuffer);
                    return new String(bytes, StandardCharsets.UTF_8);
                }).defaultIfEmpty("");
    }

    /**
     * parse formData to map .
     *
     * @return encodedMap
     */
    private Map<String, String> parseFormUrlEncoded(final String body) {
        if (Objects.isNull(body) || body.isEmpty()) {
            return Map.of();
        }

        return Arrays.stream(body.split("&"))
                .map(kv -> kv.split("=", 2))
                .filter(arr -> arr.length == 2)
                .collect(Collectors.toMap(
                        arr -> urlDecode(arr[0]),
                        arr -> urlDecode(arr[1]),
                        (v1, v2) -> v2,
                        LinkedHashMap::new));
    }

    private String urlDecode(final String s) {
        try {
            return URLDecoder.decode(s, StandardCharsets.UTF_8.name());
        } catch (Exception e) {
            return s;
        }
    }

    /**
     * assembleMessage .
     *
     * @return message
     */
    public Mono<String> assembleMessage() {
        JsonNode headersJson = headersToJson(originalRequest.getHeaders());

        MediaType contentType = originalRequest.getHeaders().getContentType();

        return bodyToString(originalRequest.getBody())
                .flatMap(bodyString -> {
                    ObjectNode rootNode = objectMapper.createObjectNode();
                    rootNode.put("system_prompt", SYS_CONTENT);
                    rootNode.put("user_prompt", userContent);

                    ObjectNode requestNode = objectMapper.createObjectNode();
                    requestNode.set("headers", headersJson);

                    if (Objects.nonNull(contentType)) {
                        if (MediaType.APPLICATION_JSON.isCompatibleWith(contentType)) {
                            try {
                                JsonNode bodyJsonNode = objectMapper.readTree(bodyString);
                                requestNode.set("body", bodyJsonNode);
                            } catch (Exception e) {
                                requestNode.put("body", bodyString);
                            }
                        } else if (MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(contentType)) {
                            Map<String, String> formMap = parseFormUrlEncoded(bodyString);
                            JsonNode formJson = objectMapper.valueToTree(formMap);
                            requestNode.set("body", formJson);
                        } else {
                            requestNode.put("body", bodyString);
                        }
                    } else {
                        requestNode.put("body", bodyString);
                    }

                    rootNode.set("request", requestNode);

                    try {
                        String jsonString = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(rootNode);
                        return Mono.just(jsonString);
                    } catch (Exception e) {
                        return Mono.error(e);
                    }
                });
    }
}
