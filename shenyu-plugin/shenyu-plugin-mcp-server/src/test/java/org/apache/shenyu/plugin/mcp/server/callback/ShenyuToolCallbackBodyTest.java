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

package org.apache.shenyu.plugin.mcp.server.callback;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.mcp.server.definition.ShenyuToolDefinition;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.MediaType;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.http.codec.multipart.FormFieldPart;
import org.springframework.http.codec.multipart.Part;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerRequest;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.springframework.web.reactive.function.server.RequestPredicates.POST;

/**
 * Exercises documented tool configurations against local WebFlux downstream handlers.
 */
class ShenyuToolCallbackBodyTest {

    @ParameterizedTest
    @ValueSource(strings = {"MCP_TOOL_EXAMPLES.md", "MCP_TOOL_EXAMPLES_EN.md"})
    void testDocumentedFormExample(final String document) throws IOException {
        final ToolExample example = readExample(document, "postHi");
        assertEquals("Ada & Bob", execute(example));
    }

    @ParameterizedTest
    @ValueSource(strings = {"MCP_TOOL_EXAMPLES.md", "MCP_TOOL_EXAMPLES_EN.md"})
    void testDocumentedSingleFileExample(final String document) throws IOException {
        final ToolExample example = readExample(document, "uploadSingleFile");
        assertEquals("hello.txt", execute(example));
    }

    @ParameterizedTest
    @ValueSource(strings = {"MCP_TOOL_EXAMPLES.md", "MCP_TOOL_EXAMPLES_EN.md"})
    void testDocumentedMultipleFilesExample(final String document) throws IOException {
        final ToolExample example = readExample(document, "uploadMultipleFiles");
        assertEquals("hello.txt,empty.bin", execute(example));
    }

    @ParameterizedTest
    @ValueSource(strings = {"application/x-www-form-urlencoded", "multipart/form-data"})
    void testNonJsonBodiesRetainJsonLookingStrings(final String contentType) {
        final String config = "{\"requestTemplate\":{\"url\":\"/shenyu/client/post/hi\",\"method\":\"POST\","
                + "\"headers\":[{\"key\":\"content-type\",\"value\":\"" + contentType + "\"}],"
                + "\"argsToJsonBody\":true},\"argsPosition\":{\"name\":\"body\"}}";
        for (String value : List.of("{}", "[1,2]")) {
            final JsonObject input = new JsonObject();
            input.addProperty("name", value);
            assertEquals(value, execute(new ToolExample(config, input.toString())));
        }
    }

    @Test
    void testJsonStillParsesStringifiedValues() {
        final JsonObject input = new JsonObject();
        input.addProperty("name", "{\"enabled\":true}");
        final String config = "{\"requestTemplate\":{\"url\":\"/json\",\"method\":\"POST\",\"argsToJsonBody\":true},"
                + "\"argsPosition\":{\"name\":\"body\"}}";

        final JsonObject response = JsonParser.parseString(execute(new ToolExample(config, input.toString()))).getAsJsonObject();

        assertTrue(response.getAsJsonObject("name").get("enabled").getAsBoolean());
    }

    @ParameterizedTest
    @ValueSource(strings = {"application/json", "application/vnd.example+json", "application/x-www-form-urlencoded"})
    void testEmptyToolBodyDoesNotForwardMcpEnvelope(final String contentType) {
        final MockServerWebExchange origin = MockServerWebExchange.from(MockServerHttpRequest.post("http://gateway.example/mcp")
                .contentType(MediaType.APPLICATION_JSON).body("{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\"}"));
        final ShenyuPluginChain chain = exchange -> DataBufferUtils.join(exchange.getRequest().getBody())
                .map(buffer -> {
                    try {
                        final byte[] bytes = new byte[buffer.readableByteCount()];
                        buffer.read(bytes);
                        return new String(bytes, StandardCharsets.UTF_8);
                    } finally {
                        DataBufferUtils.release(buffer);
                    }
                }).defaultIfEmpty("").flatMap(body -> {
                    assertEquals("application/x-www-form-urlencoded".equals(contentType) ? "" : "{}", body);
                    return exchange.getResponse().setComplete();
                });
        final String config = "{\"requestTemplate\":{\"url\":\"/empty\",\"method\":\"POST\","
                + "\"headers\":[{\"key\":\"Content-Type\",\"value\":\"" + contentType + "\"}]},\"argsPosition\":{}}";
        final ShenyuToolCallback callback = new ShenyuToolCallback(mock(ShenyuToolDefinition.class));

        assertEquals("", callback.executeToolCall(origin, chain, "body-test", config, "{}", 5));
    }

    @ParameterizedTest
    @ValueSource(strings = {"text/plain", "application/xml", "application/graphql", "application/vnd.example.custom"})
    void testOtherConcreteMediaTypesRemainReachableFromToolTemplates(final String contentType) {
        final MockServerWebExchange origin = MockServerWebExchange.from(MockServerHttpRequest.post("http://gateway.example/mcp")
                .contentType(MediaType.APPLICATION_JSON).body("{\"jsonrpc\":\"2.0\"}"));
        final JsonObject input = new JsonObject();
        input.addProperty("name", "Ada & 中文");
        final ShenyuPluginChain chain = exchange -> {
            assertEquals(MediaType.parseMediaType(contentType), exchange.getRequest().getHeaders().getContentType());
            return DataBufferUtils.join(exchange.getRequest().getBody()).flatMap(buffer -> {
                try {
                    final byte[] bytes = new byte[buffer.readableByteCount()];
                    buffer.read(bytes);
                    assertEquals(input.toString(), new String(bytes, StandardCharsets.UTF_8));
                    return exchange.getResponse().setComplete();
                } finally {
                    DataBufferUtils.release(buffer);
                }
            });
        };
        final String config = "{\"requestTemplate\":{\"url\":\"/raw\",\"method\":\"POST\","
                + "\"headers\":[{\"key\":\"Content-Type\",\"value\":\"" + contentType + "\"}],\"argsToJsonBody\":true},"
                + "\"argsPosition\":{\"name\":\"body\"}}";
        final ShenyuToolCallback callback = new ShenyuToolCallback(mock(ShenyuToolDefinition.class));

        assertEquals("", callback.executeToolCall(origin, chain, "raw-body-test", config, input.toString(), 5));
    }

    private String execute(final ToolExample example) {
        final WebTestClient client = WebTestClient.bindToRouterFunction(RouterFunctions
                .route(POST("/shenyu/client/post/hi"), this::echoName)
                .andRoute(POST("/json"), request -> request.bodyToMono(String.class).flatMap(body -> ServerResponse.ok().bodyValue(body)))
                .andRoute(POST("/upload/webFluxSingle"), request -> upload(request, "file"))
                .andRoute(POST("/upload/webFluxFiles"), request -> upload(request, "files"))).build();
        final ShenyuPluginChain chain = exchange -> Mono.defer(() -> {
            final byte[] response = client.method(exchange.getRequest().getMethod())
                    .uri(exchange.getRequest().getURI().getRawPath())
                    .headers(headers -> headers.addAll(exchange.getRequest().getHeaders()))
                    .body(BodyInserters.fromDataBuffers(exchange.getRequest().getBody()))
                    .exchange().expectStatus().isOk().expectBody().returnResult().getResponseBody();
            assertNotNull(response);
            return exchange.getResponse().writeWith(Mono.just(exchange.getResponse().bufferFactory().wrap(response)));
        });
        final MockServerWebExchange origin = MockServerWebExchange.from(MockServerHttpRequest.post("http://gateway.example/mcp")
                .contentType(MediaType.APPLICATION_JSON).body("{\"jsonrpc\":\"2.0\"}"));
        final ShenyuToolCallback callback = new ShenyuToolCallback(mock(ShenyuToolDefinition.class));
        return callback.executeToolCall(origin, chain, "body-test", example.config(), example.input(), 5);
    }

    private Mono<ServerResponse> echoName(final ServerRequest request) {
        if (request.headers().contentType().map(MediaType.MULTIPART_FORM_DATA::isCompatibleWith).orElse(false)) {
            return request.multipartData().flatMap(parts -> {
                final FormFieldPart part = (FormFieldPart) parts.getFirst("name");
                assertNotNull(part);
                return ServerResponse.ok().bodyValue(part.value());
            });
        }
        return request.formData().flatMap(fields -> ServerResponse.ok().bodyValue(fields.getFirst("name")));
    }

    private Mono<ServerResponse> upload(final ServerRequest request, final String name) {
        return request.multipartData().flatMap(parts -> {
            final List<Part> files = parts.get(name);
            assertNotNull(files);
            return Flux.fromIterable(files).cast(FilePart.class).concatMap(file -> DataBufferUtils.join(file.content()).map(buffer -> {
                try {
                    final byte[] bytes = new byte[buffer.readableByteCount()];
                    buffer.read(bytes);
                    return bytes;
                } finally {
                    DataBufferUtils.release(buffer);
                }
            }).defaultIfEmpty(new byte[0]).flatMap(bytes -> {
                final boolean empty = "empty.bin".equals(file.filename());
                assertEquals(empty ? MediaType.APPLICATION_OCTET_STREAM : MediaType.TEXT_PLAIN, file.headers().getContentType());
                assertArrayEquals(empty ? new byte[0] : "hello".getBytes(StandardCharsets.UTF_8), bytes);
                return file.delete().thenReturn(file.filename());
            })).collectList().flatMap(names -> ServerResponse.ok().bodyValue(String.join(",", names)));
        });
    }

    private ToolExample readExample(final String document, final String name) throws IOException {
        final Matcher blocks = Pattern.compile("```json\\R(.*?)\\R```", Pattern.DOTALL)
                .matcher(Files.readString(Path.of(document)));
        while (blocks.find()) {
            final JsonObject block = JsonParser.parseString(blocks.group(1)).getAsJsonObject();
            if (block.has("name") && name.equals(block.get("name").getAsString())) {
                final String config = block.get("requestConfig").getAsString();
                assertTrue(blocks.find(), "Documented tool must include call arguments");
                return new ToolExample(config, blocks.group(1));
            }
        }
        throw new IllegalArgumentException("Missing documented tool " + name);
    }

    private record ToolExample(String config, String input) {
    }
}
