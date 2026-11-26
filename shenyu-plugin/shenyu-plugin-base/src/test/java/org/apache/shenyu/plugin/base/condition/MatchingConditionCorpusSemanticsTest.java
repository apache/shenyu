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

package org.apache.shenyu.plugin.base.condition;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.plugin.api.RemoteAddressResolver;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.condition.data.ParameterDataFactory;
import org.apache.shenyu.plugin.base.condition.judge.PredicateJudgeFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpMethod;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Verifies the shared matching-condition corpus against the real gateway condition SPIs.
 */
final class MatchingConditionCorpusSemanticsTest {

    private static final String CORPUS_RESOURCE = "matching-condition/corpus-v1.json";

    private static final int IPV4_PART_COUNT = 4;

    private static final int MAX_IPV4_PART = 255;

    private static final int REMOTE_PORT = 8080;

    private GenericApplicationContext context;

    private ApplicationContext previousContext;

    private InetSocketAddress remoteAddress;

    @BeforeEach
    void setUp() {
        previousContext = SpringBeanUtils.getInstance().getApplicationContext();
        context = new GenericApplicationContext();
        context.getBeanFactory().registerSingleton("remoteAddressResolver", new RemoteAddressResolver() {
            @Override
            public InetSocketAddress resolve(final ServerWebExchange exchange) {
                return remoteAddress;
            }
        });
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void tearDown() {
        SpringBeanUtils.getInstance().setApplicationContext(previousContext);
        context.close();
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("semanticCases")
    void evaluatesCorpusCase(final String caseId, final SemanticCase semanticCase) throws UnknownHostException {
        ServerWebExchange exchange = exchange(semanticCase.request());
        ConditionData conditionData = conditionData(semanticCase);
        String realData = ParameterDataFactory.builderData(semanticCase.source(), semanticCase.paramName(), exchange);
        Boolean actual = PredicateJudgeFactory.judge(conditionData, realData);
        String message = "case=" + caseId + ", source=" + semanticCase.source()
                + ", operator=" + semanticCase.operator() + ", realData=" + realData;
        assertEquals(semanticCase.expected(), actual, message);
    }

    private ServerWebExchange exchange(final RequestFixture request) throws UnknownHostException {
        remoteAddress = new InetSocketAddress(InetAddress.getByAddress(request.remoteHost(), ipv4(request.remoteIp())), REMOTE_PORT);
        MockServerHttpRequest.BodyBuilder builder = MockServerHttpRequest.method(HttpMethod.valueOf(request.method()), request.path())
                .remoteAddress(remoteAddress);
        request.headers().forEach(builder::header);
        request.query().forEach(builder::queryParam);
        request.cookies().forEach((name, value) -> builder.cookie(new HttpCookie(name, value)));
        MockServerWebExchange exchange = MockServerWebExchange.from(builder.build());
        ShenyuContext shenyuContext = new ShenyuContext();
        request.shenyuContext().forEach((field, value) -> ReflectUtils.setFieldValue(shenyuContext, field, value));
        exchange.getAttributes().put(Constants.CONTEXT, shenyuContext);
        return exchange;
    }

    private static byte[] ipv4(final String value) {
        String[] parts = value.split("\\.", -1);
        if (parts.length != IPV4_PART_COUNT) {
            throw new IllegalArgumentException("Expected an IPv4 address in the corpus");
        }
        byte[] result = new byte[IPV4_PART_COUNT];
        for (int index = 0; index < parts.length; index++) {
            int part = Integer.parseInt(parts[index]);
            if (part < 0 || part > MAX_IPV4_PART) {
                throw new IllegalArgumentException("Expected an IPv4 address in the corpus");
            }
            result[index] = (byte) part;
        }
        return result;
    }

    private static ConditionData conditionData(final SemanticCase semanticCase) {
        ConditionData result = new ConditionData();
        result.setParamType(semanticCase.source());
        result.setOperator(semanticCase.operator());
        result.setParamName(semanticCase.paramName());
        result.setParamValue(semanticCase.paramValue());
        return result;
    }

    private static Stream<Arguments> semanticCases() {
        return loadSemanticCases().stream().map(semanticCase -> Arguments.of(semanticCase.id(), semanticCase));
    }

    private static List<SemanticCase> loadSemanticCases() {
        InputStream input = MatchingConditionCorpusSemanticsTest.class.getClassLoader().getResourceAsStream(CORPUS_RESOURCE);
        if (Objects.isNull(input)) {
            throw new IllegalStateException("Missing test resource " + CORPUS_RESOURCE);
        }
        try (Reader reader = new InputStreamReader(input, StandardCharsets.UTF_8)) {
            JsonObject corpus = GsonUtils.getGson().fromJson(reader, JsonObject.class);
            requiredString(corpus, "version", "corpus");
            requiredArray(corpus, "structural", "corpus");
            JsonArray semantics = requiredArray(corpus, "semantics", "corpus");
            return semantics.asList().stream().map(JsonElement::getAsJsonObject).map(MatchingConditionCorpusSemanticsTest::semanticCase).toList();
        } catch (IOException ex) {
            throw new UncheckedIOException("Cannot read " + CORPUS_RESOURCE, ex);
        }
    }

    private static SemanticCase semanticCase(final JsonObject json) {
        String id = requiredString(json, "id", "semantic case");
        JsonObject request = requiredObject(json, "request", id);
        return new SemanticCase(id, requiredString(json, "source", id), requiredString(json, "operator", id),
                requiredString(json, "paramName", id), requiredString(json, "paramValue", id), requestFixture(request, id),
                requiredBoolean(json, "expected", id));
    }

    private static RequestFixture requestFixture(final JsonObject json, final String owner) {
        return new RequestFixture(requiredString(json, "path", owner), requiredString(json, "method", owner),
                stringMap(json, "headers", owner), stringMap(json, "query", owner), stringMap(json, "cookies", owner),
                requiredString(json, "remoteHost", owner), requiredString(json, "remoteIp", owner),
                stringMap(json, "context", owner));
    }

    private static Map<String, String> stringMap(final JsonObject json, final String name, final String owner) {
        JsonObject values = requiredObject(json, name, owner);
        Map<String, String> result = new LinkedHashMap<>();
        values.entrySet().forEach(entry -> result.put(entry.getKey(), stringValue(entry.getValue(), name, owner)));
        return Map.copyOf(result);
    }

    private static String stringValue(final JsonElement value, final String name, final String owner) {
        if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()) {
            throw new IllegalStateException("Field " + owner + "." + name + " must contain strings");
        }
        return value.getAsString();
    }

    private static String requiredString(final JsonObject json, final String name, final String owner) {
        return stringValue(required(json, name, owner), name, owner);
    }

    private static boolean requiredBoolean(final JsonObject json, final String name, final String owner) {
        JsonElement value = required(json, name, owner);
        if (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isBoolean()) {
            throw new IllegalStateException("Field " + owner + "." + name + " must be a boolean");
        }
        return value.getAsBoolean();
    }

    private static JsonObject requiredObject(final JsonObject json, final String name, final String owner) {
        JsonElement value = required(json, name, owner);
        if (!value.isJsonObject()) {
            throw new IllegalStateException("Field " + owner + "." + name + " must be an object");
        }
        return value.getAsJsonObject();
    }

    private static JsonArray requiredArray(final JsonObject json, final String name, final String owner) {
        JsonElement value = required(json, name, owner);
        if (!value.isJsonArray()) {
            throw new IllegalStateException("Field " + owner + "." + name + " must be an array");
        }
        return value.getAsJsonArray();
    }

    private static JsonElement required(final JsonObject json, final String name, final String owner) {
        if (Objects.isNull(json) || !json.has(name) || json.get(name).isJsonNull()) {
            throw new IllegalStateException("Missing field " + owner + "." + name);
        }
        return json.get(name);
    }

    private record SemanticCase(String id, String source, String operator, String paramName, String paramValue,
                                RequestFixture request, boolean expected) {
    }

    private record RequestFixture(String path, String method, Map<String, String> headers, Map<String, String> query,
                                  Map<String, String> cookies, String remoteHost, String remoteIp,
                                  Map<String, String> shenyuContext) {
    }
}
