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

package org.apache.shenyu.client.core.utils;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.protobuf.Empty;
import org.apache.shenyu.client.core.test.Test.Address;
import org.apache.shenyu.client.core.test.Test.TestRequest;
import org.apache.shenyu.client.core.utils.OpenApiUtils.Parameter;
import org.apache.shenyu.client.core.utils.OpenApiUtils.ResponseType;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.junit.jupiter.api.Test;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

public class OpenApiUtilsTest {

    @Test
    void testGenerateRpcDocumentResponse() throws Exception {
        Method method = DubboTestService.class.getMethod("findById", String.class);
        Map<String, Object> response = OpenApiUtils.generateRpcDocumentResponse("/dubbo/findById", method);

        assertThat(response.containsKey("200"), is(true));
        assertThat(response.containsKey("404"), is(true));
        assertThat(response.containsKey("409"), is(false));
    }

    @Test
    void testGenerateRpcDocumentResponseVoidReturn() throws Exception {
        Method method = RpcComplexParamService.class.getMethod("deleteById", String.class);
        Map<String, Object> response = OpenApiUtils.generateRpcDocumentResponse("/rpc/deleteById", method);
        assertThat(response.containsKey("200"), is(true));
        assertThat(response.containsKey("404"), is(true));
        assertThat(response.containsKey("409"), is(false));
        Map<String, Object> successResponse = (Map<String, Object>) response.get("200");
        Map<String, Object> content = (Map<String, Object>) successResponse.get("content");
        Map<String, Object> schema = (Map<String, Object>) ((Map<String, Object>) content.get("*/*")).get("schema");
        assertThat(schema.get("type"), is("void"));
    }

    @Test
    void testParseReturnTypeObject() throws Exception {
        Method method = DubboTestService.class.getMethod("findById", String.class);
        ResponseType returnType = OpenApiUtils.parseReturnType(method);

        assertThat(returnType.getType(), is("object"));
        assertThat(returnType.getRefs(), notNullValue());
        assertThat(returnType.getRefs(), hasSize(2));
        assertThat(returnType.getRefs().get(0).getName(), is("id"));
        assertThat(returnType.getRefs().get(0).getType(), is("string"));
        assertThat(returnType.getRefs().get(1).getName(), is("name"));
        assertThat(returnType.getRefs().get(1).getType(), is("string"));
    }

    @Test
    void testParseReturnTypeList() throws Exception {
        Method method = DubboTestService.class.getMethod("findAll");
        ResponseType returnType = OpenApiUtils.parseReturnType(method);

        assertThat(returnType.getType(), is("array"));
        assertThat(returnType.getRefs(), notNullValue());
        assertThat(returnType.getRefs(), hasSize(1));
        assertThat(returnType.getRefs().get(0).getType(), is("object"));
    }

    @Test
    void testIsProtobufMessageNegative() {
        assertThat(OpenApiUtils.isProtobufMessage(DubboTest.class), is(false));
        assertThat(OpenApiUtils.isProtobufMessage(String.class), is(false));
        assertThat(OpenApiUtils.isProtobufMessage(null), is(false));
        assertThat(OpenApiUtils.isProtobufMessage(int.class), is(false));
    }

    @Test
    void testIsProtobufMessagePositive() {
        assertThat(OpenApiUtils.isProtobufMessage(TestRequest.class), is(true));
        assertThat(OpenApiUtils.isProtobufMessage(Address.class), is(true));
        assertThat(OpenApiUtils.isProtobufMessage(Empty.class), is(true));
    }

    @Test
    void testParseReturnTypeProtobufWithAllFieldTypes() throws Exception {
        Method method = ProtobufTestService.class.getMethod("getTestRequest");
        ResponseType returnType = OpenApiUtils.parseReturnType(method);
        assertThat(returnType.getType(), is("object"));
        assertThat(returnType.getRefs(), notNullValue());
        assertThat(returnType.getRefs(), hasSize(7));

        assertThat(returnType.getRefs().get(0).getName(), is("id"));
        assertThat(returnType.getRefs().get(0).getType(), is("string"));

        assertThat(returnType.getRefs().get(1).getName(), is("count"));
        assertThat(returnType.getRefs().get(1).getType(), is("integer"));

        assertThat(returnType.getRefs().get(2).getName(), is("enabled"));
        assertThat(returnType.getRefs().get(2).getType(), is("boolean"));

        assertThat(returnType.getRefs().get(3).getName(), is("status"));
        assertThat(returnType.getRefs().get(3).getType(), is("string"));

        assertThat(returnType.getRefs().get(4).getName(), is("address"));
        assertThat(returnType.getRefs().get(4).getType(), is("object"));
        assertThat(returnType.getRefs().get(4).getRefs(), notNullValue());
        assertThat(returnType.getRefs().get(4).getRefs(), hasSize(2));
        assertThat(returnType.getRefs().get(4).getRefs().get(0).getName(), is("street"));
        assertThat(returnType.getRefs().get(4).getRefs().get(0).getType(), is("string"));
        assertThat(returnType.getRefs().get(4).getRefs().get(1).getName(), is("city"));
        assertThat(returnType.getRefs().get(4).getRefs().get(1).getType(), is("string"));

        assertThat(returnType.getRefs().get(5).getName(), is("tags"));
        assertThat(returnType.getRefs().get(5).getType(), is("array"));
        assertThat(returnType.getRefs().get(5).getRefs(), notNullValue());
        assertThat(returnType.getRefs().get(5).getRefs(), hasSize(1));
        assertThat(returnType.getRefs().get(5).getRefs().get(0).getType(), is("string"));

        assertThat(returnType.getRefs().get(6).getName(), is("addresses"));
        assertThat(returnType.getRefs().get(6).getType(), is("array"));
        assertThat(returnType.getRefs().get(6).getRefs(), notNullValue());
        assertThat(returnType.getRefs().get(6).getRefs(), hasSize(1));
        assertThat(returnType.getRefs().get(6).getRefs().get(0).getType(), is("object"));
    }

    @Test
    void testParseReturnTypeProtobufEmpty() throws Exception {
        Method method = ProtobufTestService.class.getMethod("getEmpty");
        ResponseType returnType = OpenApiUtils.parseReturnType(method);
        assertThat(returnType.getType(), is("object"));
        assertThat(returnType.getRefs(), nullValue());
    }

    @Test
    void testGenerateRpcRequestDocParametersProtobuf() throws Exception {
        Method method = ProtobufTestService.class.getMethod("sendTestRequest", TestRequest.class);
        List<Parameter> params = OpenApiUtils.generateRpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("request"));
        assertThat(params.get(0).getType(), is("object"));
        assertThat(params.get(0).getRefs(), notNullValue());
        assertThat(params.get(0).getRefs(), hasSize(7));
        assertThat(params.get(0).getRefs().get(0).getName(), is("id"));
        assertThat(params.get(0).getRefs().get(0).getType(), is("string"));
        assertThat(params.get(0).getRefs().get(3).getName(), is("status"));
        assertThat(params.get(0).getRefs().get(3).getType(), is("string"));
        assertThat(params.get(0).getRefs().get(5).getName(), is("tags"));
        assertThat(params.get(0).getRefs().get(5).getType(), is("array"));
    }

    @Test
    void testGenerateDocumentResponseExistingBehavior() {
        Map<String, Object> response = OpenApiUtils.generateDocumentResponse("/test/path");
        assertThat(response.containsKey("200"), is(true));
        assertThat(response.containsKey("404"), is(true));
        assertThat(response.containsKey("409"), is(true));
    }

    @Test
    void testGenerateRequestDocParametersWithRequestParam() throws Exception {
        Method method = SpringMvcController.class.getMethod("query", String.class);
        List<Parameter> params = OpenApiUtils.generateRequestDocParameters("/test/query", method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("name"));
        assertThat(params.get(0).getType(), is("string"));
        assertThat(params.get(0).isRequired(), is(true));
    }

    @Test
    void testGenerateRequestDocParametersWithPathVariable() throws Exception {
        Method method = SpringMvcController.class.getMethod("getByPath", String.class);
        List<Parameter> params = OpenApiUtils.generateRequestDocParameters("/test/{id}", method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("id"));
        assertThat(params.get(0).getType(), is("string"));
        assertThat(params.get(0).isRequired(), is(true));
    }

    @Test
    void testGenerateRequestDocParametersWithQueryAndPath() throws Exception {
        Method method = SpringMvcController.class.getMethod("getByPathWithQuery", String.class, String.class);
        List<Parameter> params = OpenApiUtils.generateRequestDocParameters("/test/{id}/detail", method);
        assertThat(params, hasSize(2));
        assertThat(params.get(0).getName(), is("name"));
        assertThat(params.get(0).getIn(), is("query"));
        assertThat(params.get(1).getName(), is("id"));
        assertThat(params.get(1).getIn(), is("path"));
    }

    @Test
    void testGenerateRequestDocParametersNoAnnotations() throws Exception {
        Method method = DubboTestService.class.getMethod("findById", String.class);
        List<Parameter> params = OpenApiUtils.generateRequestDocParameters("/dubbo/findById", method);
        assertThat(params, hasSize(0));
    }

    @Test
    void testGenerateRpcRequestDocParametersSimpleType() throws Exception {
        Method method = DubboTestService.class.getMethod("findById", String.class);
        List<Parameter> params = OpenApiUtils.generateRpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("id"));
        assertThat(params.get(0).getType(), is("string"));
        assertThat(params.get(0).isRequired(), is(true));
    }

    @Test
    void testGenerateRpcRequestDocParametersComplexType() throws Exception {
        Method method = DubboTestService.class.getMethod("insert", DubboTest.class);
        List<Parameter> params = OpenApiUtils.generateRpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getType(), is("object"));
        assertThat(params.get(0).getRefs(), notNullValue());
        assertThat(params.get(0).getRefs(), hasSize(2));
        assertThat(params.get(0).isRequired(), is(true));
    }

    @Test
    void testGenerateRpcRequestDocParametersWithListParameter() throws Exception {
        Method method = RpcComplexParamService.class.getMethod("batchInsert", List.class);
        List<Parameter> params = OpenApiUtils.generateRpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("ids"));
        assertThat(params.get(0).getType(), is("array"));
    }

    @Test
    void testGenerateRpcRequestDocParametersWithMapParameter() throws Exception {
        Method method = RpcComplexParamService.class.getMethod("searchByMap", Map.class);
        List<Parameter> params = OpenApiUtils.generateRpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("params"));
        assertThat(params.get(0).getType(), is("object"));
    }

    @Test
    void testUseSpringMvcParamParsing() {
        assertThat(OpenApiUtils.useSpringMvcParamParsing(RpcTypeEnum.HTTP), is(true));
        assertThat(OpenApiUtils.useSpringMvcParamParsing(RpcTypeEnum.WEB_SOCKET), is(true));
        assertThat(OpenApiUtils.useSpringMvcParamParsing(RpcTypeEnum.SPRING_CLOUD), is(true));
        assertThat(OpenApiUtils.useSpringMvcParamParsing(RpcTypeEnum.DUBBO), is(false));
        assertThat(OpenApiUtils.useSpringMvcParamParsing(RpcTypeEnum.GRPC), is(false));
        assertThat(OpenApiUtils.useSpringMvcParamParsing(RpcTypeEnum.SOFA), is(false));
    }

    // --- gRPC tests ---

    @Test
    void testGenerateGrpcRequestDocParametersSimpleType() throws Exception {
        Method method = GrpcTestService.class.getMethod("unaryCall", String.class, io.grpc.stub.StreamObserver.class);
        List<Parameter> params = OpenApiUtils.generateGrpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("request"));
        assertThat(params.get(0).getType(), is("string"));
        assertThat(params.get(0).isRequired(), is(true));
    }

    @Test
    void testGenerateGrpcRequestDocParametersComplexType() throws Exception {
        Method method = GrpcTestService.class.getMethod("unaryCallComplex", GrpcTestClass.class, io.grpc.stub.StreamObserver.class);
        List<Parameter> params = OpenApiUtils.generateGrpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("request"));
        assertThat(params.get(0).getType(), is("object"));
        assertThat(params.get(0).getRefs(), notNullValue());
        assertThat(params.get(0).isRequired(), is(true));
    }

    @Test
    void testGenerateGrpcRequestDocParametersClientStreaming() throws Exception {
        Method method = GrpcClientStreamingService.class.getMethod("clientStreaming", io.grpc.stub.StreamObserver.class);
        List<Parameter> params = OpenApiUtils.generateGrpcRequestDocParameters(method);
        assertThat(params, hasSize(1));
        assertThat(params.get(0).getName(), is("request"));
        assertThat(params.get(0).getType(), is("string"));
        assertThat(params.get(0).isRequired(), is(true));
    }

    @Test
    void testParseGrpcReturnTypeWithStreamObserver() throws Exception {
        Method method = GrpcTestService.class.getMethod("unaryCall", String.class, io.grpc.stub.StreamObserver.class);
        ResponseType returnType = OpenApiUtils.parseGrpcReturnType(method);
        assertThat(returnType.getName(), is("ROOT"));
        assertThat(returnType.getType(), is("object"));
        assertThat(returnType.getRefs(), notNullValue());
        assertThat(returnType.getRefs(), hasSize(2));
    }

    @Test
    void testParseGrpcReturnTypeVoid() throws Exception {
        Method method = GrpcTestService.class.getMethod("noStreamObserver", String.class);
        ResponseType returnType = OpenApiUtils.parseGrpcReturnType(method);
        assertThat(returnType.getName(), is("ROOT"));
        assertThat(returnType.getType(), is("void"));
    }

    @Test
    void testParseGrpcReturnTypeClientStreaming() throws Exception {
        Method method = GrpcClientStreamingService.class.getMethod("clientStreaming", io.grpc.stub.StreamObserver.class);
        ResponseType returnType = OpenApiUtils.parseGrpcReturnType(method);
        assertThat(returnType.getName(), is("ROOT"));
        assertThat(returnType.getType(), is("object"));
        assertThat(returnType.getRefs(), notNullValue());
        assertThat(returnType.getRefs(), hasSize(2));
    }

    @Test
    void testGenerateGrpcDocumentResponseWithStreamObserver() throws Exception {
        Method method = GrpcTestService.class.getMethod("unaryCall", String.class, io.grpc.stub.StreamObserver.class);
        Map<String, Object> response = OpenApiUtils.generateGrpcDocumentResponse("/grpc/unaryCall", method);
        assertThat(response.containsKey("200"), is(true));
        assertThat(response.containsKey("404"), is(true));
        assertThat(response.containsKey("409"), is(false));
    }

    @Test
    void testGenerateGrpcDocumentResponseVoid() throws Exception {
        Method method = GrpcTestService.class.getMethod("noStreamObserver", String.class);
        Map<String, Object> response = OpenApiUtils.generateGrpcDocumentResponse("/grpc/noStream", method);
        assertThat(response.containsKey("200"), is(true));
        assertThat(response.containsKey("404"), is(true));
    }

    // --- buildDocumentJson dispatch tests ---

    @Test
    void testBuildDocumentJsonHttp() throws Exception {
        Method method = SpringMvcController.class.getMethod("query", String.class);
        String json = OpenApiUtils.buildDocumentJson(Arrays.asList("tag1"), "/test/query", method, RpcTypeEnum.HTTP);
        JsonObject doc = JsonParser.parseString(json).getAsJsonObject();
        assertThat(doc.has("requestParameters"), is(true));
        assertThat(doc.has("responseParameters"), is(true));
        assertThat(doc.has("parameters"), is(false));
        assertThat(doc.has("responseType"), is(false));
        JsonObject responses = doc.getAsJsonObject("responses");
        assertThat(responses.has("200"), is(true));
        assertThat(responses.has("404"), is(true));
        assertThat(responses.has("409"), is(true));
    }

    @Test
    void testBuildDocumentJsonGrpc() throws Exception {
        Method method = GrpcTestService.class.getMethod("unaryCall", String.class, io.grpc.stub.StreamObserver.class);
        String json = OpenApiUtils.buildDocumentJson(Arrays.asList("tag1"), "/grpc/unaryCall", method, RpcTypeEnum.GRPC);
        JsonObject doc = JsonParser.parseString(json).getAsJsonObject();
        assertThat(doc.has("requestParameters"), is(true));
        assertThat(doc.has("responseParameters"), is(true));
        assertThat(doc.has("parameters"), is(false));
        assertThat(doc.has("responseType"), is(false));
        JsonObject responses = doc.getAsJsonObject("responses");
        assertThat(responses.has("200"), is(true));
        assertThat(responses.has("404"), is(true));
        assertThat(responses.has("409"), is(false));
    }

    @Test
    void testBuildDocumentJsonRpc() throws Exception {
        Method method = DubboTestService.class.getMethod("findById", String.class);
        String json = OpenApiUtils.buildDocumentJson(Arrays.asList("tag1"), "/dubbo/findById", method, RpcTypeEnum.DUBBO);
        JsonObject doc = JsonParser.parseString(json).getAsJsonObject();
        assertThat(doc.has("requestParameters"), is(true));
        assertThat(doc.has("responseParameters"), is(true));
        assertThat(doc.has("parameters"), is(false));
        assertThat(doc.has("responseType"), is(false));
        JsonObject responses = doc.getAsJsonObject("responses");
        assertThat(responses.has("200"), is(true));
        assertThat(responses.has("404"), is(true));
        assertThat(responses.has("409"), is(false));
    }

    // --- Inner types (must be after all methods per checkstyle InnerTypeLast) ---

    public static class DubboTest {

        private String id;

        private String name;

        public DubboTest() {
        }

        public String getId() {
            return id;
        }

        public String getName() {
            return name;
        }
    }

    public static class DubboTestService {

        public DubboTest findById(final String id) {
            return null;
        }

        public DubboTest insert(final DubboTest dubboTest) {
            return null;
        }

        public List<DubboTest> findAll() {
            return null;
        }
    }

    @RestController
    @RequestMapping("/test")
    public static class SpringMvcController {

        @RequestMapping("/query")
        public String query(@RequestParam("name") final String name) {
            return "";
        }

        @RequestMapping("/{id}")
        public String getByPath(@PathVariable("id") final String id) {
            return "";
        }

        @RequestMapping("/{id}/detail")
        public String getByPathWithQuery(@PathVariable("id") final String id, @RequestParam("name") final String name) {
            return "";
        }
    }

    public static class GrpcTestClass {

        private String id;

        private String name;

        public GrpcTestClass() {
        }

        public String getId() {
            return id;
        }

        public String getName() {
            return name;
        }
    }

    public static class GrpcTestService {

        public void unaryCall(final String request, final io.grpc.stub.StreamObserver<GrpcTestClass> responseObserver) {
        }

        public void unaryCallComplex(final GrpcTestClass request, final io.grpc.stub.StreamObserver<GrpcTestClass> responseObserver) {
        }

        public String noStreamObserver(final String request) {
            return null;
        }
    }

    public static class GrpcClientStreamingService {

        public io.grpc.stub.StreamObserver<String> clientStreaming(
                final io.grpc.stub.StreamObserver<GrpcTestClass> responseObserver) {
            return null;
        }
    }

    public static class ProtobufTestService {

        public TestRequest getTestRequest() {
            return null;
        }

        public TestRequest sendTestRequest(final TestRequest request) {
            return null;
        }

        public Empty getEmpty() {
            return null;
        }
    }

    public static class RpcComplexParamService {

        public String batchInsert(final List<String> ids) {
            return null;
        }

        public String searchByMap(final Map<String, String> params) {
            return null;
        }

        public void deleteById(final String id) {
        }
    }
}
