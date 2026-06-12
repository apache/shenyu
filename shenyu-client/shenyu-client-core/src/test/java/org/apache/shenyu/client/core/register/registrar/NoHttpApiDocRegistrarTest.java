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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.apidocs.annotations.ApiDoc;
import org.apache.shenyu.client.apidocs.annotations.ApiModule;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

public class NoHttpApiDocRegistrarTest {

    private TestShenyuClientRegisterEventPublisher testPublisher;

    private NoHttpApiDocRegistrar noHttpApiDocRegistrar;

    private NoHttpApiDocRegistrar grpcApiDocRegistrar;

    @BeforeEach
    public void init() {
        testPublisher = new TestShenyuClientRegisterEventPublisher();
        noHttpApiDocRegistrar = new NoHttpApiDocRegistrar(testPublisher, new DubboTestClientRegisterConfig());
        grpcApiDocRegistrar = new NoHttpApiDocRegistrar(testPublisher, new GrpcTestClientRegisterConfig());
    }

    @Test
    public void testDoParse() {
        final TestApiBeanAnnotatedClassAndMethod bean = new TestApiBeanAnnotatedClassAndMethod();

        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(), "bean", bean);

        apiBean.addApiDefinition(null, null);

        AbstractApiDocRegistrar.HttpApiSpecificInfo httpApiSpecificInfo =
                noHttpApiDocRegistrar.doParse(apiBean.getApiDefinitions().get(0));

        assertThat(httpApiSpecificInfo.getApiHttpMethodEnums().get(0), is(ApiHttpMethodEnum.NOT_HTTP));

        assertThat(httpApiSpecificInfo.getConsume(), is(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE));

        assertThat(httpApiSpecificInfo.getProduce(), is(ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE));
    }

    @Test
    public void testRpcDocumentGenerationWithParameters() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(),
                DubboTestServiceImpl.class.getName(),
                DubboTestServiceImpl.class.getDeclaredConstructor().newInstance(),
                "dubboTestService");

        apiBean.addApiDefinition(DubboTestServiceImpl.class.getMethod("findById", String.class), "/findById");

        noHttpApiDocRegistrar.register(apiBean);

        ApiDocRegisterDTO dto = testPublisher.metaData;
        assertThat(dto, notNullValue());

        // Verify document contains requestParameters (ResponseType format for RPC)
        assertThat(dto.getDocument(), notNullValue());
        assertThat("Document should contain requestParameters field", dto.getDocument().contains("\"requestParameters\""), is(true));
        assertThat("Parameters should have string type", dto.getDocument().contains("\"type\":\"string\""), is(true));
    }

    @Test
    public void testRpcDocumentGenerationWithComplexParameter() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(),
                DubboTestServiceImpl.class.getName(),
                DubboTestServiceImpl.class.getDeclaredConstructor().newInstance(),
                "dubboTestService");

        apiBean.addApiDefinition(DubboTestServiceImpl.class.getMethod("insert", DubboTest.class), "/insert");

        noHttpApiDocRegistrar.register(apiBean);

        ApiDocRegisterDTO dto = testPublisher.metaData;
        assertThat(dto, notNullValue());

        // Verify document contains object-type parameter with nested refs
        assertThat(dto.getDocument(), notNullValue());
        assertThat("Document should contain requestParameters with object type",
                dto.getDocument().contains("\"type\":\"object\""), is(true));
        assertThat("Complex parameter should have nested refs",
                dto.getDocument().contains("\"refs\""), is(true));
    }

    @Test
    public void testRpcDocumentGenerationResponseType() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(),
                DubboTestServiceImpl.class.getName(),
                DubboTestServiceImpl.class.getDeclaredConstructor().newInstance(),
                "dubboTestService");

        apiBean.addApiDefinition(DubboTestServiceImpl.class.getMethod("findById", String.class), "/findById");

        noHttpApiDocRegistrar.register(apiBean);

        ApiDocRegisterDTO dto = testPublisher.metaData;
        assertThat(dto, notNullValue());

        // Verify responseParameters contains object with id and name fields
        assertThat(dto.getDocument(), notNullValue());
        assertThat("responseParameters should contain object type",
                dto.getDocument().contains("\"responseParameters\""), is(true));
    }

    @Test
    public void testGrpcDocumentGenerationWithParameters() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.GRPC.getName(),
                GrpcTestServiceImpl.class.getName(),
                GrpcTestServiceImpl.class.getDeclaredConstructor().newInstance(),
                "grpcTestService");

        apiBean.addApiDefinition(GrpcTestServiceImpl.class.getMethod("unaryCall", String.class, io.grpc.stub.StreamObserver.class), "/unaryCall");

        grpcApiDocRegistrar.register(apiBean);

        ApiDocRegisterDTO dto = testPublisher.metaData;
        assertThat(dto, notNullValue());
        assertThat(dto.getDocument(), notNullValue());
        assertThat("gRPC document should contain requestParameters field", dto.getDocument().contains("\"requestParameters\""), is(true));
        assertThat("gRPC document should contain responseParameters field", dto.getDocument().contains("\"responseParameters\""), is(true));
    }

    @Test
    public void testGrpcDocumentGenerationWithVoidReturn() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.GRPC.getName(),
                GrpcTestServiceImpl.class.getName(),
                GrpcTestServiceImpl.class.getDeclaredConstructor().newInstance(),
                "grpcTestService");

        apiBean.addApiDefinition(GrpcTestServiceImpl.class.getMethod("noStreamObserver", String.class), "/noStream");

        grpcApiDocRegistrar.register(apiBean);

        ApiDocRegisterDTO dto = testPublisher.metaData;
        assertThat(dto, notNullValue());
        assertThat(dto.getDocument(), notNullValue());
        assertThat("gRPC document should contain requestParameters field", dto.getDocument().contains("\"requestParameters\""), is(true));
        assertThat("gRPC document should contain responseParameters field", dto.getDocument().contains("\"responseParameters\""), is(true));
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

    @ApiModule("dubboTestService")
    public static class DubboTestServiceImpl {

        @ApiDoc(desc = "findById")
        public DubboTest findById(final String id) {
            return null;
        }

        @ApiDoc(desc = "insert")
        public DubboTest insert(final DubboTest dubboTest) {
            return null;
        }
    }

    @ApiModule("grpcTestService")
    public static class GrpcTestServiceImpl {

        @ApiDoc(desc = "unaryCall")
        public void unaryCall(final String request, final io.grpc.stub.StreamObserver<DubboTest> responseObserver) {
        }

        @ApiDoc(desc = "noStreamObserver")
        public String noStreamObserver(final String request) {
            return null;
        }
    }

    @ApiModule("testClass")
    static class TestApiBeanAnnotatedClassAndMethod {

        @ApiDoc(desc = "testMethod")
        public String testMethod() {
            return "";
        }
    }

    static class DubboTestClientRegisterConfig implements ClientRegisterConfig {

        @Override
        public Integer getPort() {
            return 20880;
        }

        @Override
        public String getHost() {
            return "127.0.0.1";
        }

        @Override
        public String getAppName() {
            return "test-dubbo";
        }

        @Override
        public String getContextPath() {
            return "/dubbo";
        }

        @Override
        public String getIpAndPort() {
            return "127.0.0.1:20880";
        }

        @Override
        public Boolean getAddPrefixed() {
            return false;
        }

        @Override
        public RpcTypeEnum getRpcTypeEnum() {
            return RpcTypeEnum.DUBBO;
        }
    }

    static class GrpcTestClientRegisterConfig implements ClientRegisterConfig {

        @Override
        public Integer getPort() {
            return 9090;
        }

        @Override
        public String getHost() {
            return "127.0.0.1";
        }

        @Override
        public String getAppName() {
            return "test-grpc";
        }

        @Override
        public String getContextPath() {
            return "/grpc";
        }

        @Override
        public String getIpAndPort() {
            return "127.0.0.1:9090";
        }

        @Override
        public Boolean getAddPrefixed() {
            return false;
        }

        @Override
        public RpcTypeEnum getRpcTypeEnum() {
            return RpcTypeEnum.GRPC;
        }
    }

    static class TestShenyuClientRegisterEventPublisher extends ShenyuClientRegisterEventPublisher {

        private ApiDocRegisterDTO metaData;

        @Override
        public void start(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        }

        @Override
        public void publishEvent(final DataTypeParent data) {
            this.metaData = (ApiDocRegisterDTO) data;
        }
    }
}
