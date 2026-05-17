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

import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class ApiDocRegistrarImplTest {

    private ApiDocRegistrarImpl dubboRegistrar;

    private ApiDocRegistrarImpl httpRegistrar;

    private ApiDocRegistrarImpl grpcRegistrar;

    @BeforeEach
    public void init() {
        dubboRegistrar = new ApiDocRegistrarImpl(new DubboClientRegisterConfig());
        httpRegistrar = new ApiDocRegistrarImpl(new HttpClientRegisterConfig());
        grpcRegistrar = new ApiDocRegistrarImpl(new GrpcClientRegisterConfig());
    }

    @Test
    void testGetDocumentForRpcType() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(),
                TestDubboService.class.getName(),
                TestDubboService.class.getDeclaredConstructor().newInstance(),
                "dubboTestService");

        apiBean.addApiDefinition(TestDubboService.class.getMethod("findById", String.class), "/findById");

        String document = invokeGetDocument(dubboRegistrar, apiBean.getApiDefinitions().get(0));

        // RPC document should use requestParameters/responseParameters (not parameters/responseType)
        Map<String, Object> docMap = GsonUtils.getInstance().toObjectMap(document);
        assertThat("RPC document should contain requestParameters", docMap.containsKey("requestParameters"), is(true));
        assertThat("RPC document should contain responseParameters", docMap.containsKey("responseParameters"), is(true));
        assertThat("RPC document should NOT contain parameters (old field)", docMap.containsKey("parameters"), is(false));
        assertThat("RPC document should NOT contain responseType (old field)", docMap.containsKey("responseType"), is(false));
    }

    @Test
    void testGetDocumentForHttpType() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.HTTP.getName(),
                TestHttpService.class.getName(),
                TestHttpService.class.getDeclaredConstructor().newInstance(),
                "httpTestService");

        apiBean.addApiDefinition(TestHttpService.class.getMethod("findById", String.class), "/findById");

        String document = invokeGetDocument(httpRegistrar, apiBean.getApiDefinitions().get(0));

        // HTTP document should also use requestParameters/responseParameters
        Map<String, Object> docMap = GsonUtils.getInstance().toObjectMap(document);
        assertThat("HTTP document should contain requestParameters", docMap.containsKey("requestParameters"), is(true));
        assertThat("HTTP document should contain responseParameters", docMap.containsKey("responseParameters"), is(true));
        assertThat("HTTP document should NOT contain parameters (old field)", docMap.containsKey("parameters"), is(false));
        assertThat("HTTP document should NOT contain responseType (old field)", docMap.containsKey("responseType"), is(false));
    }

    @Test
    void testGetDocumentForGrpcType() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.GRPC.getName(),
                TestGrpcService.class.getName(),
                TestGrpcService.class.getDeclaredConstructor().newInstance(),
                "grpcTestService");

        apiBean.addApiDefinition(TestGrpcService.class.getMethod("findById", String.class), "/findById");

        String document = invokeGetDocument(grpcRegistrar, apiBean.getApiDefinitions().get(0));

        // gRPC document should use requestParameters/responseParameters
        Map<String, Object> docMap = GsonUtils.getInstance().toObjectMap(document);
        assertThat("gRPC document should contain requestParameters", docMap.containsKey("requestParameters"), is(true));
        assertThat("gRPC document should contain responseParameters", docMap.containsKey("responseParameters"), is(true));
        assertThat("gRPC document should NOT contain parameters (old field)", docMap.containsKey("parameters"), is(false));
        assertThat("gRPC document should NOT contain responseType (old field)", docMap.containsKey("responseType"), is(false));
    }

    @Test
    void testGetDocumentWithCustomDocument() throws Exception {
        ApiBean apiBean = new ApiBean(RpcTypeEnum.DUBBO.getName(),
                TestDubboService.class.getName(),
                TestDubboService.class.getDeclaredConstructor().newInstance(),
                "dubboTestService");

        apiBean.addApiDefinition(TestDubboService.class.getMethod("findById", String.class), "/findById");

        // Set custom document via properties
        String customDoc = "{\"tags\":[\"custom\"],\"operationId\":\"/custom\"}";
        apiBean.getApiDefinitions().get(0).addProperties("document", customDoc);

        String document = invokeGetDocument(dubboRegistrar, apiBean.getApiDefinitions().get(0));

        // Should return the custom document as-is
        Map<String, Object> docMap = GsonUtils.getInstance().toObjectMap(document);
        assertThat("Should use custom document", docMap.containsKey("operationId"), is(true));
        assertThat(docMap.get("operationId"), is("/custom"));
    }

    @SuppressWarnings("unchecked")
    private String invokeGetDocument(final ApiDocRegistrarImpl registrar, final ApiBean.ApiDefinition api) throws Exception {
        Method getDocumentMethod = ApiDocRegistrarImpl.class.getDeclaredMethod("getDocument", ApiBean.ApiDefinition.class);
        getDocumentMethod.setAccessible(true);
        return (String) getDocumentMethod.invoke(registrar, api);
    }

    // --- Inner types (must be after all methods per checkstyle InnerTypeLast) ---

    public static class TestDubboService {
        public Object findById(final String id) {
            return null;
        }
    }

    public static class TestHttpService {
        public Object findById(final String id) {
            return null;
        }
    }

    public static class TestGrpcService {
        public Object findById(final String id) {
            return null;
        }
    }

    static class DubboClientRegisterConfig implements ClientRegisterConfig {
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

    static class HttpClientRegisterConfig implements ClientRegisterConfig {
        @Override
        public Integer getPort() {
            return 8080;
        }

        @Override
        public String getHost() {
            return "127.0.0.1";
        }

        @Override
        public String getAppName() {
            return "test-http";
        }

        @Override
        public String getContextPath() {
            return "/http";
        }

        @Override
        public String getIpAndPort() {
            return "127.0.0.1:8080";
        }

        @Override
        public Boolean getAddPrefixed() {
            return false;
        }

        @Override
        public RpcTypeEnum getRpcTypeEnum() {
            return RpcTypeEnum.HTTP;
        }
    }

    static class GrpcClientRegisterConfig implements ClientRegisterConfig {
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
}
