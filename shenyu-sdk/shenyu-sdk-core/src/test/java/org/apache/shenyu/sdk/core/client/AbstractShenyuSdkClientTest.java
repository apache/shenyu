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

package org.apache.shenyu.sdk.core.client;

import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.util.Collections;
import java.util.Properties;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link AbstractShenyuSdkClient}.
 */
public class AbstractShenyuSdkClientTest {

    @ParameterizedTest(name = "registry={0}, query={1}")
    @MethodSource("queryArguments")
    public void testExecutePreservesRawQuery(final boolean fromRegistry, final String query, final String expectedQuery) throws IOException {
        RegisterConfig registerConfig = new RegisterConfig("local", "127.0.0.1:9195", new Properties());
        ShenyuInstanceRegisterRepository repository = null;
        if (fromRegistry) {
            registerConfig.setServerLists("unused.invalid:9999");
            repository = mock(ShenyuInstanceRegisterRepository.class);
            when(repository.selectInstances("gateway")).thenReturn(Collections.singletonList(new InstanceEntity("gateway", "127.0.0.1", 9195)));
        }
        String sourceUrl = "http://original.invalid/sdk%2Fresource" + query;
        ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, sourceUrl, Collections.emptyMap(), null, "gateway", null);
        try (TestShenyuSdkClient client = new TestShenyuSdkClient()) {
            client.init(registerConfig, Collections.emptyList(), repository);

            ShenyuResponse response = client.execute(request);

            assertEquals("http://127.0.0.1:9195/sdk%2Fresource" + expectedQuery, response.getRequest().getUrl());
            assertEquals(sourceUrl, request.getUrl());
        }
        if (fromRegistry) {
            verify(repository).selectInstances("gateway");
        }
    }

    private static Stream<Arguments> queryArguments() {
        return Stream.of(false, true).flatMap(fromRegistry -> Stream.of(
                Arguments.of(fromRegistry, "?q=plain", "?q=plain"),
                Arguments.of(fromRegistry, "?q=a%26b%3Dc", "?q=a%26b%3Dc"),
                Arguments.of(fromRegistry, "?q=a%2Bb", "?q=a%2Bb"),
                Arguments.of(fromRegistry, "?q=a%20b", "?q=a%20b"),
                Arguments.of(fromRegistry, "?q=a%23b", "?q=a%23b"),
                Arguments.of(fromRegistry, "?q=a%25b", "?q=a%25b"),
                Arguments.of(fromRegistry, "?q=a%2526b", "?q=a%2526b"),
                Arguments.of(fromRegistry, "?q=%E4%B8%AD", "?q=%E4%B8%AD"),
                Arguments.of(fromRegistry, "?k%26y=v%3Dl", "?k%26y=v%3Dl"),
                Arguments.of(fromRegistry, "?q=1&q=2", "?q=1&q=2"),
                Arguments.of(fromRegistry, "", ""),
                Arguments.of(fromRegistry, "?", ""),
                Arguments.of(fromRegistry, "?q=", "?q=")));
    }

    private static final class TestShenyuSdkClient extends AbstractShenyuSdkClient {

        @Override
        protected ShenyuResponse doRequest(final ShenyuRequest request) {
            return new ShenyuResponse(200, "OK", Collections.emptyMap(), "ok", request);
        }

        @Override
        protected void initClient(final Properties props) {
        }
    }
}
