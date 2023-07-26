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

package org.apache.shenyu.sdk.spring.annotation;

import com.google.common.collect.Maps;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.mockito.Mockito.mock;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * {@link RequestBody} process test.
 */
public class RequestBodyParameterProcessorTest {

    private ShenyuRequest request;

    private AnnotatedParameterProcessor processor;

    @BeforeEach
    public void init() {
        this.processor = new RequestBodyParameterProcessor();
        this.request = ShenyuRequest.create(ShenyuRequest.HttpMethod.POST, "/dev/null", Maps.newHashMap(), "", "test", mock(RequestTemplate.class));
    }

    @Test
    public void processArgumentNullTest() {
        final RequestBody body = mock(RequestBody.class);
        processor.processArgument(request, body, "");

        assertEquals(request.getBody(), "");
    }

    @ParameterizedTest
    @CsvSource({
        "'/dev/url', shenyu, V1.0, true, 1",
        "'/dev/null', shenyu, V2.0, false, 2"
    })
    public void processArgumentSomeObjectTest(final String url, final String group, final String version, final Boolean status, final int weight) {
        final RequestBody body = mock(RequestBody.class);

        Upstream upstream = Upstream.builder()
                                .url(url)
                                .group(group)
                                .version(version)
                                .status(status)
                                .weight(weight)
                                .build();
        processor.processArgument(request, body, upstream);
        assertEquals(request.getBody(), JsonUtils.toJson(upstream));
    }

}
