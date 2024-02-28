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

package org.apache.shenyu.sdk.core.retry;

import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import static java.lang.String.format;

/**
 * Test for {@link RetryableException}.
 */
public class RetryableExceptionTest {

    private ShenyuRequest request;
    
    private RetryableException retryableException;

    @Before
    public void setUp() {
        Map<String, Collection<String>> headerMap = new HashMap<>();
        headerMap.put("header", Arrays.asList("header1", "header2"));
        request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "https://shenyu.apache.org",
                headerMap, null, null, null);

        try {
            //do request logic...
            //when occurred IOException
            throw new IOException();
        } catch (IOException cause) {
            //then throw RetryableException
            retryableException = new RetryableException(
                    format("%s executing %s %s", cause.getMessage(), request.getHttpMethod(), request.getUrl()),
                    cause,
                    new Date(),
                    request);
            Assert.assertNotNull(retryableException);
            //throw retryableException;
        }
    }

    @Test
    public void retryableExceptionTest() {
        Assert.assertNotNull(retryableException);
    }

    @Test
    public void retryAfterTest() {
        Date date = retryableException.retryAfter();
        Assert.assertNotNull(date);
    }

    @Test
    public void methodTest() {
        ShenyuRequest.HttpMethod httpMethod = retryableException.method();
        Assert.assertNotNull(httpMethod);
    }
}
