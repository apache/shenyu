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

package org.apache.shenyu.sdk.spring.support;

import java.lang.reflect.Method;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.apache.shenyu.sdk.spring.ShenyuClientFactoryBean;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * {@link SpringMvcContract} test.
 */
public class SpringMvcContractTest {

    private static final String PATH = "/dev/null";

    private static final Method FIND_BY_ID;

    private static final Method INSERT;

    private static final Method UPDATE;

    private static final Method DEL;

    static {
        try {
            FIND_BY_ID = InvocationClient.class.getDeclaredMethod("findById", String.class);
            INSERT = InvocationClient.class.getDeclaredMethod("insert", MetaData.class);
            UPDATE = InvocationClient.class.getDeclaredMethod("update", MetaData.class);
            DEL = InvocationClient.class.getDeclaredMethod("del", String.class);
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    private ShenyuClientFactoryBean bean;

    @BeforeEach
    public void init() {
        bean = mock(ShenyuClientFactoryBean.class);
        when(bean.getPath()).thenReturn(PATH);
    }

    @Test
    public void parseRequestTplTest() {
        SpringMvcContract contract = new SpringMvcContract();
        RequestTemplate template = contract.parseRequestTemplate(FIND_BY_ID, bean);

        assertSame(template.getMethod(), FIND_BY_ID);
        assertEquals(template.getPath(), "/findById");
        assertEquals(template.getHttpMethod(), ShenyuRequest.HttpMethod.GET);

        template = contract.parseRequestTemplate(INSERT, bean);
        assertSame(template.getMethod(), INSERT);
        assertEquals(template.getPath(), "/insert");
        assertEquals(template.getHttpMethod(), ShenyuRequest.HttpMethod.POST);

        template = contract.parseRequestTemplate(UPDATE, bean);
        assertSame(template.getMethod(), UPDATE);
        assertEquals(template.getPath(), "/update");
        assertEquals(template.getHttpMethod(), ShenyuRequest.HttpMethod.PUT);

        template = contract.parseRequestTemplate(DEL, bean);
        assertSame(template.getMethod(), DEL);
        assertEquals(template.getPath(), "/delete");
        assertEquals(template.getHttpMethod(), ShenyuRequest.HttpMethod.DELETE);
    }

    @ShenyuClient(value = "invocationClient", path = "/dev/null")
    public interface InvocationClient {

        /**
         * find by id mapping.
         * @param id id
         * @return MetaData
         */
        @GetMapping("/findById")
        MetaData findById(@RequestParam("id") String id);

        /**
         * insert mapping.
         * @param one one
         * @return Integer
         */
        @PostMapping("/insert")
        Integer insert(@RequestBody MetaData one);

        /**
         * update mapping.
         * @param one one
         * @return int
         */
        @PutMapping("/update")
        int update(MetaData one);

        /**
         * delete mapping.
         * @param id id
         * @return int
         */
        @DeleteMapping("/delete")
        int del(@RequestParam("id") String id);
    }

}
