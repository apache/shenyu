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

package org.apache.shenyu.client.apache.dubbo.validation;

import org.apache.dubbo.common.URL;
import org.apache.dubbo.validation.Validator;
import org.apache.shenyu.client.apache.dubbo.validation.mock.MockValidationParameter;
import org.apache.shenyu.client.apache.dubbo.validation.service.TestService;
import org.junit.Before;
import org.junit.Test;

import javax.validation.ValidationException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Test case for {@link ApacheDubboClientValidator}.
 */
public final class ApacheDubboClientValidatorTest {

    private static final String MOCK_SERVICE_URL =
            "mock://localhost:28000/org.apache.shenyu.client.apache.dubbo.validation.mock.MockValidatorTarget";

    private ApacheDubboClientValidator apacheDubboClientValidatorUnderTest;

    /**
     * test method {@link ApacheDubboClientValidator#validate(java.lang.String, java.lang.Class[], java.lang.Object[])}.
     */
    @Test
    public void validate() throws Exception {
        URL url = URL.valueOf("dubbo://127.0.0.1:20880/org.apache.shenyu"
                + ".client.apache.dubbo.validation.service.TestService"
                + "?accepts=500&anyhost=true&application=shenyu-proxy"
                + "&bind.ip=127.0.0.1&bind.port=20880&deprecated=false"
                + "&dubbo=2.0.2&dynamic=true&generic=false"
                + "&interface=org.apache.shenyu.client.apache.dubbo.validation.service.TestService"
                + "&keep.alive=true&methods=test&pid=67352&qos.enable=false&release=2.7.0"
                + "&side=provider&threadpool=fixed&threads=500&timeout=20000"
                + "&timestamp=1608119259859&validation=shenyuValidation");
        Validator apacheDubboClientValidator = new ApacheDubboClientValidation().getValidator(url);

        apacheDubboClientValidator.validate("test",
                new Class[]{TestService.TestObject.class},
                new Object[]{new TestService.TestObject(1)});
    }

    @Before
    public void setUp() {
        URL url = URL.valueOf(MOCK_SERVICE_URL);
        apacheDubboClientValidatorUnderTest = new ApacheDubboClientValidator(url);
    }

    @Test(expected = NoSuchMethodException.class)
    public void testValidateWithNonExistMethod() throws Exception {
        apacheDubboClientValidatorUnderTest
                .validate("nonExistingMethod", new Class<?>[]{String.class}, new Object[]{"arg1"});
    }

    @Test
    public void testValidateWithExistMethod() throws Exception {
        final URL url = URL.valueOf(MOCK_SERVICE_URL + "?shenyuValidation=org.hibernate.validator.HibernateValidator");
        ApacheDubboClientValidator apacheDubboClientValidator = new ApacheDubboClientValidator(url);
        apacheDubboClientValidator
                .validate("methodOne", new Class<?>[]{String.class}, new Object[]{"anything"});
        apacheDubboClientValidator
                .validate("methodOne", new Class<?>[]{String.class}, new Object[]{"anything"});
    }

    @Test(expected = ValidationException.class)
    public void testValidateWhenMeetsConstraintThenValidationFailed() throws Exception {
        apacheDubboClientValidatorUnderTest
                .validate(
                        "methodTwo",
                        new Class<?>[]{MockValidationParameter.class},
                        new Object[]{new MockValidationParameter("NotBeNull")});
    }

    @Test
    public void testValidateWithArrayArg() throws Exception {
        MockValidationParameter parameter = new MockValidationParameter("parameter");
        MockValidationParameter[] mockValidationParameters = {parameter};
        Object[] objects = {mockValidationParameters};
        apacheDubboClientValidatorUnderTest.validate("methodThree", new Class<?>[]{MockValidationParameter[].class}, objects);
    }

    @Test
    public void testItWithCollectionArg() throws Exception {
        apacheDubboClientValidatorUnderTest
                .validate(
                        "methodFour",
                        new Class<?>[]{List.class},
                        new Object[]{Collections.singletonList("parameter")});
    }

    @Test
    public void testItWithMapArg() throws Exception {
        final Map<String, String> map = new HashMap<>();
        map.put("key", "value");
        apacheDubboClientValidatorUnderTest.validate(
                "methodFive", new Class<?>[]{Map.class}, new Object[]{map});
    }
}
