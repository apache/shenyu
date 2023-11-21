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

package org.apache.shenyu.sdk.core.util;

import org.junit.Assert;
import org.junit.Test;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import static org.mockito.Mockito.mock;

/**
 * Test for {@link Types}.
 */
public class TypesTest {

    @Test
    public void resolveReturnTypeTest() {
        //case1: baseType and overridingType are booth Class instances
        Class<?> baseType = Number.class;
        Class<?> overridingType = Integer.class;
        Type result = Types.resolveReturnType(baseType, overridingType);
        Assert.assertEquals(result, overridingType);

        //case2: baseType is Class instance, overridingType is ParameterizedType instance
        baseType = Comparable.class;
        ParameterizedType parameterizedType = mock(ParameterizedType.class);
        result = Types.resolveReturnType(baseType, parameterizedType);
        Assert.assertEquals(result, parameterizedType);

        //case3: baseType is Class instance, overridingType is TypeVariable instance
        baseType = Comparable.class;
        TypeVariable typeVariable = mock(TypeVariable.class);
        result = Types.resolveReturnType(baseType, typeVariable);
        Assert.assertEquals(result, typeVariable);

        //case4 :vaseType and overridingType are not matching any conditions
        baseType = String.class;
        overridingType = Integer.class;
        result = Types.resolveReturnType(baseType, overridingType);
        Assert.assertEquals(result, baseType);
    }

}
