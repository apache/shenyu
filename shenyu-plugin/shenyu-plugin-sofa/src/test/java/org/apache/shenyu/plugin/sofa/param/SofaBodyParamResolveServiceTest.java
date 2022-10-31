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

package org.apache.shenyu.plugin.sofa.param;

import com.alipay.hessian.generic.model.GenericObject;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.lang.NonNull;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Test cases for WebSocketParamFilter.
 */
@ExtendWith(MockitoExtension.class)
public final class SofaBodyParamResolveServiceTest {
    
    @InjectMocks
    private SofaParamResolveServiceImpl impl;
    
    @Test
    public void testBuildParameter() {
        //language=JSON
        String body = "{\"id\": \"12345\",\"name\": \"candyYu\"}";
        String parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.Student";
        Pair<String[], Object[]> pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 1);
        assertIsStudent(pair.getRight()[0], true);
        
        //language=JSON
        body = "{\"testArray\":[{\"id\":\"123\",\"name\":\"candy\"},{\"id\":\"456\",\"name\":\"myth\"}]}";
        parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.Student[]";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 1);
        List<Student> right = new ArrayList<>((Collection) pair.getRight()[0]);
        assertIsStudent(right.get(0), true);
        assertIsStudent(right.get(1), true);

        //language=JSON
        body = "{\"ids\":[\"123\",\"456\"],\"name\":\"hello world\"}\n";
        parameterTypes = "java.lang.Integer[],java.lang.String";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 2);
        
        //language=JSON
        body = "{\"idMaps\":{\"id2\":\"2\",\"id1\":\"1\"},\"name\":\"hello world\"}\n";
        parameterTypes = "java.util.HashMap,java.lang.String";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 2);
        
        //language=JSON
        body = "{\"complexBeanTest\":{\"dubboTest\":{\"id\":\"123\",\"name\":\"xiaoyu\"},\"idLists\":[\"456\",\"789\"],\"idMaps\":{\"id2\":\"2\",\"id1\":\"1\"}}}";
        parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.ComplexBean";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 1);
        assertIsComplexBean(pair.getRight()[0], true);
        
        //language=JSON
        body = "{\"complexBeanTest\":{\"dubboTest\":{\"id\":\"123\",\"name\":\"xiaoyu\"},\"idLists\":[\"456\",\"789\"],\"idMaps\":{\"id2\":\"2\",\"id1\":\"1\"}},\"name\":\"xiaoyu\"}";
        parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.ComplexBean, java.lang.String";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 2);
        
        // test format json
        //language=JSON
        body = "{\n"
                + "  \"ids\": [\n"
                + "    \"123\",\n"
                + "    \"456\"\n"
                + "  ],\n"
                + "  \"id\": 123,\n"
                + "  \"name\": \"hello world\",\n"
                + "  \"testArray\": [\n"
                + "    {\n"
                + "      \"id\": \"123\",\n"
                + "      \"name\": \"candy\"\n"
                + "    },\n"
                + "    {\n"
                + "      \"id\": \"456\",\n"
                + "      \"name\": \"myth\"\n"
                + "    }\n"
                + "  ]\n"
                + "}\n";
        parameterTypes = "java.lang.Integer[],java.lang.Integer,java.lang.String,org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.Student[]";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 4);
        assertIsStudent(pair.getRight()[3], true);
    }
    
    @Test
    public void testBuildParameterWithNull() {
        //language=JSON
        String body = "{\"student\":{\"id\":null,\"name\":null}}";
        String parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.Student";
        Pair<String[], Object[]> pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 1);
        assertIsStudent(pair.getRight()[0], false);
        
        //language=JSON
        body = "{\"students\":[{\"id\":null,\"name\":null}]}";
        parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.Student[]";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 1);
        assertIsStudent(pair.getRight()[0], false);
        
        //language=JSON
        body = "{\"complexBean\":{\"dubboTest\":{\"id\":null,\"name\":null},\"idLists\":[null,null],\"idMaps\":{\"id2\":null,\"id1\":null}}}";
        parameterTypes = "org.apache.shenyu.web.rpc.DubboMultiParameterResolveServiceImplTest.ComplexBean";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 1);
        assertIsComplexBean(pair.getRight()[0], false);
        
        //language=JSON
        body = "{\"name\":null}";
        parameterTypes = "java.lang.String";
        pair = impl.buildParameter(body, parameterTypes);
        assertLeftAndRightSame(pair, 1);
        assertNull(pair.getRight()[0]);
    }
    
    private void assertLeftAndRightSame(final Pair<String[], Object[]> pair, final int i) {
        assertThat(pair.getLeft().length, is(i));
        assertThat(pair.getRight().length, is(i));
    }
    
    /**
     * assert object is student.<br>
     * <ul>
     *     <li>{@link GenericObject} structure is {@link Student}.</li>
     *     <li>{@link Map} structure is {@link Student}.</li>
     *     <li>{@link List} value structure is {@link Student}.</li>
     * </ul>
     *
     * @param object            target.
     * @param allowValueNotNull allow value not null.
     * @see Student
     */
    private void assertIsStudent(@NonNull final Object object, final boolean allowValueNotNull) {
        assertIsObject(object, Student.class, !allowValueNotNull);
    }
    
    /**
     * assert object is student.<br>
     * <ul>
     *     <li>{@link GenericObject} structure is {@link ComplexBean}.</li>
     *     <li>{@link Map} structure is {@link ComplexBean}.</li>
     *     <li>{@link List} value structure is {@link ComplexBean}.</li>
     * </ul>
     *
     * @param object            target.
     * @param allowValueNotNull allow value not null.
     * @see ComplexBean
     */
    private void assertIsComplexBean(@NonNull final Object object, final boolean allowValueNotNull) {
        assertIsObject(object, ComplexBean.class, !allowValueNotNull);
        
    }
    
    /**
     * assert object is target structure.<br>
     * <ul>
     *     <li>{@link GenericObject} structure is target structure.</li>
     *     <li>{@link Map} structure is target structure.</li>
     *     <li>{@link List} value structure is target structure.</li>
     * </ul>
     *
     * @param object             object.
     * @param clazz              target structure.
     * @param allowValueToBeNull allow value to be Null. If the value is not null, the deep assertion
     */
    private void assertIsObject(@NonNull final Object object, @NonNull final Class<?> clazz, final boolean allowValueToBeNull) {
        final Field[] fields = clazz.getDeclaredFields();
        if (object instanceof GenericObject) {
            final GenericObject genericObject = (GenericObject) object;
            for (Field field : fields) {
                if (excludeField(field)) {
                    continue;
                }
                if (!genericObject.hasField(field.getName())) {
                    fail(genericObject + " fieldName: " + field.getName() + " fieldType => " + field.getType().getName());
                }
                // not allow value is null
                if (!allowValueToBeNull) {
                    assertNotNull(genericObject.getField(field.getName()));
                    // value is not null, deep assert
                    assertIsObject(genericObject.getField(field.getName()), field.getType(), allowValueToBeNull);
                }
            }
        }
        if (object instanceof Map) {
            final Map<?, ?> map = (Map<?, ?>) object;
            for (Field field : fields) {
                if (excludeField(field)) {
                    continue;
                }
                if (!map.containsKey(field.getName())) {
                    fail(map + " fieldName: " + field.getName() + " fieldType => " + field.getType().getName());
                }
                // not allow value is null
                if (!allowValueToBeNull) {
                    assertNotNull(map.get(field.getName()));
                    assertIsObject(map.get(field.getName()), field.getType(), allowValueToBeNull);
                }
            }
        }
        if (object instanceof List) {
            for (Object o : (List<?>) object) {
                assertIsObject(o, clazz, allowValueToBeNull);
            }
        }
    }
    
    /**
     * exclude field.<br>
     * if used jacoco, if used idea test coverage, It will add fields to your clazz. you mast ignore.
     * <a href="https://youtrack.jetbrains.com/issue/IDEA-274803">idea test coverage solution</a>
     * <a href="https://www.eclemma.org/jacoco/trunk/doc/faq.html">jacoco solution</a>
     *
     * @param field field
     * @return is exclude
     */
    private boolean excludeField(final Field field) {
        return StringUtils.startsWithAny(field.getName(), "_", "$");
    }
    
    private static final class Student {
        
        private String id;
        
        private String name;
        
        private Student() {
        }
        
        private Student(final String id, final String name) {
            this.id = id;
            this.name = name;
        }
        
        private String getId() {
            return id;
        }
        
        private void setId(final String id) {
            this.id = id;
        }
        
        private String getName() {
            return name;
        }
        
        private void setName(final String name) {
            this.name = name;
        }
    }
    
    private static final class ComplexBean {
        
        private Student dubboTest;
        
        private List<String> idLists;
        
        private Map<String, String> idMaps;
        
        private ComplexBean() {
        }
        
        private ComplexBean(final Student dubboTest, final List<String> idLists, final Map<String, String> idMaps) {
            this.dubboTest = dubboTest;
            this.idLists = idLists;
            this.idMaps = idMaps;
        }
        
        private Student getDubboTest() {
            return dubboTest;
        }
        
        private void setDubboTest(final Student dubboTest) {
            this.dubboTest = dubboTest;
        }
        
        private List<String> getIdLists() {
            return idLists;
        }
        
        private void setIdLists(final List<String> idLists) {
            this.idLists = idLists;
        }
        
        private Map<String, String> getIdMaps() {
            return idMaps;
        }
        
        private void setIdMaps(final Map<String, String> idMaps) {
            this.idMaps = idMaps;
        }
    }
}
