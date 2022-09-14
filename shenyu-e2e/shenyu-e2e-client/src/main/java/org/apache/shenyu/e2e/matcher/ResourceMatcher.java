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

package org.apache.shenyu.e2e.matcher;

import lombok.AllArgsConstructor;
import lombok.Data;
import org.apache.shenyu.e2e.client.admin.model.data.ResourceData;
import org.apache.shenyu.e2e.client.admin.model.response.ResourceDTO;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.Lists;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class ResourceMatcher<T extends ResourceData, A extends ResourceDTO> implements Predicate<A> {
    private T expected;
    
    public ResourceMatcher(T expected) {
        this.expected = expected;
    }
    
    @Override
    public boolean test(A actual) {
        Class<? extends ResourceData> clazz = expected.getClass();
        Map<String, Field> expectedFieldMap = Lists.newArrayList(clazz.getDeclaredFields()).stream()
                .filter(f -> !"id".equals(f.getName()))
                .collect(Collectors.toMap(Field::getName, f -> f));
        
        Class<? extends ResourceDTO> actualClass = actual.getClass();
        Arrays.stream(actualClass.getDeclaredFields())
                .filter(f -> expectedFieldMap.containsKey(f.getName()))
                .map(f -> new Pair(expectedFieldMap.get(f.getName()), f))
                .filter(ff -> ff.actual.getType() == ff.expected.getType())
                .forEach(pair -> {
                    Field fa = pair.getActual();
                    fa.setAccessible(true);
                    Field fe = pair.getExpected();
                    fe.setAccessible(true);
                    
                    try {
                        System.out.println(fa.getName());
                        Assertions.assertThat(fa.get(actual)).isEqualTo(fe.get(expected)).describedAs("%s {}", fa.getName());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                });
        return false;
    }
    
    @Data
    @AllArgsConstructor
    static class Pair {
        Field expected;
        Field actual;
    }
}
