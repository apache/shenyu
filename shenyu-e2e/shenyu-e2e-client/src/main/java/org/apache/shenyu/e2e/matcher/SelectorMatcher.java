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

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import org.apache.shenyu.e2e.client.admin.model.data.SelectorData;
import org.apache.shenyu.e2e.client.admin.model.response.SelectorDTO;
import org.skyscreamer.jsonassert.JSONAssert;
import org.skyscreamer.jsonassert.JSONCompareMode;

import java.util.Objects;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.isEmptyOrNullString;
import static org.hamcrest.Matchers.notNullValue;

public class SelectorMatcher {
    private final ObjectMapper mapper = new ObjectMapper();
    private final SelectorData expected;
    
    private SelectorMatcher(SelectorData expected) {
        this.expected = expected;
    }
    
    @SneakyThrows
    public void matches(SelectorDTO actual) {
        String handle = actual.getHandle();
        if (Objects.nonNull(expected.getHandle())) {
            String expected = mapper.writer().writeValueAsString(this.expected.getHandle());
            JSONAssert.assertEquals(expected, handle, JSONCompareMode.LENIENT);
        } else {
            assertThat(actual, hasProperty("handle", isEmptyOrNullString()));
        }
        
        assertThat(actual, hasProperty("name", startsWith(expected.getName())));
        assertThat(actual, hasProperty("pluginId", equalTo(expected.getPlugin().getId())));
        assertThat(actual, hasProperty("type", equalTo(Integer.parseInt(expected.getType().getId()))));
        assertThat(actual, hasProperty("matchMode", equalTo(Integer.parseInt(expected.getMatchMode().getId()))));
        assertThat(actual, hasProperty("sort", equalTo(expected.getSort())));
        assertThat(actual, hasProperty("logged", equalTo(expected.isLogged())));
        assertThat(actual, hasProperty("enabled", equalTo(expected.isEnabled())));
        
        assertThat(actual, hasProperty("typeName", equalTo(expected.getType().alias())));
        assertThat(actual, hasProperty("matchModeName", equalTo(expected.getMatchMode().alias())));
        
        assertThat(actual, hasProperty("dateCreated", notNullValue()));
        assertThat(actual, hasProperty("dateUpdated", notNullValue()));
    }
    
    public static SelectorMatcher verify(SelectorData expected) {
        return new SelectorMatcher(expected);
    }
}
