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

package org.apache.shenyu.admin.model.query;

import org.apache.shenyu.admin.AbstractReflectGetterSetterTest;
import org.junit.jupiter.api.Test;
import java.util.HashSet;
import java.util.Set;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;

/**
 * Test case for TagRelationQuery.
 */
public class TagRelationQueryTest extends AbstractReflectGetterSetterTest {
    @Override
    protected Class<?> getTargetClass() {
        return TagRelationQuery.class;
    }

    @Test
    public void testEqualsAndHashCode() {
        TagRelationQuery tagRelationQuery1 = TagRelationQuery.builder().apiId("apiId").tagId("tagId").build();
        TagRelationQuery tagRelationQuery2 = TagRelationQuery.builder().apiId("apiId").tagId("tagId").build();

        Set<TagRelationQuery> set = new HashSet<>();
        set.add(tagRelationQuery1);
        set.add(tagRelationQuery2);

        assertThat(set, hasSize(1));
    }
}
