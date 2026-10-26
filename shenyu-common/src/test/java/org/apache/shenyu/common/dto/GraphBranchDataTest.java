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

package org.apache.shenyu.common.dto;

import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

/**
 * Test case for {@link GraphBranchData}.
 */
public class GraphBranchDataTest {

    @Test
    public void testGetterSetter() {
        final GraphBranchData branch = new GraphBranchData();
        branch.setConditions(Collections.singletonList(new ConditionData()));
        branch.setMatchMode(1);
        branch.setTarget("n1");

        assertThat(branch.getConditions(), hasSize(1));
        assertThat(branch.getMatchMode(), is(1));
        assertThat(branch.getTarget(), is("n1"));
    }

    @Test
    public void testAllArgsConstructor() {
        final GraphBranchData branch = new GraphBranchData(Collections.emptyList(), 0, "n2");
        assertThat(branch.getTarget(), is("n2"));
        assertThat(branch.getMatchMode(), is(0));
    }

    @Test
    public void testEqualsAndHashCode() {
        final GraphBranchData one = new GraphBranchData(Collections.emptyList(), 0, "n1");
        final GraphBranchData two = new GraphBranchData(Collections.emptyList(), 0, "n1");
        final Set<GraphBranchData> set = new HashSet<>();
        set.add(one);
        set.add(two);
        assertThat(set, hasSize(1));
        assertThat(one, is(two));
        assertThat(one.hashCode(), is(two.hashCode()));
    }
}
