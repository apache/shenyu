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

package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/**
 * Test case for {@link GraphNodeTypeEnum}.
 */
public class GraphNodeTypeEnumTest {

    @Test
    public void testGetName() {
        assertThat(GraphNodeTypeEnum.PLUGIN.getName(), is("PLUGIN"));
        assertThat(GraphNodeTypeEnum.CHOICE.getName(), is("CHOICE"));
        assertThat(GraphNodeTypeEnum.END.getName(), is("END"));
    }

    @Test
    public void testFromName() {
        assertThat(GraphNodeTypeEnum.fromName("PLUGIN").orElse(null), is(GraphNodeTypeEnum.PLUGIN));
        assertThat(GraphNodeTypeEnum.fromName("CHOICE").orElse(null), is(GraphNodeTypeEnum.CHOICE));
        assertThat(GraphNodeTypeEnum.fromName("END").orElse(null), is(GraphNodeTypeEnum.END));
        assertThat(GraphNodeTypeEnum.fromName("SWITCH").isPresent(), is(false));
        assertThat(GraphNodeTypeEnum.fromName(null).isPresent(), is(false));
    }

    @Test
    public void testIsValidName() {
        assertThat(GraphNodeTypeEnum.isValidName("PLUGIN"), is(true));
        assertThat(GraphNodeTypeEnum.isValidName("plugin"), is(false));
        assertThat(GraphNodeTypeEnum.isValidName(null), is(false));
    }
}
