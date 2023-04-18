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

package org.apache.shenyu.admin.model.bean;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test cast for {@link DocParameter}.
 */
public final class DocParameterTest {

    private DocParameter docParameter;

    @BeforeEach
    public void setUp() {
        docParameter = new DocParameter();
        docParameter.setDescription("shenyuDescription");
        docParameter.setExample("shenyuExample");
        docParameter.setId(0);
        docParameter.setMaxLength("shenyuMaxLength");
        docParameter.setModule("shenyuSetModule");
        docParameter.setName("shenyuName");
        docParameter.setRequired(true);
        docParameter.setType("shenyuType");
        docParameter.setXExample("shenyuXExample");
        docParameter.setRefs(Collections.singletonList(docParameter));
    }

    @Test
    public void testEquals() {
        assertEquals("shenyuDescription", docParameter.getDescription());
        assertEquals(0, docParameter.getId().intValue());
        assertEquals("shenyuMaxLength", docParameter.getMaxLength());
        assertEquals("shenyuSetModule", docParameter.getModule());
        assertEquals("shenyuName", docParameter.getName());
        assertTrue(docParameter.isRequired());
        assertEquals("shenyuType", docParameter.getType());
        assertEquals("shenyuXExample", docParameter.getXExample());
        assertEquals(Collections.singletonList(docParameter), docParameter.getRefs());
    }

    @Test
    public void testGetExample() {
        assertEquals("shenyuExample", docParameter.getExample());
        docParameter.setExample("");
        final String example = docParameter.getExample();
        assertEquals("shenyuXExample", docParameter.getExample());
    }
}
