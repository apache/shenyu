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

package org.dromara.soul.admin.dto;

import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link SoulDictDTO}.
 *
 * @author Jiang Jining
 */
public final class SoulDictDTOTest {
    
    private SoulDictDTO soulDictDTO;
    
    @Before
    public void initSoulDictDTO() {
        soulDictDTO = new SoulDictDTO();
        soulDictDTO.setDictCode("01");
        soulDictDTO.setDictName("Test dict name");
        soulDictDTO.setDictValue("Test dict value");
        soulDictDTO.setEnabled(false);
        soulDictDTO.setSort(1);
        soulDictDTO.setDesc("Test dict desc");
        soulDictDTO.setId("3320c187baa9478abd86a68223b49f50");
        soulDictDTO.setType("e561f2d31417426fa929a193ad2f77d2");
    }
    
    @Test
    public void testSoulDictDTO() {
        Assertions.assertNotNull(soulDictDTO);
        Assertions.assertEquals(soulDictDTO.getDictCode(), "01");
        Assertions.assertEquals(soulDictDTO.getDictName(), "Test dict name");
        Assertions.assertEquals(soulDictDTO.getDictValue(), "Test dict value");
        Assertions.assertEquals(soulDictDTO.getDesc(), "Test dict desc");
        Assertions.assertEquals(soulDictDTO.getId(), "3320c187baa9478abd86a68223b49f50");
        Assertions.assertEquals(soulDictDTO.getType(), "e561f2d31417426fa929a193ad2f77d2");
        Assertions.assertFalse(soulDictDTO.getEnabled());
        Assertions.assertEquals(soulDictDTO.getSort(), 1);
        soulDictDTO.setEnabled(true);
        Assertions.assertTrue(soulDictDTO.getEnabled());
    }
}