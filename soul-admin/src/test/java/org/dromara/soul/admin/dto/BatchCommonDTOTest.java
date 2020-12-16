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

import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import java.util.List;

/**
 * Test case for {@link BatchCommonDTO}.
 *
 * @author Jiang Jining
 */
public final class BatchCommonDTOTest {
    private BatchCommonDTO batchCommonDTO;
    
    @Before
    public void initBatchCommonDTO() {
        batchCommonDTO = new BatchCommonDTO();
        batchCommonDTO.setEnabled(true);
        batchCommonDTO.setIds(Lists.newArrayList("1", "2", "3", "4", "5"));
    }
    
    @Test
    public void testBatchCommonDTO() {
        Assertions.assertNotNull(batchCommonDTO);
        Assertions.assertTrue(batchCommonDTO.getEnabled());
        List<String> ids = batchCommonDTO.getIds();
        Assertions.assertNotNull(ids);
        Assertions.assertEquals(ids.size(), 5);
        batchCommonDTO.setEnabled(false);
        Assertions.assertFalse(batchCommonDTO.getEnabled());
    }
}