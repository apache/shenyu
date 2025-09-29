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

package org.apache.shenyu.admin.model.vo;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public final class InstanceDataVisualVOTest {

    @Test
    void testPieAndLineData() {
        InstanceDataVisualVO vo = new InstanceDataVisualVO();
        List<InstanceDataVisualVO.Entry> pie = Arrays.asList(
                new InstanceDataVisualVO.Entry("ONLINE", 2L),
                new InstanceDataVisualVO.Entry("OFFLINE", 1L)
        );
        vo.setPieData(pie);
        InstanceDataVisualLineVO line1 = new InstanceDataVisualLineVO("ONLINE", Arrays.asList(1L, 2L));
        InstanceDataVisualLineVO line2 = new InstanceDataVisualLineVO("OFFLINE", Arrays.asList(0L, 1L));
        vo.setLineData(Arrays.asList(line1, line2));

        assertEquals(2, vo.getPieData().size());
        assertEquals("ONLINE", vo.getPieData().get(0).getName());
        assertEquals(2L, vo.getPieData().get(0).getValue());
        assertEquals(2, vo.getLineData().size());
        assertEquals("ONLINE", vo.getLineData().get(0).getName());
        assertNotNull(vo.getLineData().get(0).getData());
    }
}
