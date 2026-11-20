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

package org.apache.shenyu.admin.model.entity;

import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests dashboard user status mapping for creation and updates.
 */
final class DashboardUserDOTest {

    @Test
    void preservesDisabledStatusOnCreation() {
        DashboardUserDTO dto = new DashboardUserDTO();
        dto.setEnabled(false);
        DashboardUserDO entity = DashboardUserDO.buildDashboardUserDO(dto);
        assertFalse(entity.getEnabled());
        assertNotNull(entity.getId());
        assertNotNull(entity.getDateCreated());
    }

    @Test
    void preservesEnabledStatusOnCreation() {
        DashboardUserDTO dto = new DashboardUserDTO();
        dto.setEnabled(true);
        assertTrue(DashboardUserDO.buildDashboardUserDO(dto).getEnabled());
    }

    @Test
    void defaultsNewUsersToEnabledWhenStatusIsOmitted() {
        assertTrue(DashboardUserDO.buildDashboardUserDO(new DashboardUserDTO()).getEnabled());
    }

    @Test
    void preservesDisabledStatusOnUpdate() {
        DashboardUserDTO dto = new DashboardUserDTO();
        dto.setId("existing-user");
        dto.setEnabled(false);
        DashboardUserDO entity = DashboardUserDO.buildDashboardUserDO(dto);
        assertEquals("existing-user", entity.getId());
        assertFalse(entity.getEnabled());
        assertNull(entity.getDateCreated());
    }

    @Test
    void preservesOmittedStatusOnPartialUpdate() {
        DashboardUserDTO dto = new DashboardUserDTO();
        dto.setId("existing-user");
        assertNull(DashboardUserDO.buildDashboardUserDO(dto).getEnabled());
    }
}
