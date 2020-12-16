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
 * Test case for {@link DashboardUserDTO}.
 *
 * @author Jiang Jining
 */
public final class DashboardUserDTOTest {
    
    private DashboardUserDTO dashboardUserDTO;
   
    private DashboardUserDTO dashboardUserDTOConstructor;
    
    @Before
    public void initDashboardUserDTO() {
        dashboardUserDTO = DashboardUserDTO.builder().userName("Soul-admin")
                .enabled(true).password("jHcpKkiDbbQh7W7hh8yQSA==").role(2).build();
        dashboardUserDTOConstructor = new DashboardUserDTO();
        dashboardUserDTOConstructor.setId("68367257b0ef4f0d830ef59713d29d02");
    }
    
    @Test
    public void testDashboardUserDTO() {
        Assertions.assertNotNull(dashboardUserDTO);
        Assertions.assertNotNull(dashboardUserDTOConstructor);
        Assertions.assertEquals(dashboardUserDTOConstructor.getId(), "68367257b0ef4f0d830ef59713d29d02");
        Assertions.assertTrue(dashboardUserDTO.getEnabled());
        Assertions.assertEquals(dashboardUserDTO.getUserName(), "Soul-admin");
        Assertions.assertEquals(dashboardUserDTO.getPassword(), "jHcpKkiDbbQh7W7hh8yQSA==");
        Assertions.assertEquals(dashboardUserDTO.getRole(), 2);
        Assertions.assertNull(dashboardUserDTO.getId());
        dashboardUserDTO.setId("1335612349480407040");
        Assertions.assertEquals(dashboardUserDTO.getId(), "1335612349480407040");
    }
}
