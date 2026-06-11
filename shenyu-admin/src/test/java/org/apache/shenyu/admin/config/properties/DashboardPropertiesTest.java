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

package org.apache.shenyu.admin.config.properties;

import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;

/**
 * Test cases for {@link DashboardProperties}.
 */
@ExtendWith(MockitoExtension.class)
public class DashboardPropertiesTest extends AbstractConfigurationTest {
    
    @Test
    public void dashboardPropertiesTest() {
        final DashboardProperties dashboardProperties = new DashboardProperties();
        dashboardProperties.setOnlyCleanDays(0);
        dashboardProperties.setEnablePrintApiLog(true);
        dashboardProperties.setRecordLogLimit(0);
        dashboardProperties.setEnableOnlySuperAdminPermission(false);
        dashboardProperties.setEnableSuperAdminPasswordSafe(false);
        dashboardProperties.setSuperAdminPasswordValidDuration(0L);

        Assertions.assertEquals(0, dashboardProperties.getOnlyCleanDays());
        Assertions.assertEquals(true, dashboardProperties.getEnablePrintApiLog());
        Assertions.assertEquals(0, dashboardProperties.getRecordLogLimit());
        Assertions.assertEquals(false, dashboardProperties.getEnableOnlySuperAdminPermission());
        Assertions.assertEquals(false, dashboardProperties.getEnableSuperAdminPasswordSafe());
        Assertions.assertEquals(0, dashboardProperties.getSuperAdminPasswordValidDuration());
    }

    @Test
    public void testSpecified() {
        load(DashboardPropertiesTest.DashboardPropertiesConfiguration.class,
                "shenyu.dashboard.core.recordLogLimit=10",
                "shenyu.dashboard.core.onlyCleanDays=1",
                "shenyu.dashboard.core.enablePrintApiLog=true",
                "shenyu.dashboard.core.enableOnlySuperAdminPermission=false",
                "shenyu.dashboard.core.enableSuperAdminPasswordSafe=false",
                "shenyu.dashboard.core.superAdminPasswordValidDuration=0");

        List<String> onlySuperAdminPermission = new ArrayList<>();
        onlySuperAdminPermission.add("1");
        DashboardProperties dashboardProperties = getContext().getBean(DashboardProperties.class);
        dashboardProperties.setOnlySuperAdminPermission(onlySuperAdminPermission);

        Assertions.assertEquals(10, dashboardProperties.getRecordLogLimit());
        Assertions.assertEquals(1, dashboardProperties.getOnlyCleanDays());
        Assertions.assertTrue(dashboardProperties.getEnablePrintApiLog());
        Assertions.assertFalse(dashboardProperties.getEnableOnlySuperAdminPermission());
        Assertions.assertFalse(dashboardProperties.getEnableSuperAdminPasswordSafe());
        Assertions.assertEquals(0L, dashboardProperties.getSuperAdminPasswordValidDuration());
        Assertions.assertEquals(1, dashboardProperties.getOnlySuperAdminPermission().size());
        Assertions.assertEquals("1", dashboardProperties.getOnlySuperAdminPermission().get(0));
    }

    @Test
    public void testOnlySuperAdminPermission() {
        final DashboardProperties dashboardProperties = new DashboardProperties();
        dashboardProperties.afterPropertiesSet();
        Assertions.assertEquals(12, dashboardProperties.getOnlySuperAdminPermission().size());
        // Check user permissions
        Assertions.assertEquals("system:manager:add", dashboardProperties.getOnlySuperAdminPermission().get(0));
        Assertions.assertEquals("system:manager:edit", dashboardProperties.getOnlySuperAdminPermission().get(1));
        Assertions.assertEquals("system:manager:delete", dashboardProperties.getOnlySuperAdminPermission().get(2));

        // Check role permissions
        Assertions.assertEquals("system:role:edit", dashboardProperties.getOnlySuperAdminPermission().get(3));
        Assertions.assertEquals("system:role:add", dashboardProperties.getOnlySuperAdminPermission().get(4));
        Assertions.assertEquals("system:role:delete", dashboardProperties.getOnlySuperAdminPermission().get(5));

        // Check resource permissions
        Assertions.assertEquals("system:resource:addButton", dashboardProperties.getOnlySuperAdminPermission().get(6));
        Assertions.assertEquals("system:resource:addMenu", dashboardProperties.getOnlySuperAdminPermission().get(7));
        Assertions.assertEquals("system:resource:editButton", dashboardProperties.getOnlySuperAdminPermission().get(8));
        Assertions.assertEquals("system:resource:editMenu", dashboardProperties.getOnlySuperAdminPermission().get(9));
        Assertions.assertEquals("system:resource:deleteMenu", dashboardProperties.getOnlySuperAdminPermission().get(10));
        Assertions.assertEquals("system:resource:deleteButton", dashboardProperties.getOnlySuperAdminPermission().get(11));

        List<String> onlySuperAdminPermission = new ArrayList<>();
        onlySuperAdminPermission.add("1");
        dashboardProperties.setOnlySuperAdminPermission(onlySuperAdminPermission);
        Assertions.assertEquals(1, dashboardProperties.getOnlySuperAdminPermission().size());

        dashboardProperties.setEnableOnlySuperAdminPermission(false);
        dashboardProperties.afterPropertiesSet();
        Assertions.assertEquals(0, dashboardProperties.getOnlySuperAdminPermission().size());
    }

    @Configuration
    @EnableConfigurationProperties(DashboardProperties.class)
    static class DashboardPropertiesConfiguration {
    }
}
