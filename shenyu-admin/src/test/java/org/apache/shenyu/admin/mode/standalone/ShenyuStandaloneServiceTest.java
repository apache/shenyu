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

package org.apache.shenyu.admin.mode.standalone;

import org.apache.shenyu.admin.scale.scaler.ScaleLifecycleService;
import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.inOrder;

/**
 * Test cases for {@link ShenyuStandaloneService}.
 */
@ExtendWith(MockitoExtension.class)
public final class ShenyuStandaloneServiceTest {

    @Mock
    private UpstreamCheckService upstreamCheckService;

    @Mock
    private LoadServiceDocEntry loadServiceDocEntry;

    @Mock
    private InstanceCheckService instanceCheckService;

    @Mock
    private ScaleLifecycleService scaleLifecycleService;

    private ShenyuStandaloneService standaloneService;

    @BeforeEach
    public void setUp() {
        standaloneService = new ShenyuStandaloneService(
                upstreamCheckService, loadServiceDocEntry, instanceCheckService, scaleLifecycleService);
    }

    @Test
    public void testStartDelegatesToServices() {
        standaloneService.start("127.0.0.1", 9095, "/");

        final InOrder order = inOrder(
                upstreamCheckService, instanceCheckService, scaleLifecycleService, loadServiceDocEntry);
        order.verify(upstreamCheckService).setup();
        order.verify(instanceCheckService).setup();
        order.verify(scaleLifecycleService).start();
        order.verify(loadServiceDocEntry).loadApiDocument();
    }

    @Test
    public void testShutdownDelegatesToServices() {
        standaloneService.shutdown();

        final InOrder order = inOrder(upstreamCheckService, instanceCheckService, scaleLifecycleService);
        order.verify(upstreamCheckService).close();
        order.verify(instanceCheckService).close();
        order.verify(scaleLifecycleService).stop();
    }
}
