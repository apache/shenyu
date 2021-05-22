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

package org.apache.shenyu.plugin.grpc.resolver;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For {@link ShenyuServiceInstanceLists}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ShenyuServiceInstanceListsTest {

    private ShenyuServiceInstanceLists shenyuServiceInstanceLists;

    private CopyOnWriteArrayList<ShenyuServiceInstance> shenyuServiceInstances;

    @Before
    public void setUp() {
        shenyuServiceInstances = new CopyOnWriteArrayList<>();
        shenyuServiceInstances.add(mock(ShenyuServiceInstance.class));
        shenyuServiceInstanceLists = new ShenyuServiceInstanceLists(shenyuServiceInstances, "shenyu");
    }

    @Test
    public void noArgsConstructor() {
        shenyuServiceInstanceLists = new ShenyuServiceInstanceLists();
    }

    @Test
    public void testSet() {
        shenyuServiceInstanceLists.setAppName("shenyu");
        shenyuServiceInstanceLists.setShenyuServiceInstances(shenyuServiceInstances);
        assertEquals(shenyuServiceInstanceLists.getAppName(), "shenyu");
        assertEquals(shenyuServiceInstanceLists.getShenyuServiceInstances(), shenyuServiceInstances);
    }

    @Test
    public void getCopyInstances() {
        List<ShenyuServiceInstance> list = shenyuServiceInstanceLists.getCopyInstances();
        assert list.size() == 1;
    }
}
