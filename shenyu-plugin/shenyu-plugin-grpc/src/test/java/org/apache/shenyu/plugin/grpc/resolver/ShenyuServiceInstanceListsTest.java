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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * The Test Case For {@link ShenyuServiceInstanceLists}.
 */
@ExtendWith(MockitoExtension.class)
public class ShenyuServiceInstanceListsTest {
    
    private final String appName = "shenyu";
    
    private ShenyuServiceInstanceLists shenyuServiceInstanceLists;
    
    private CopyOnWriteArrayList<ShenyuServiceInstance> shenyuServiceInstances;
    
    @BeforeEach
    public void setUp() {
        shenyuServiceInstances = new CopyOnWriteArrayList<>();
        shenyuServiceInstances.add(mock(ShenyuServiceInstance.class));
        shenyuServiceInstanceLists = new ShenyuServiceInstanceLists(shenyuServiceInstances, appName);
    }
    
    @Test
    public void noArgsConstructor() {
        shenyuServiceInstanceLists = new ShenyuServiceInstanceLists();
        assertNotNull(shenyuServiceInstanceLists.getShenyuServiceInstances());
    }
    
    @Test
    public void testSet() {
        shenyuServiceInstanceLists.setAppName("shenyu");
        shenyuServiceInstanceLists.addShenyuServiceInstances(Stream.of(mock(ShenyuServiceInstance.class)).collect(Collectors.toList()));
        assertEquals(shenyuServiceInstanceLists.getAppName(), appName);
        assertTrue(shenyuServiceInstanceLists.getShenyuServiceInstances().containsAll(shenyuServiceInstances));
    }
    
    @Test
    public void getCopyInstances() {
        List<ShenyuServiceInstance> list = shenyuServiceInstanceLists.getCopyInstances();
        assertEquals(1, list.size());
    }
}
