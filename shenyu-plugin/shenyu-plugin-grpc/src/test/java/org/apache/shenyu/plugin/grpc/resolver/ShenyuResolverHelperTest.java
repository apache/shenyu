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

import static org.junit.Assert.assertNotNull;

/**
 * The Test Case For {@link ShenyuResolverHelper}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ShenyuResolverHelperTest {

    private ShenyuServiceInstance serviceInstance;

    @Before
    public void setUp() {
        serviceInstance = new ShenyuServiceInstance("localhost", 8080);
    }

    @Test
    public void convertToEquivalentAddressGroup() {
        assertNotNull(ShenyuResolverHelper.convertToEquivalentAddressGroup(serviceInstance));
    }
}
