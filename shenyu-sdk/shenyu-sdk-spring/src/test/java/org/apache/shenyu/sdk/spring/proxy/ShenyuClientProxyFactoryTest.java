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

package org.apache.shenyu.sdk.spring.proxy;

import java.io.IOException;
import java.util.concurrent.ConcurrentMap;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledForJreRange;
import org.junit.jupiter.api.condition.JRE;

/**
 * {@link ShenyuClientProxyFactory} test.
 */
public class ShenyuClientProxyFactoryTest extends AbstractProxyTest {

    @Test
    @SuppressWarnings("unchecked")
    @DisabledForJreRange(min = JRE.JAVA_16)
    public void factoryTest() throws IllegalAccessException, IOException {
        init();

        final ConcurrentMap<Class<?>, Object> proxyMap = (ConcurrentMap<Class<?>, Object>) PROXY_CACHE.get(null);
        assertNotNull(proxyMap);
    }

}
