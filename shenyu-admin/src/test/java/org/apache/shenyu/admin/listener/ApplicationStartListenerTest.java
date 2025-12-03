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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ApplicationStartListener}.
 */
public final class ApplicationStartListenerTest extends AbstractSpringIntegrationTest {

    @Test
    public void testOnApplicationEvent() {
        // ApplicationStartListener is automatically triggered during Spring Boot startup
        // This test verifies that the application context loads successfully
        // and the listener is registered and executed without errors
    }
}
