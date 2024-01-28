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

package org.apache.shenyu.client.core.register;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertNotNull;

public class ShenyuClientRegisterRepositoryFactoryTest {
    @Test
    void testNewInstance() {
        // Arrange
        Properties props = new Properties();
        props.setProperty("key", "value");

        ShenyuRegisterCenterConfig config1 = new ShenyuRegisterCenterConfig("type1", "serverLists1", props);
        ShenyuRegisterCenterConfig config2 = new ShenyuRegisterCenterConfig("type2", "serverLists2", props);

        // Act
        ShenyuClientRegisterRepository repository1 = ShenyuClientRegisterRepositoryFactory.newInstance(config1);
        ShenyuClientRegisterRepository repository2 = ShenyuClientRegisterRepositoryFactory.newInstance(config2);
        ShenyuClientRegisterRepository repository3 = ShenyuClientRegisterRepositoryFactory.newInstance(config1);

        // Assert
        assertNotNull(repository1);
        assertNotNull(repository2);
        assertNotNull(repository3);

        // Check if the same repository instance is returned for the same register type
        Assertions.assertSame(repository1, repository3);
        Assertions.assertNotSame(repository1, repository2);
    }

}
