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

package org.apache.shenyu.sdk.spring.registrar.package2;

import org.apache.shenyu.sdk.spring.EnableShenyuClients;
import org.apache.shenyu.sdk.spring.ShenyuClient;

/**
 * ClientConfig.
 */
public class ClientConfig {

    @EnableShenyuClients(basePackageClasses = {NullUrlShenClient.class})
    public static class NullUrlTestConfig {

    }

    @EnableShenyuClients(basePackageClasses = {NotNullUrlShenClient.class})
    public static class NotNullUrlTestConfig {

    }

    @ShenyuClient(name = "nullUrlShenClient", url = "${test.url:#{null}}", path = "${test.path:#{null}}")
    public interface NullUrlShenClient {

    }

    @ShenyuClient(name = "notNullUrlShenClient", url = "#{test.url}", path = "#{test.path}")
    public interface NotNullUrlShenClient {

    }
}
