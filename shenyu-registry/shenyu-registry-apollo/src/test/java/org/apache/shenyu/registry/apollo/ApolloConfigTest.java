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

package org.apache.shenyu.registry.apollo;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ApolloConfigTest {
    /**
     * Methods under test.
     *
     * <ul>
     *   <li>default or parameterless constructor of {@link ApolloConfig}
     *   <li>{@link ApolloConfig#setAppId(String)}
     *   <li>{@link ApolloConfig#setClusterName(String)}
     *   <li>{@link ApolloConfig#setEnv(String)}
     *   <li>{@link ApolloConfig#setNamespace(String)}
     *   <li>{@link ApolloConfig#setPortalUrl(String)}
     *   <li>{@link ApolloConfig#setToken(String)}
     *   <li>{@link ApolloConfig#getAppId()}
     *   <li>{@link ApolloConfig#getClusterName()}
     *   <li>{@link ApolloConfig#getEnv()}
     *   <li>{@link ApolloConfig#getNamespace()}
     *   <li>{@link ApolloConfig#getPortalUrl()}
     *   <li>{@link ApolloConfig#getToken()}
     * </ul>
     */
    @Test
    public void testConstructor() {
        ApolloConfig actualApolloConfig = new ApolloConfig();
        actualApolloConfig.setAppId("42");
        actualApolloConfig.setClusterName("Cluster Name");
        actualApolloConfig.setEnv("Env");
        actualApolloConfig.setNamespace("Namespace");
        actualApolloConfig.setPortalUrl("https://example.org/example");
        actualApolloConfig.setToken("ABC123");
        assertEquals("42", actualApolloConfig.getAppId());
        assertEquals("Cluster Name", actualApolloConfig.getClusterName());
        assertEquals("Env", actualApolloConfig.getEnv());
        assertEquals("Namespace", actualApolloConfig.getNamespace());
        assertEquals("https://example.org/example", actualApolloConfig.getPortalUrl());
        assertEquals("ABC123", actualApolloConfig.getToken());
    }
}

