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

package org.apache.shenyu.sync.data.apollo.config;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ApolloConfigTest {

    /**
     * Methods under test.
     *
     * <ul>
     *   <li>default or parameterless constructor of {@link ApolloConfig}
     *   <li>{@link ApolloConfig#setAccessKey(String)}
     *   <li>{@link ApolloConfig#setAppId(String)}
     *   <li>{@link ApolloConfig#setClusterName(String)}
     *   <li>{@link ApolloConfig#setEnv(String)}
     *   <li>{@link ApolloConfig#setMeta(String)}
     *   <li>{@link ApolloConfig#setNamespace(String)}
     *   <li>{@link ApolloConfig#getAccessKey()}
     *   <li>{@link ApolloConfig#getAppId()}
     *   <li>{@link ApolloConfig#getClusterName()}
     *   <li>{@link ApolloConfig#getEnv()}
     *   <li>{@link ApolloConfig#getMeta()}
     *   <li>{@link ApolloConfig#getNamespace()}
     * </ul>
     */
    @Test
    public void testConstructor() {
        ApolloConfig actualApolloConfig = new ApolloConfig();
        actualApolloConfig.setAccessKey("EXAMPLEakiAIOSFODNN7");
        actualApolloConfig.setAppId("42");
        actualApolloConfig.setClusterName("Cluster Name");
        actualApolloConfig.setEnv("Env");
        actualApolloConfig.setMeta("Meta");
        actualApolloConfig.setNamespace("Namespace");
        assertEquals("EXAMPLEakiAIOSFODNN7", actualApolloConfig.getAccessKey());
        assertEquals("42", actualApolloConfig.getAppId());
        assertEquals("Cluster Name", actualApolloConfig.getClusterName());
        assertEquals("Env", actualApolloConfig.getEnv());
        assertEquals("Meta", actualApolloConfig.getMeta());
        assertEquals("Namespace", actualApolloConfig.getNamespace());
    }
}

