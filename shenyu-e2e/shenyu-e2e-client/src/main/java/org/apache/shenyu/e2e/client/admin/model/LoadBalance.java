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

package org.apache.shenyu.e2e.client.admin.model;

/**
 * Load balance enum.
 */
public enum LoadBalance {

    /**
     * Hash load balance enum.
     */
    HASH("hash"),

    /**
     * Random load balance enum.
     */
    RANDOM("random"),

    /**
     * Round robin load balance enum.
     */
    ROUND_ROBIN("roundRobin"),

    /**
     * least activity load balance enum.
     */
    LEAST_ACTIVITY("leastActive"),

    /**
     * pick of 2 choices load balance enum.
     */
    P2C("p2c"),

    /**
     * shortest response load balance enum.
     */
    SHORTEST_RESPONSE("shortestResponse");

    private final String name;

    LoadBalance(String name) {
        this.name = name;
    }

    /**
     * get name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

}
