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

package org.apache.shenyu.common.dto.convert.selector;

/**
 * DiscoveryUpstream.
 */
public class DiscoveryUpstream extends CommonUpstream {

    private int weight;

    private String prop;

    /**
     * getWeight.
     *
     * @return weight
     */
    public int getWeight() {
        return weight;
    }

    /**
     * setWeight.
     *
     * @param weight weight
     */
    public void setWeight(final int weight) {
        this.weight = weight;
    }

    /**
     * getProp.
     *
     * @return prop
     */
    public String getProp() {
        return prop;
    }

    /**
     * setProp.
     *
     * @param prop prop
     */
    public void setProp(final String prop) {
        this.prop = prop;
    }
}
