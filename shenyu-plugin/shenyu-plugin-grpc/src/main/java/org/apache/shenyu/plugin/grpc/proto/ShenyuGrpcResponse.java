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

package org.apache.shenyu.plugin.grpc.proto;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.List;

/**
 * ShenyuGrpcResponse.
 */
public class ShenyuGrpcResponse implements Serializable {

    private static final long serialVersionUID = 4182753303732523014L;

    private List<String> results;

    /**
     * Instantiates a new Shenyu grpc response.
     */
    public ShenyuGrpcResponse() {
        this.results = new ArrayList<>();
    }

    /**
     * Gets results.
     *
     * @return the results
     */
    public List<String> getResults() {
        return results;
    }

    /**
     * Sets results.
     *
     * @param results the results
     */
    public void setResults(final List<String> results) {
        this.results = results;
    }
}
