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

package org.apache.shenyu.examples.sdk.grpc.consumer.dto;

import java.util.List;

public class EchoResponse {

    private String message;

    private List<Trace> traces;

    /**
     * getMessage.
     *
     * @return message
     */
    public String getMessage() {
        return message;
    }

    /**
     * setMessage.
     *
     * @param message message
     */
    public void setMessage(final String message) {
        this.message = message;
    }

    /**
     * getTraces.
     *
     * @return traces
     */
    public List<Trace> getTraces() {
        return traces;
    }

    /**
     * setTraces.
     * @param traces traces
     */
    public void setTraces(final List<Trace> traces) {
        this.traces = traces;
    }

    public static class Trace {

        private String host;

        /**
         * getHost.
         *
         * @return host
         */
        public String getHost() {
            return host;
        }

        /**
         * setHost.
         *
         * @param host host
         */
        public void setHost(final String host) {
            this.host = host;
        }
    }
}
