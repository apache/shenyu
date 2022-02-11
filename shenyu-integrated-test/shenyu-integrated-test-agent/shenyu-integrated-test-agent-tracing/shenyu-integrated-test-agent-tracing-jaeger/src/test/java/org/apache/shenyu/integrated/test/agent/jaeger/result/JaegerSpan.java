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

package org.apache.shenyu.integrated.test.agent.jaeger.result;

import java.util.List;

/**
 * The type jaeger span.
 */
public class JaegerSpan {

    private String traceID;

    private String spanID;

    private String operationName;

    private List<Reference> references;

    private Long startTime;

    private Long duration;

    private List<Tag> tags;

    /**
     * Get traceID.
     *
     * @return traceID
     */
    public String getTraceID() {
        return traceID;
    }

    /**
     * Get spanID.
     *
     * @return spanID
     */
    public String getSpanID() {
        return spanID;
    }

    /**
     * Get operationName.
     *
     * @return operationName
     */
    public String getOperationName() {
        return operationName;
    }

    /**
     * Get references.
     *
     * @return references
     */
    public List<Reference> getReferences() {
        return references;
    }

    /**
     * Get startTime.
     *
     * @return startTime
     */
    public Long getStartTime() {
        return startTime;
    }

    /**
     * Get duration.
     *
     * @return duration
     */
    public Long getDuration() {
        return duration;
    }

    /**
     * Get tags.
     *
     * @return tags
     */
    public List<Tag> getTags() {
        return tags;
    }

    public static class Reference {

        private String refType;

        private String traceID;

        private String spanID;
    }

    public static class Tag {

        private String key;

        private String type;

        private String value;

        /**
         * Get key.
         *
         * @return key
         */
        public String getKey() {
            return key;
        }

        /**
         * Get type.
         *
         * @return type
         */
        public String getType() {
            return type;
        }

        /**
         * Get value.
         *
         * @return value
         */
        public String getValue() {
            return value;
        }
    }
}
