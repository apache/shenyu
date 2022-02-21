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

package org.apache.shenyu.integrated.test.agent.zipkin.result;

import java.util.Map;

/**
 * The type zipkin span.
 */
public class ZipkinSpan {

    private String traceId;

    private String id;

    private String name;

    private Long timestamp;

    private Long duration;

    private Map<String, String> localEndpoint;

    private Map<String, String> tags;

    /**
     * get trace id.
     *
     * @return trace id
     */
    public String getTraceId() {
        return traceId;
    }

    /**
     * set trace id.
     *
     * @param traceId the trace id
     */
    public void setTraceId(final String traceId) {
        this.traceId = traceId;
    }

    /**
     * get id.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get name.
     *
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get time stamp.
     *
     * @return timestamp
     */
    public Long getTimestamp() {
        return timestamp;
    }

    /**
     * set timestamp.
     *
     * @param timestamp timestamp
     */
    public void setTimestamp(final Long timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * get duration.
     *
     * @return duration
     */
    public Long getDuration() {
        return duration;
    }

    /**
     * set duration.
     *
     * @param duration duration
     */
    public void setDuration(final Long duration) {
        this.duration = duration;
    }

    /**
     * get local end point.
     *
     * @return local end point
     */
    public Map<String, String> getLocalEndpoint() {
        return localEndpoint;
    }

    /**
     * set local end point.
     *
     * @param localEndpoint local end point
     */
    public void setLocalEndpoint(final Map<String, String> localEndpoint) {
        this.localEndpoint = localEndpoint;
    }

    /**
     * get tags.
     *
     * @return tags
     */
    public Map<String, String> getTags() {
        return tags;
    }

    /**
     * set tags.
     *
     * @param tags tags
     */
    public void setTags(final Map<String, String> tags) {
        this.tags = tags;
    }
}
