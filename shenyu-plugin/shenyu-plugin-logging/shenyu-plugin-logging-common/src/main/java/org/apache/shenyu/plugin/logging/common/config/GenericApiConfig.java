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

package org.apache.shenyu.plugin.logging.common.config;

/**
 * GenericApiConfig.
 */
public class GenericApiConfig {

    /**
     * 0 means never sample, 1 means always sample. Minimum probability is 0.01, or 1% of logging
     */
    private String sampleRate;

    /**
     * This topic is useful if you use message queuing to collect logs.
     */
    private String topic;

    /**
     * get sample rate.
     *
     * @return sample rate
     */
    public String getSampleRate() {
        return sampleRate;
    }

    /**
     * set sample rate.
     *
     * @param sampleRate sample rate
     */
    public void setSampleRate(final String sampleRate) {
        this.sampleRate = sampleRate;
    }

    /**
     * get mq topic.
     *
     * @return mq topic
     */
    public String getTopic() {
        return topic;
    }

    /**
     * set  mq topic.
     *
     * @param topic mq topic
     */
    public void setTopic(final String topic) {
        this.topic = topic;
    }
}
