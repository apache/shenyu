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

package org.apache.shenyu.protocol.mqtt;

import java.util.Objects;

/**
 * MQTT topic filter matching per MQTT-4.7.
 *
 * <p>+ matches exactly one topic level.</p>
 *
 * <p># matches any number of subsequent levels (must appear at the end of the filter).</p>
 */
public final class TopicMatcher {

    private TopicMatcher() {
    }

    /**
     * Check whether a topic filter matches a topic name.
     *
     * @param filter the subscription topic filter (may contain + and # wildcards)
     * @param topic  the published topic name (no wildcards)
     * @return true if the filter matches the topic
     */
    public static boolean matches(final String filter, final String topic) {
        if (Objects.isNull(filter) || Objects.isNull(topic)) {
            return false;
        }

        // $ topics must not be matched by wildcards at the first level
        if (topic.startsWith("$") && filter.length() > 0 && (filter.charAt(0) == '+' || filter.charAt(0) == '#')) {
            return false;
        }

        String[] filterLevels = filter.split("/", -1);
        String[] topicLevels = topic.split("/", -1);

        int filterLen = filterLevels.length;
        int topicLen = topicLevels.length;

        for (int i = 0; i < filterLen; i++) {
            String f = filterLevels[i];

            if ("#".equals(f)) {
                // MQTT-4.7.1-2: # matches any number of levels including the parent level
                return i == filterLen - 1;
            }

            if (i >= topicLen) {
                return false;
            }

            if (!"+".equals(f) && !f.equals(topicLevels[i])) {
                return false;
            }
        }

        return filterLen == topicLen;
    }
}
