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

package org.apache.shenyu.metrics.config;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import javax.management.ObjectName;
import lombok.Data;

/**
 * The type Jmx config.
 */
@Data
public class JmxConfig implements Serializable {

    private static final long serialVersionUID = 4511567901029116022L;

    /**
     * The Start delay seconds.
     */
    private Integer startDelaySeconds = 0;

    /**
     * The Jmx url.
     */
    private String jmxUrl = "";

    /**
     * The Username.
     */
    private String username = "";

    /**
     * The Password.
     */
    private String password = "";

    /**
     * The Ssl.
     */
    private boolean ssl;

    /**
     * The Lowercase output name.
     */
    private boolean lowercaseOutputName;

    /**
     * The Lowercase output label names.
     */
    private boolean lowercaseOutputLabelNames;

    /**
     * The Whitelist object names.
     */
    private List<ObjectName> whitelistObjectNames = new ArrayList<>();

    /**
     * The Blacklist object names.
     */
    private List<ObjectName> blacklistObjectNames = new ArrayList<>();

    /**
     * The Rules.
     */
    private List<Rule> rules = new ArrayList<>();

    /**
     * The type Rule.
     */
    @Data
    public static class Rule {

        private Pattern pattern;

        /**
         * The Name.
         */
        private String name;

        /**
         * The Value.
         */
        private String value;

        /**
         * The Value factor.
         */
        private Double valueFactor = 1.0;

        /**
         * The Help.
         */
        private String help;

        /**
         * The Attr name snake case.
         */
        private boolean attrNameSnakeCase;

        /**
         * The Type.
         */
        private Type type = Type.UNTYPED;

        /**
         * The Label names.
         */
        private List<String> labelNames = new ArrayList<>();

        /**
         * The Label values.
         */
        private List<String> labelValues = new ArrayList<>();
    }

    /**
     * The enum Type.
     */
    public enum Type {
        /**
         * Counter type.
         */
        COUNTER,
        /**
         * Gauge type.
         */
        GAUGE,
        /**
         * Summary type.
         */
        SUMMARY,
        /**
         * Histogram type.
         */
        HISTOGRAM,
        /**
         * Untyped type.
         */
        UNTYPED,
    }
}
