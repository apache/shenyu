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

import javax.management.ObjectName;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.StringJoiner;
import java.util.regex.Pattern;

/**
 * The type Jmx config.
 */
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
     * Gets start delay seconds.
     *
     * @return the start delay seconds
     */
    public Integer getStartDelaySeconds() {
        return startDelaySeconds;
    }

    /**
     * Sets start delay seconds.
     *
     * @param startDelaySeconds the start delay seconds
     * @return the start delay seconds
     */
    public JmxConfig setStartDelaySeconds(final Integer startDelaySeconds) {
        this.startDelaySeconds = startDelaySeconds;
        return this;
    }

    /**
     * Gets jmx url.
     *
     * @return the jmx url
     */
    public String getJmxUrl() {
        return jmxUrl;
    }

    /**
     * Sets jmx url.
     *
     * @param jmxUrl the jmx url
     * @return the jmx url
     */
    public JmxConfig setJmxUrl(final String jmxUrl) {
        this.jmxUrl = jmxUrl;
        return this;
    }

    /**
     * Gets username.
     *
     * @return the username
     */
    public String getUsername() {
        return username;
    }

    /**
     * Sets username.
     *
     * @param username the username
     * @return the username
     */
    public JmxConfig setUsername(final String username) {
        this.username = username;
        return this;
    }

    /**
     * Gets password.
     *
     * @return the password
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets password.
     *
     * @param password the password
     * @return the password
     */
    public JmxConfig setPassword(final String password) {
        this.password = password;
        return this;
    }

    /**
     * Is ssl boolean.
     *
     * @return the boolean
     */
    public boolean isSsl() {
        return ssl;
    }

    /**
     * Sets ssl.
     *
     * @param ssl the ssl
     * @return the ssl
     */
    public JmxConfig setSsl(final boolean ssl) {
        this.ssl = ssl;
        return this;
    }

    /**
     * Is lowercase output name boolean.
     *
     * @return the boolean
     */
    public boolean isLowercaseOutputName() {
        return lowercaseOutputName;
    }

    /**
     * Sets lowercase output name.
     *
     * @param lowercaseOutputName the lowercase output name
     * @return the lowercase output name
     */
    public JmxConfig setLowercaseOutputName(final boolean lowercaseOutputName) {
        this.lowercaseOutputName = lowercaseOutputName;
        return this;
    }

    /**
     * Is lowercase output label names boolean.
     *
     * @return the boolean
     */
    public boolean isLowercaseOutputLabelNames() {
        return lowercaseOutputLabelNames;
    }

    /**
     * Sets lowercase output label names.
     *
     * @param lowercaseOutputLabelNames the lowercase output label names
     * @return the lowercase output label names
     */
    public JmxConfig setLowercaseOutputLabelNames(final boolean lowercaseOutputLabelNames) {
        this.lowercaseOutputLabelNames = lowercaseOutputLabelNames;
        return this;
    }

    /**
     * Gets whitelist object names.
     *
     * @return the whitelist object names
     */
    public List<ObjectName> getWhitelistObjectNames() {
        return whitelistObjectNames;
    }

    /**
     * Sets whitelist object names.
     *
     * @param whitelistObjectNames the whitelist object names
     * @return the whitelist object names
     */
    public JmxConfig setWhitelistObjectNames(final List<ObjectName> whitelistObjectNames) {
        this.whitelistObjectNames = whitelistObjectNames;
        return this;
    }

    /**
     * Gets blacklist object names.
     *
     * @return the blacklist object names
     */
    public List<ObjectName> getBlacklistObjectNames() {
        return blacklistObjectNames;
    }

    /**
     * Sets blacklist object names.
     *
     * @param blacklistObjectNames the blacklist object names
     * @return the blacklist object names
     */
    public JmxConfig setBlacklistObjectNames(final List<ObjectName> blacklistObjectNames) {
        this.blacklistObjectNames = blacklistObjectNames;
        return this;
    }

    /**
     * Gets rules.
     *
     * @return the rules
     */
    public List<Rule> getRules() {
        return rules;
    }

    /**
     * Sets rules.
     *
     * @param rules the rules
     * @return the rules
     */
    public JmxConfig setRules(final List<Rule> rules) {
        this.rules = rules;
        return this;
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", JmxConfig.class.getSimpleName() + "(", ")")
                .add("startDelaySeconds=" + startDelaySeconds)
                .add("jmxUrl=" + jmxUrl)
                .add("username=" + username)
                .add("password=" + password)
                .add("ssl=" + ssl)
                .add("lowercaseOutputName=" + lowercaseOutputName)
                .add("lowercaseOutputLabelNames=" + lowercaseOutputLabelNames)
                .add("whitelistObjectNames=" + whitelistObjectNames)
                .add("blacklistObjectNames=" + blacklistObjectNames)
                .add("rules=" + rules)
                .toString();
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
        UNTYPED
    }

    /**
     * The type Rule.
     */
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

        /**
         * Gets pattern.
         *
         * @return the pattern
         */
        public Pattern getPattern() {
            return pattern;
        }

        /**
         * Sets pattern.
         *
         * @param pattern the pattern
         * @return the pattern
         */
        public Rule setPattern(final Pattern pattern) {
            this.pattern = pattern;
            return this;
        }

        /**
         * Gets name.
         *
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * Sets name.
         *
         * @param name the name
         * @return the name
         */
        public Rule setName(final String name) {
            this.name = name;
            return this;
        }

        /**
         * Gets value.
         *
         * @return the value
         */
        public String getValue() {
            return value;
        }

        /**
         * Sets value.
         *
         * @param value the value
         * @return the value
         */
        public Rule setValue(final String value) {
            this.value = value;
            return this;
        }

        /**
         * Gets value factor.
         *
         * @return the value factor
         */
        public Double getValueFactor() {
            return valueFactor;
        }

        /**
         * Sets value factor.
         *
         * @param valueFactor the value factor
         * @return the value factor
         */
        public Rule setValueFactor(final Double valueFactor) {
            this.valueFactor = valueFactor;
            return this;
        }

        /**
         * Gets help.
         *
         * @return the help
         */
        public String getHelp() {
            return help;
        }

        /**
         * Sets help.
         *
         * @param help the help
         * @return the help
         */
        public Rule setHelp(final String help) {
            this.help = help;
            return this;
        }

        /**
         * Is attr name snake case boolean.
         *
         * @return the boolean
         */
        public boolean isAttrNameSnakeCase() {
            return attrNameSnakeCase;
        }

        /**
         * Sets attr name snake case.
         *
         * @param attrNameSnakeCase the attr name snake case
         * @return the attr name snake case
         */
        public Rule setAttrNameSnakeCase(final boolean attrNameSnakeCase) {
            this.attrNameSnakeCase = attrNameSnakeCase;
            return this;
        }

        /**
         * Gets type.
         *
         * @return the type
         */
        public Type getType() {
            return type;
        }

        /**
         * Sets type.
         *
         * @param type the type
         * @return the type
         */
        public Rule setType(final Type type) {
            this.type = type;
            return this;
        }

        /**
         * Gets label names.
         *
         * @return the label names
         */
        public List<String> getLabelNames() {
            return labelNames;
        }

        /**
         * Sets label names.
         *
         * @param labelNames the label names
         * @return the label names
         */
        public Rule setLabelNames(final List<String> labelNames) {
            this.labelNames = labelNames;
            return this;
        }

        /**
         * Gets label values.
         *
         * @return the label values
         */
        public List<String> getLabelValues() {
            return labelValues;
        }

        /**
         * Sets label values.
         *
         * @param labelValues the label values
         * @return the label values
         */
        public Rule setLabelValues(final List<String> labelValues) {
            this.labelValues = labelValues;
            return this;
        }

        @Override
        public String toString() {
            return new StringJoiner(", ", JmxConfig.class.getSimpleName() + "." + Rule.class.getSimpleName() + "(", ")")
                    .add("pattern=" + pattern)
                    .add("name=" + name)
                    .add("value=" + value)
                    .add("valueFactor=" + valueFactor)
                    .add("help=" + help)
                    .add("attrNameSnakeCase=" + attrNameSnakeCase)
                    .add("type=" + type)
                    .add("labelNames=" + labelNames)
                    .add("labelValues=" + labelValues)
                    .toString();
        }
    }

}
