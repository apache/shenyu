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

package org.apache.shenyu.agent.plugin.metrics.api.config;

import java.util.List;
import java.util.Properties;

/**
 * The type Metrics.
 */
public class Metrics {
    
    private List<Metric> metrics;
    
    /**
     * Gets metrics.
     *
     * @return the metrics
     */
    public List<Metric> getMetrics() {
        return metrics;
    }
    
    /**
     * Sets metrics.
     *
     * @param metrics the metrics
     */
    public void setMetrics(final List<Metric> metrics) {
        this.metrics = metrics;
    }
    
    /**
     * The type Metric.
     */
    public static class Metric {
        
        private String name;
        
        private String type;
    
        private List<String> labelNames;
    
        private Properties props;
    
        private String help;
    
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
         */
        public void setName(final String name) {
            this.name = name;
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
         */
        public void setHelp(final String help) {
            this.help = help;
        }
    
        /**
         * Gets type.
         *
         * @return the type
         */
        public String getType() {
            return type;
        }
    
        /**
         * Sets type.
         *
         * @param type the type
         */
        public void setType(final String type) {
            this.type = type;
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
         */
        public void setLabelNames(final List<String> labelNames) {
            this.labelNames = labelNames;
        }
    
        /**
         * Gets props.
         *
         * @return the props
         */
        public Properties getProps() {
            return props;
        }
    
        /**
         * Sets props.
         *
         * @param props the props
         */
        public void setProps(final Properties props) {
            this.props = props;
        }
    }
}
