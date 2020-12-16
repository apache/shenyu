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

package org.dromara.soul.metrics.prometheus.impl.collector.support;

import lombok.Data;
import org.dromara.soul.common.utils.GsonUtils;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * JmxCollectorConfigObject.
 *
 * @author David Liu
 */
@Data
public class JmxCollectorConfigObject implements Serializable {
    private static final long serialVersionUID = 1707894072466488748L;
    
    /**
     * The Start delay seconds.
     */
    private Integer startDelaySeconds;
    
    /**
     * The Jmx Server host:port config string.
     */
    private String hostPort;
    
    /**
     * The Jmx url.
     */
    private String jmxUrl;
    
    /**
     * The Username.
     */
    private String username;
    
    /**
     * The Password.
     */
    private String password;
    
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
    private List<String> whitelistObjectNames;
    
    /**
     * The Blacklist object names.
     */
    private List<String> blacklistObjectNames;
    
    /**
     * The Rules.
     */
    private List<Rule> rules;
    
    /**
     * convert current instance object to {@link org.dromara.soul.metrics.prometheus.impl.collector.JmxCollector#JmxCollector(String)} construct param style json string.
     *
     * @return jmx config json
     */
    public String toConfigJson() {
        return GsonUtils.getInstance().toJson(this);
    }
    
    @Data
    public static class Rule implements Serializable {
        
        private static final long serialVersionUID = -8500995065513717543L;
        
        /**
         * The pattern.
         */
        private String pattern;
        
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
        private Double valueFactor;
        
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
        private String type;
        
        /**
         * Labels.
         */
        private Map<String, String> labels;
    }
}
