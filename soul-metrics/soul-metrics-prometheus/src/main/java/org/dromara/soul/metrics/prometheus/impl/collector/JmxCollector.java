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

package org.dromara.soul.metrics.prometheus.impl.collector;

import io.prometheus.client.Collector;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.metrics.config.JmxConfig;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentNavigableMap;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The type Jmx collector.
 */
public class JmxCollector extends Collector implements Collector.Describable {
    
    private static final Logger LOGGER = Logger.getLogger(JmxCollector.class.getName());
    
    private final JmxConfig config;
    
    private final long createTimeNanoSecs = System.nanoTime();
    
    private final JmxMBeanPropertyCache jmxMBeanPropertyCache = new JmxMBeanPropertyCache();
    
    /**
     * Instantiates a new Jmx collector.
     *
     * @param json the json
     * @throws MalformedObjectNameException the malformed object name exception
     */
    public JmxCollector(final String json) throws MalformedObjectNameException {
        config = loadConfig(GsonUtils.getInstance().toObjectMap(json));
    }
    
    private JmxConfig loadConfig(final Map<String, Object> paramMap) throws MalformedObjectNameException {
        JmxConfig cfg = new JmxConfig();
        if (paramMap == null || paramMap.size() == 0) {
            return cfg;
        }
        if (paramMap.containsKey("startDelaySeconds")) {
            try {
                cfg.setStartDelaySeconds(Integer.valueOf(paramMap.get("startDelaySeconds").toString()));
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException("Invalid number provided for startDelaySeconds", e);
            }
        }
        if (paramMap.containsKey("hostPort")) {
            if (paramMap.containsKey("jmxUrl")) {
                throw new IllegalArgumentException("At most one of hostPort and jmxUrl must be provided");
            }
            cfg.setJmxUrl("service:jmx:rmi:///jndi/rmi://" + paramMap.get("hostPort") + "/jmxrmi");
        } else if (paramMap.containsKey("jmxUrl")) {
            cfg.setJmxUrl((String) paramMap.get("jmxUrl"));
        }
        
        if (paramMap.containsKey("username")) {
            cfg.setUsername((String) paramMap.get("username"));
        }
        
        if (paramMap.containsKey("password")) {
            cfg.setPassword((String) paramMap.get("password"));
        }
        
        if (paramMap.containsKey("ssl")) {
            cfg.setSsl((Boolean) paramMap.get("ssl"));
        }
        
        if (paramMap.containsKey("lowercaseOutputName")) {
            cfg.setLowercaseOutputName((Boolean) paramMap.get("lowercaseOutputName"));
        }
        
        if (paramMap.containsKey("lowercaseOutputLabelNames")) {
            cfg.setLowercaseOutputLabelNames((Boolean) paramMap.get("lowercaseOutputLabelNames"));
        }
        
        if (paramMap.containsKey("whitelistObjectNames")) {
            List<String> names = GsonUtils.getInstance().fromList(paramMap.get("whitelistObjectNames").toString(), String.class);
            for (String name : names) {
                cfg.getWhitelistObjectNames().add(new ObjectName(name));
            }
        }
        
        if (paramMap.containsKey("blacklistObjectNames")) {
            List<String> names = GsonUtils.getInstance().fromList(paramMap.get("blacklistObjectNames").toString(), String.class);
            for (String name : names) {
                cfg.getBlacklistObjectNames().add(new ObjectName(name));
            }
        }
        
        if (paramMap.containsKey("rules")) {
            List<Map<String, Object>> configRules = GsonUtils.getInstance().toListMap(paramMap.get("rules").toString());
            for (Map<String, Object> ruleObject : configRules) {
                JmxConfig.Rule rule = new JmxConfig.Rule();
                cfg.getRules().add(rule);
                if (ruleObject.containsKey("pattern")) {
                    rule.setPattern(Pattern.compile("^.*(?:" + ruleObject.get("pattern") + ").*$"));
                }
                if (ruleObject.containsKey("name")) {
                    rule.setName((String) ruleObject.get("name"));
                }
                if (ruleObject.containsKey("value")) {
                    rule.setValue(String.valueOf(ruleObject.get("value")));
                }
                if (ruleObject.containsKey("valueFactor")) {
                    String valueFactor = String.valueOf(ruleObject.get("valueFactor"));
                    try {
                        rule.setValueFactor(Double.valueOf(valueFactor));
                    } catch (NumberFormatException e) {
                        // use default value
                    }
                }
                if (ruleObject.containsKey("attrNameSnakeCase")) {
                    rule.setAttrNameSnakeCase((Boolean) ruleObject.get("attrNameSnakeCase"));
                }
                if (ruleObject.containsKey("type")) {
                    rule.setType(JmxConfig.Type.valueOf(String.valueOf(ruleObject.get("type"))));
                }
                if (ruleObject.containsKey("help")) {
                    rule.setHelp(String.valueOf(ruleObject.get("help")));
                }
                if (ruleObject.containsKey("labels")) {
                    ConcurrentNavigableMap<String, Object> labels = GsonUtils.getInstance().toTreeMap(ruleObject.get("labels").toString());
                    for (Map.Entry<String, Object> entry : labels.entrySet()) {
                        rule.getLabelNames().add(entry.getKey());
                        rule.getLabelValues().add((String) entry.getValue());
                    }
                }
                // Validation.
                if ((rule.getLabelValues() != null || rule.getHelp() != null) && rule.getName() == null) {
                    throw new IllegalArgumentException("Must provide name, if help or labels are given: " + ruleObject);
                }
                if (rule.getName() != null && rule.getPattern() == null) {
                    throw new IllegalArgumentException("Must provide pattern, if name is given: " + ruleObject);
                }
            }
        } else {
            // Default to a single default rule.
            cfg.getRules().add(new JmxConfig.Rule());
        }
        return cfg;
    }
    
    private static String toSnakeAndLowerCase(final String attrName) {
        if (attrName == null || attrName.isEmpty()) {
            return attrName;
        }
        char firstChar = attrName.subSequence(0, 1).charAt(0);
        boolean prevCharIsUpperCaseOrUnderscore = Character.isUpperCase(firstChar) || firstChar == '_';
        StringBuilder resultBuilder = new StringBuilder(attrName.length()).append(Character.toLowerCase(firstChar));
        for (char attrChar : attrName.substring(1).toCharArray()) {
            boolean charIsUpperCase = Character.isUpperCase(attrChar);
            if (!prevCharIsUpperCaseOrUnderscore && charIsUpperCase) {
                resultBuilder.append("_");
            }
            resultBuilder.append(Character.toLowerCase(attrChar));
            prevCharIsUpperCaseOrUnderscore = charIsUpperCase || attrChar == '_';
        }
        return resultBuilder.toString();
    }
    
    private static String safeName(final String name) {
        if (name == null) {
            return null;
        }
        boolean prevCharIsUnderscore = false;
        StringBuilder safeNameBuilder = new StringBuilder(name.length());
        if (!name.isEmpty() && Character.isDigit(name.charAt(0))) {
            // prevent a numeric prefix.
            safeNameBuilder.append("_");
        }
        for (char nameChar : name.toCharArray()) {
            boolean isUnsafeChar = !JmxCollector.isLegalCharacter(nameChar);
            if (isUnsafeChar || nameChar == '_') {
                if (!prevCharIsUnderscore) {
                    safeNameBuilder.append("_");
                    prevCharIsUnderscore = true;
                }
            } else {
                safeNameBuilder.append(nameChar);
                prevCharIsUnderscore = false;
            }
        }
        return safeNameBuilder.toString();
    }
    
    private static boolean isLegalCharacter(final char input) {
        return (input == ':') || (input == '_') || (input >= 'a' && input <= 'z') || (input >= 'A' && input <= 'Z') || (input >= '0' && input <= '9');
    }
    
    @Override
    public List<MetricFamilySamples> collect() {
        Receiver receiver = new Receiver();
        JmxScraper scraper = new JmxScraper(config.getJmxUrl(), config.getUsername(), config.getPassword(), config.isSsl(),
                config.getWhitelistObjectNames(), config.getBlacklistObjectNames(), receiver, jmxMBeanPropertyCache);
        long start = System.nanoTime();
        double error = 0;
        if ((config.getStartDelaySeconds() > 0) && ((start - createTimeNanoSecs) / 1000000000L < config.getStartDelaySeconds())) {
            throw new IllegalStateException("JMXCollector waiting for startDelaySeconds");
        }
        try {
            scraper.doScrape();
        } catch (IOException e) {
            error = 1;
            StringWriter sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            LOGGER.severe("JMX scrape failed: " + sw.toString());
        }
        List<MetricFamilySamples> mfsList = new ArrayList<>(receiver.metricFamilySamplesMap.values());
        List<MetricFamilySamples.Sample> samples = new ArrayList<>();
        samples.add(new MetricFamilySamples.Sample(
                "jmx_scrape_duration_seconds", new ArrayList<>(), new ArrayList<>(), (System.nanoTime() - start) / 1.0E9));
        mfsList.add(new MetricFamilySamples("jmx_scrape_duration_seconds", Type.GAUGE, "Time this JMX scrape took, in seconds.", samples));
        samples = new ArrayList<>();
        samples.add(new MetricFamilySamples.Sample(
                "jmx_scrape_error", new ArrayList<>(), new ArrayList<>(), error));
        mfsList.add(new MetricFamilySamples("jmx_scrape_error", Type.GAUGE, "Non-zero if this scrape failed.", samples));
        return mfsList;
    }
    
    @Override
    public List<MetricFamilySamples> describe() {
        List<MetricFamilySamples> sampleFamilies = new ArrayList<>();
        sampleFamilies.add(new MetricFamilySamples("jmx_scrape_duration_seconds", Type.GAUGE, "Time this JMX scrape took, in seconds.", new ArrayList<>()));
        sampleFamilies.add(new MetricFamilySamples("jmx_scrape_error", Type.GAUGE, "Non-zero if this scrape failed.", new ArrayList<>()));
        return sampleFamilies;
    }
    
    /**
     * The type Receiver.
     */
    class Receiver implements MBeanReceiver {
        
        private static final char SEP = '_';
        
        private final Map<String, MetricFamilySamples> metricFamilySamplesMap = new HashMap<>();
        
        // [] and () are special in regexes, so swtich to <>.
        private String angleBrackets(final String s) {
            return "<" + s.substring(1, s.length() - 1) + ">";
        }
        
        /**
         * Add sample.
         *
         * @param sample the sample
         * @param type   the type
         * @param help   the help
         */
        void addSample(final MetricFamilySamples.Sample sample, final Type type, final String help) {
            MetricFamilySamples mfs = metricFamilySamplesMap.get(sample.name);
            if (mfs == null) {
                // JmxScraper.MBeanReceiver is only called from one thread,
                // so there's no race here.
                mfs = new MetricFamilySamples(sample.name, type, help, new ArrayList<>());
                metricFamilySamplesMap.put(sample.name, mfs);
            }
            mfs.samples.add(sample);
        }
        
        private void defaultExport(final String domain, final Map<String, String> beanProperties,
                                   final LinkedList<String> attrKeys, final String attrName, final String help, final Object value, final Type type) {
            StringBuilder name = new StringBuilder();
            name.append(domain);
            if (beanProperties.size() > 0) {
                name.append(SEP);
                name.append(beanProperties.values().iterator().next());
            }
            for (String k : attrKeys) {
                name.append(SEP);
                name.append(k);
            }
            name.append(SEP);
            name.append(attrName);
            String fullname = safeName(name.toString());
            
            if (config.isLowercaseOutputName()) {
                fullname = fullname.toLowerCase();
            }
            
            List<String> labelNames = new ArrayList<>();
            List<String> labelValues = new ArrayList<>();
            if (beanProperties.size() > 1) {
                Iterator<Map.Entry<String, String>> iter = beanProperties.entrySet().iterator();
                // Skip the first one, it's been used in the name.
                iter.next();
                while (iter.hasNext()) {
                    Map.Entry<String, String> entry = iter.next();
                    String labelName = safeName(entry.getKey());
                    if (config.isLowercaseOutputName()) {
                        labelName = labelName.toLowerCase();
                    }
                    labelNames.add(labelName);
                    labelValues.add(entry.getValue());
                }
            }
            addSample(new MetricFamilySamples.Sample(fullname, labelNames, labelValues, ((Number) value).doubleValue()), type, help);
        }
        
        public void recordBean(final String domain, final Map<String, String> beanProperties, final LinkedList<String> attrKeys,
                               final String attrName, final String attrType, final String attrDescription, final Object beanObject) {
            String beanName = domain + angleBrackets(beanProperties.toString()) + angleBrackets(attrKeys.toString());
            // attrDescription tends not to be useful, so give the fully qualified name too.
            String help = attrDescription + " (" + beanName + attrName + ")";
            String attrNameSnakeCase = toSnakeAndLowerCase(attrName);
            for (JmxConfig.Rule rule : config.getRules()) {
                Matcher matcher = null;
                String matchName = beanName + (rule.isAttrNameSnakeCase() ? attrNameSnakeCase : attrName);
                if (rule.getPattern() != null) {
                    matcher = rule.getPattern().matcher(matchName + ": " + beanObject);
                    if (!matcher.matches()) {
                        continue;
                    }
                }
                Number value;
                Object beanValue = beanObject;
                if (rule.getValue() != null && !rule.getValue().isEmpty()) {
                    assert matcher != null;
                    String val = matcher.replaceAll(rule.getValue());
                    
                    try {
                        beanValue = Double.valueOf(val);
                    } catch (NumberFormatException e) {
                        LOGGER.fine("Unable to parse configured value '" + val + "' to number for bean: " + beanName + attrName + ": " + beanValue);
                        return;
                    }
                }
                if (beanValue instanceof Number) {
                    value = ((Number) beanValue).doubleValue() * rule.getValueFactor();
                } else if (beanValue instanceof Boolean) {
                    value = (Boolean) beanValue ? 1 : 0;
                } else {
                    LOGGER.fine("Ignoring unsupported bean: " + beanName + attrName + ": " + beanValue);
                    return;
                }
                // If there's no name provided, use default export format.
                if (rule.getName() == null) {
                    defaultExport(domain, beanProperties, attrKeys, rule.isAttrNameSnakeCase() ? attrNameSnakeCase : attrName, help, value, Type.valueOf(rule.getType().name()));
                    return;
                }
                
                // Matcher is set below here due to validation in the constructor.
                assert matcher != null;
                String name = safeName(matcher.replaceAll(rule.getName()));
                if (name.isEmpty()) {
                    return;
                }
                if (config.isLowercaseOutputName()) {
                    name = name.toLowerCase();
                }
                // Set the help.
                if (rule.getHelp() != null) {
                    help = matcher.replaceAll(rule.getHelp());
                }
                // Set the labels.
                ArrayList<String> labelNames = new ArrayList<>();
                ArrayList<String> labelValues = new ArrayList<>();
                if (rule.getLabelNames() != null) {
                    for (int i = 0; i < rule.getLabelNames().size(); i++) {
                        final String unsafeLabelName = rule.getLabelNames().get(i);
                        final String labelValReplacement = rule.getLabelValues().get(i);
                        String labelName = safeName(matcher.replaceAll(unsafeLabelName));
                        String labelValue = matcher.replaceAll(labelValReplacement);
                        if (config.isLowercaseOutputName()) {
                            labelName = labelName.toLowerCase();
                        }
                        if (!labelName.isEmpty() && !labelValue.isEmpty()) {
                            labelNames.add(labelName);
                            labelValues.add(labelValue);
                        }
                    }
                }
                // Add to samples.
                LOGGER.fine("add metric sample: " + name + " " + labelNames + " " + labelValues + " " + value.doubleValue());
                addSample(new MetricFamilySamples.Sample(name, labelNames, labelValues, value.doubleValue()), Type.valueOf(rule.getType().name()), help);
                return;
            }
        }
    }
}
