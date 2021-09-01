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

package org.apache.shenyu.common.config;

import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type shenyu config.
 */
public class ShenyuConfig {

    private SwitchConfig switchConfig = new SwitchConfig();
    
    private FileConfig file = new FileConfig();
    
    private ExcludePath exclude = new ExcludePath();
    
    private ExtPlugin extPlugin = new ExtPlugin();
    
    private Scheduler scheduler = new Scheduler();
    
    private UpstreamCheck upstreamCheck = new UpstreamCheck();

    private CrossFilterConfig cross = new CrossFilterConfig();
    
    /**
     * Gets switch config.
     *
     * @return the switch config
     */
    public SwitchConfig getSwitchConfig() {
        return switchConfig;
    }
    
    /**
     * Sets switch config.
     *
     * @param switchConfig the switch config
     */
    public void setSwitchConfig(final SwitchConfig switchConfig) {
        this.switchConfig = switchConfig;
    }
    
    /**
     * Gets scheduler.
     *
     * @return the scheduler
     */
    public Scheduler getScheduler() {
        return scheduler;
    }
    
    /**
     * Sets scheduler.
     *
     * @param scheduler the scheduler
     */
    public void setScheduler(final Scheduler scheduler) {
        this.scheduler = scheduler;
    }
    
    /**
     * Gets ext plugin.
     *
     * @return the ext plugin
     */
    public ExtPlugin getExtPlugin() {
        return extPlugin;
    }
    
    /**
     * Sets ext plugin.
     *
     * @param extPlugin the ext plugin
     */
    public void setExtPlugin(final ExtPlugin extPlugin) {
        this.extPlugin = extPlugin;
    }
    
    /**
     * Gets file.
     *
     * @return the file
     */
    public FileConfig getFile() {
        return file;
    }
    
    /**
     * Sets file.
     *
     * @param file the file
     */
    public void setFile(final FileConfig file) {
        this.file = file;
    }
    
    /**
     * Gets exclude.
     *
     * @return the exclude
     */
    public ExcludePath getExclude() {
        return exclude;
    }
    
    /**
     * Sets exclude.
     *
     * @param exclude the exclude
     */
    public void setExclude(final ExcludePath exclude) {
        this.exclude = exclude;
    }
    
    /**
     * Gets upstream check.
     *
     * @return the upstream check
     */
    public UpstreamCheck getUpstreamCheck() {
        return upstreamCheck;
    }
    
    /**
     * Sets upstream check.
     *
     * @param upstreamCheck the upstream check
     */
    public void setUpstreamCheck(final UpstreamCheck upstreamCheck) {
        this.upstreamCheck = upstreamCheck;
    }
    
    /**
     * Gets cross.
     *
     * @return the cross
     */
    public CrossFilterConfig getCross() {
        return cross;
    }
    
    /**
     * Sets cross.
     *
     * @param cross the cross
     */
    public void setCross(final CrossFilterConfig cross) {
        this.cross = cross;
    }
    
    /**
     * The type Scheduler.
     */
    public static class Scheduler {
    
        private Boolean enabled = false;
    
        private String type = "fixed";
    
        private Integer threads = Math.max((Runtime.getRuntime().availableProcessors() << 1) + 1, 16);
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public Boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final Boolean enabled) {
            this.enabled = enabled;
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
         * Gets threads.
         *
         * @return the threads
         */
        public Integer getThreads() {
            return threads;
        }
    
        /**
         * Sets threads.
         *
         * @param threads the threads
         */
        public void setThreads(final Integer threads) {
            this.threads = threads;
        }
        
    }
    
    /**
     * The type Ext plugin.
     */
    public static class ExtPlugin {
        
        private String path;
        
        private Boolean enabled = true;
    
        private Integer threads = 1;
        
        private Integer scheduleTime = 300;
    
        private Integer scheduleDelay = 30;
    
        /**
         * Gets path.
         *
         * @return the path
         */
        public String getPath() {
            return path;
        }
    
        /**
         * Sets path.
         *
         * @param path the path
         */
        public void setPath(final String path) {
            this.path = path;
        }
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public Boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final Boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * Gets threads.
         *
         * @return the threads
         */
        public Integer getThreads() {
            return threads;
        }
    
        /**
         * Sets threads.
         *
         * @param threads the threads
         */
        public void setThreads(final Integer threads) {
            this.threads = threads;
        }
    
    
        /**
         * Gets schedule time.
         *
         * @return the schedule time
         */
        public Integer getScheduleTime() {
            return scheduleTime;
        }
    
        /**
         * Sets schedule time.
         *
         * @param scheduleTime the schedule time
         */
        public void setScheduleTime(final Integer scheduleTime) {
            this.scheduleTime = scheduleTime;
        }
    
        /**
         * Gets schedule delay.
         *
         * @return the schedule delay
         */
        public Integer getScheduleDelay() {
            return scheduleDelay;
        }
    
        /**
         * Sets schedule delay.
         *
         * @param scheduleDelay the schedule delay
         */
        public void setScheduleDelay(final Integer scheduleDelay) {
            this.scheduleDelay = scheduleDelay;
        }
    }
    
    /**
     * The type Exclude path.
     */
    public static class ExcludePath {
    
        private Boolean enabled = false;
    
        private List<String> paths = new ArrayList<>();
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public Boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final Boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * Sets paths.
         *
         * @param paths the paths
         */
        public void setPaths(final List<String> paths) {
            this.paths = paths;
        }
    
        /**
         * get paths.
         *
         * @return paths paths
         */
        public List<String> getPaths() {
            return paths;
        }
    }
    
    /**
     * The type File config.
     */
    public static class FileConfig {
    
        private Boolean enabled = true;
    
        private Integer maxSize = 10;
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public Boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final Boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * Gets file max size.
         *
         * @return the file max size
         */
        public Integer getMaxSize() {
            return maxSize;
        }
    
        /**
         * Sets file max size.
         *
         * @param maxSize the file max size
         */
        public void setMaxSize(final Integer maxSize) {
            this.maxSize = maxSize;
        }
    }
    
    /**
     * The type Switch config.
     */
    public static class SwitchConfig {
        
        private Boolean local = true;
    
        /**
         * Gets local.
         *
         * @return the local
         */
        public Boolean getLocal() {
            return local;
        }
    
        /**
         * Sets local.
         *
         * @param local the local
         */
        public void setLocal(final Boolean local) {
            this.local = local;
        }
        
    }
    
    /**
     * The type Upstream check.
     */
    public static class UpstreamCheck {
    
        private Boolean enabled = false;
        
        private Integer timeout = 3000;
        
        private Integer healthyThreshold = 1;
    
        private Integer unhealthyThreshold = 1;
        
        private Integer interval = 5000;
        
        private Boolean printEnabled = true;
        
        private Integer printInterval = 60000;
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public Boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final Boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * Gets timeout.
         *
         * @return the timeout
         */
        public Integer getTimeout() {
            return timeout;
        }
    
        /**
         * Sets timeout.
         *
         * @param timeout the timeout
         */
        public void setTimeout(final Integer timeout) {
            this.timeout = timeout;
        }
    
        /**
         * Gets healthy threshold.
         *
         * @return the healthy threshold
         */
        public Integer getHealthyThreshold() {
            return healthyThreshold;
        }
    
        /**
         * Sets healthy threshold.
         *
         * @param healthyThreshold the healthy threshold
         */
        public void setHealthyThreshold(final Integer healthyThreshold) {
            this.healthyThreshold = healthyThreshold;
        }
    
        /**
         * Gets unhealthy threshold.
         *
         * @return the unhealthy threshold
         */
        public Integer getUnhealthyThreshold() {
            return unhealthyThreshold;
        }
    
        /**
         * Sets unhealthy threshold.
         *
         * @param unhealthyThreshold the unhealthy threshold
         */
        public void setUnhealthyThreshold(final Integer unhealthyThreshold) {
            this.unhealthyThreshold = unhealthyThreshold;
        }
    
        /**
         * Gets interval.
         *
         * @return the interval
         */
        public Integer getInterval() {
            return interval;
        }
    
        /**
         * Sets interval.
         *
         * @param interval the interval
         */
        public void setInterval(final Integer interval) {
            this.interval = interval;
        }
    
        /**
         * Gets print enabled.
         *
         * @return the print enabled
         */
        public Boolean getPrintEnabled() {
            return printEnabled;
        }
    
        /**
         * Sets print enabled.
         *
         * @param printEnabled the print enabled
         */
        public void setPrintEnabled(final Boolean printEnabled) {
            this.printEnabled = printEnabled;
        }
    
        /**
         * Gets print interval.
         *
         * @return the print interval
         */
        public Integer getPrintInterval() {
            return printInterval;
        }
    
        /**
         * Sets print interval.
         *
         * @param printInterval the print interval
         */
        public void setPrintInterval(final Integer printInterval) {
            this.printInterval = printInterval;
        }
    }
    
    /**
     * The Cross Filter Config.
     */
    public static class CrossFilterConfig {

        private static final Set<String> DEFAULT_ALLOWED_HEADERS;

        static {
            DEFAULT_ALLOWED_HEADERS = new HashSet<String>() {
                {
                    add("x-requested-with");
                    add("authorization");
                    add("Content-Type");
                    add("Authorization");
                    add("credential");
                    add("X-XSRF-TOKEN");
                    add("token");
                    add("username");
                    add("client");
                }
            };
        }
    
        private Boolean enabled = true;

        /**
         * Comma-separated of “header”.
         */
        private String allowedHeaders = "";

        /**
         * Comma-separated of “method”.
         */
        private String allowedMethods = "*";

        private String allowedOrigin = "*";

        private String allowedExpose = "*";

        private String maxAge = "18000";

        private boolean allowCredentials = true;

        /**
         * wrapper the headers.
         *
         * @param headers headers
         * @return wrapped headers
         */
        private String wrapperHeaders(final String headers) {
            final Set<String> headerSet = DEFAULT_ALLOWED_HEADERS;
            if (StringUtils.hasText(headers)) {
                headerSet.addAll(Stream.of(headers.split(",")).collect(Collectors.toSet()));
            }
            return String.join(",", headerSet);
        }
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public Boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final Boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * Gets the value of allowedHeaders.
         *
         * @return the value of allowedHeaders
         */
        public String getAllowedHeaders() {
            return allowedHeaders = wrapperHeaders(allowedHeaders);
        }
    
        /**
         * Sets the allowedHeaders.
         *
         * @param allowedHeaders allowedHeaders
         */
        public void setAllowedHeaders(final String allowedHeaders) {
            this.allowedHeaders = wrapperHeaders(allowedHeaders);
        }
    
        /**
         * Gets the value of allowedMethods.
         *
         * @return the value of allowedMethods
         */
        public String getAllowedMethods() {
            return allowedMethods;
        }
    
        /**
         * Sets the allowedMethods.
         *
         * @param allowedMethods allowedMethods
         */
        public void setAllowedMethods(final String allowedMethods) {
            this.allowedMethods = allowedMethods;
        }
    
        /**
         * Gets the value of allowedOrigin.
         *
         * @return the value of allowedOrigin
         */
        public String getAllowedOrigin() {
            return allowedOrigin;
        }
    
        /**
         * Sets the allowedOrigin.
         *
         * @param allowedOrigin allowedOrigin
         */
        public void setAllowedOrigin(final String allowedOrigin) {
            this.allowedOrigin = allowedOrigin;
        }
    
        /**
         * Gets the value of allowedExpose.
         *
         * @return the value of allowedExpose
         */
        public String getAllowedExpose() {
            return allowedExpose;
        }
    
        /**
         * Sets the allowedExpose.
         *
         * @param allowedExpose allowedExpose
         */
        public void setAllowedExpose(final String allowedExpose) {
            this.allowedExpose = allowedExpose;
        }
    
        /**
         * Gets the value of maxAge.
         *
         * @return the value of maxAge
         */
        public String getMaxAge() {
            return maxAge;
        }
    
        /**
         * Sets the maxAge.
         *
         * @param maxAge maxAge
         */
        public void setMaxAge(final String maxAge) {
            this.maxAge = maxAge;
        }
    
        /**
         * Gets the value of allowCredentials.
         *
         * @return the value of allowCredentials
         */
        public boolean isAllowCredentials() {
            return allowCredentials;
        }
    
        /**
         * Sets the allowCredentials.
         *
         * @param allowCredentials allowCredentials
         */
        public void setAllowCredentials(final boolean allowCredentials) {
            this.allowCredentials = allowCredentials;
        }
    }
}
