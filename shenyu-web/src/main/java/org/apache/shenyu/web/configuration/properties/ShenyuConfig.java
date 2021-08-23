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

package org.apache.shenyu.web.configuration.properties;

/**
 * The type shenyu config.
 */
public class ShenyuConfig {

    private Integer filterTime = 10;

    private Boolean filterTimeEnable = false;

    private Integer upstreamScheduledTime = 30;

    private Integer fileMaxSize = 10;
    
    private ExtPlugin extPlugin = new ExtPlugin();
    
    private Scheduler scheduler = new Scheduler();
    
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
     * get filterTime.
     *
     * @return filterTime filter time
     */
    public Integer getFilterTime() {
        return filterTime;
    }
    
    /**
     * set filterTime.
     *
     * @param filterTime filterTime.
     */
    public void setFilterTime(final Integer filterTime) {
        this.filterTime = filterTime;
    }
    
    /**
     * get filterTimeEnable.
     *
     * @return filterTimeEnable filter time enable
     */
    public Boolean getFilterTimeEnable() {
        return filterTimeEnable;
    }
    
    /**
     * set filterTimeEnable.
     *
     * @param filterTimeEnable filterTimeEnable.
     */
    public void setFilterTimeEnable(final Boolean filterTimeEnable) {
        this.filterTimeEnable = filterTimeEnable;
    }
    
    /**
     * get upstreamScheduledTime.
     *
     * @return upstreamScheduledTime upstream scheduled time
     */
    public Integer getUpstreamScheduledTime() {
        return upstreamScheduledTime;
    }
    
    /**
     * set upstreamScheduledTime.
     *
     * @param upstreamScheduledTime upstreamScheduledTime.
     */
    public void setUpstreamScheduledTime(final Integer upstreamScheduledTime) {
        this.upstreamScheduledTime = upstreamScheduledTime;
    }
    
    /**
     * get fileMaxSize.
     *
     * @return fileMaxSize file max size
     */
    public Integer getFileMaxSize() {
        return fileMaxSize;
    }
    
    /**
     * set fileMaxSize.
     *
     * @param fileMaxSize fileMaxSize.
     */
    public void setFileMaxSize(final Integer fileMaxSize) {
        this.fileMaxSize = fileMaxSize;
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
        
        private Boolean enabled = true;
    
        private Integer threads = 1;
        
        private Integer scheduleTime = 300;
    
        private Integer scheduleDelay = 30;
    
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
}
