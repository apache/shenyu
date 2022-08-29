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

import org.apache.shenyu.common.concurrent.MemoryLimitCalculator;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
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
    
    private Health health = new Health();

    private FallbackPath fallback = new FallbackPath();
    
    private ExtPlugin extPlugin = new ExtPlugin();

    private MatchCache matchCache = new MatchCache();
    
    private Scheduler scheduler = new Scheduler();
    
    private UpstreamCheck upstreamCheck = new UpstreamCheck();

    private CrossFilterConfig cross = new CrossFilterConfig();
    
    private InstanceConfig instance = new InstanceConfig();

    private RibbonConfig ribbon = new RibbonConfig();
    
    private Local local = new Local();

    private WebsocketConfig websocket = new WebsocketConfig();

    private SharedPool sharedPool = new SharedPool();
    
    private MetricsConfig metrics = new MetricsConfig();
    
    /**
     * Gets health.
     *
     * @return the health
     */
    public Health getHealth() {
        return health;
    }
    
    /**
     * Sets health.
     *
     * @param health the health
     */
    public void setHealth(final Health health) {
        this.health = health;
    }
    
    /**
     * Gets metrics.
     *
     * @return the metrics
     */
    public MetricsConfig getMetrics() {
        return metrics;
    }
    
    /**
     * Sets metrics.
     *
     * @param metrics the metrics
     */
    public void setMetrics(final MetricsConfig metrics) {
        this.metrics = metrics;
    }
    
    /**
     * Gets the shared thread pool config.
     *
     * @return the shared thread pool config
     */
    public SharedPool getSharedPool() {
        return sharedPool;
    }
    
    /**
     * Sets the shared thread pool config.
     *
     * @param sharedPool the shared thread pool config
     */
    public void setSharedPool(final SharedPool sharedPool) {
        this.sharedPool = sharedPool;
    }
    
    /**
     * Gets the local config.
     *
     * @return the local config
     */
    public Local getLocal() {
        return local;
    }
    
    /**
     * Sets the local config.
     *
     * @param local the local config
     */
    public void setLocal(final Local local) {
        this.local = local;
    }
    
    /**
     * Gets ribbon.
     *
     * @return the ribbon
     */
    public RibbonConfig getRibbon() {
        return ribbon;
    }
    
    /**
     * Sets ribbon.
     *
     * @param ribbon the ribbon
     */
    public void setRibbon(final RibbonConfig ribbon) {
        this.ribbon = ribbon;
    }
    
    /**
     * Gets instance.
     *
     * @return the instance
     */
    public InstanceConfig getInstance() {
        return instance;
    }
    
    /**
     * Sets instance.
     *
     * @param instance the instance
     */
    public void setInstance(final InstanceConfig instance) {
        this.instance = instance;
    }
    
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
     * Gets match cache.
     *
     * @return the match cache
     */
    public MatchCache getMatchCache() {
        return matchCache;
    }

    /**
     * Sets match cache.
     *
     * @param matchCache the match cache
     */
    public void setMatchCache(final MatchCache matchCache) {
        this.matchCache = matchCache;
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
     * Gets fallback.
     *
     * @return the fallback
     */
    public FallbackPath getFallback() {
        return fallback;
    }
    
    /**
     * Sets fallback.
     *
     * @param fallback the fallback
     */
    public void setFallback(final FallbackPath fallback) {
        this.fallback = fallback;
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
     * Gets the websocket config.
     *
     * @return the websocket config
     */
    public WebsocketConfig getWebsocket() {
        return websocket;
    }
    
    /**
     * Sets the websocket config.
     *
     * @param websocket the websocket config
     */
    public void setWebsocket(final WebsocketConfig websocket) {
        this.websocket = websocket;
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
    
        private boolean enabled;
    
        private String type = "fixed";
    
        private Integer threads = Math.max((Runtime.getRuntime().availableProcessors() << 1) + 1, 16);
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
        
        private boolean enabled;
    
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
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
     * the match cache.
     */
    public static class MatchCache {

        private boolean enabled;

        /**
         * Max free memory, unit mb.
         */
        private Integer maxFreeMemory = 256;

        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }

        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }

        /**
         * Gets maxFreeMemory.
         *
         * @return the maxFreeMemory
         */
        public Integer getMaxFreeMemory() {
            return maxFreeMemory;
        }

        /**
         * Sets maxFreeMemory.
         *
         * @param maxFreeMemory the maxFreeMemory
         */
        public void setMaxFreeMemory(final Integer maxFreeMemory) {
            this.maxFreeMemory = maxFreeMemory;
        }
    }
    
    /**
     * The type Exclude path.
     */
    public static class ExcludePath {
    
        private boolean enabled;
    
        private List<String> paths = new ArrayList<>();
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
     * The type fallback path.
     */
    public static class FallbackPath {

        private boolean enabled;

        private List<String> paths = new ArrayList<>();
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
     * The type Health.
     */
    public static class Health {
        
        private boolean enabled;
        
        private List<String> paths = new ArrayList<>();
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
    
        private boolean enabled;
    
        private Integer maxSize = 10;
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
        
        private boolean local = true;
    
        /**
         * Gets local.
         *
         * @return the local
         */
        public boolean getLocal() {
            return local;
        }
    
        /**
         * Sets local.
         *
         * @param local the local
         */
        public void setLocal(final boolean local) {
            this.local = local;
        }
        
    }
    
    /**
     * The type Upstream check.
     */
    public static class UpstreamCheck {
    
        private boolean enabled;
        
        private Integer timeout = 3000;
        
        private Integer healthyThreshold = 1;
    
        private Integer unhealthyThreshold = 1;
        
        private Integer interval = 5000;
        
        private boolean printEnabled;
        
        private Integer printInterval = 60000;
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
        public boolean getPrintEnabled() {
            return printEnabled;
        }
    
        /**
         * Sets print enabled.
         *
         * @param printEnabled the print enabled
         */
        public void setPrintEnabled(final boolean printEnabled) {
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
    
        private boolean enabled;

        /**
         * Comma-separated of “header”.
         */
        private String allowedHeaders = "";

        /**
         * Comma-separated of “method”.
         */
        private String allowedMethods = "*";

        private AllowedOriginConfig allowedOrigin = new AllowedOriginConfig();

        private boolean allowedAnyOrigin;

        private String allowedExpose = "";

        private String maxAge = "18000";

        private boolean allowCredentials;

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
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
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
        public AllowedOriginConfig getAllowedOrigin() {
            return allowedOrigin;
        }
    
        /**
         * Sets the allowedOrigin.
         *
         * @param allowedOrigin allowedOrigin
         */
        public void setAllowedOrigin(final AllowedOriginConfig allowedOrigin) {
            this.allowedOrigin = allowedOrigin;
        }

        /**
         * Gets the value of allowedAnyOrigin.
         *
         * @return the value of allowedAnyOrigin
         */
        public boolean isAllowedAnyOrigin() {
            return allowedAnyOrigin;
        }

        /**
         * Sets the allowedExpose.
         *
         * @param allowedAnyOrigin allowedExpose
         */
        public void setAllowedAnyOrigin(final boolean allowedAnyOrigin) {
            this.allowedAnyOrigin = allowedAnyOrigin;
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

        /**
         * the cors allowedOrigin config.
         */
        public static class AllowedOriginConfig {

            private String spacer = ".";

            private String domain;

            private Set<String> prefixes = new HashSet<>();

            private Set<String> origins;

            private String originRegex;

            /**
             * Gets the spacer.
             *
             * @return the value of spacer
             */
            public String getSpacer() {
                return spacer;
            }

            /**
             * Sets the spacer.
             *
             * @param spacer spacer
             */
            public void setSpacer(final String spacer) {
                this.spacer = spacer;
            }

            /**
             * Gets the domain.
             *
             * @return the value of domain
             */
            public String getDomain() {
                return domain;
            }

            /**
             * Sets the enabled.
             *
             * @param domain domain
             */
            public void setDomain(final String domain) {
                this.domain = domain;
            }

            /**
             * Gets the prefixes.
             *
             * @return the value of prefixes
             */
            public Set<String> getPrefixes() {
                return prefixes;
            }

            /**
             * Sets the enabled.
             *
             * @param prefixes prefixes
             */
            public void setPrefixes(final Set<String> prefixes) {
                this.prefixes = prefixes;
            }

            /**
             * Gets the prefixes.
             *
             * @return the value of prefixes
             */
            public Set<String> getOrigins() {
                return origins;
            }

            /**
             * Sets the origins.
             *
             * @param origins origins
             */
            public void setOrigins(final Set<String> origins) {
                this.origins = origins;
            }

            /**
             * Gets the originRegex.
             *
             * @return the value of originRegex
             */
            public String getOriginRegex() {
                return originRegex;
            }

            /**
             * Sets the originRegex.
             *
             * @param originRegex originRegex
             */
            public void setOriginRegex(final String originRegex) {
                this.originRegex = originRegex;
            }
        }
    }
    
    /**
     * The type Instance config.
     */
    public static class InstanceConfig {
    
        private boolean enabled;
    
        private String registerType;
    
        private String serverLists;
    
        private Properties props = new Properties();
    
        /**
         * Instantiates a new Instance config.
         */
        public InstanceConfig() {
        
        }
    
        /**
         * Instantiates a new Instance config.
         *
         * @param registerType the register type
         * @param serverLists the server lists
         * @param props the props
         */
        public InstanceConfig(final String registerType, final String serverLists, final Properties props) {
            this.registerType = registerType;
            this.serverLists = serverLists;
            this.props = props;
        }
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * getRegisterType.
         *
         * @return String register type
         */
        public String getRegisterType() {
            return registerType;
        }
    
        /**
         * setRegisterType.
         *
         * @param registerType registerType
         */
        public void setRegisterType(final String registerType) {
            this.registerType = registerType;
        }
    
        /**
         * getServerLists.
         *
         * @return String server lists
         */
        public String getServerLists() {
            return serverLists;
        }
    
        /**
         * setServerLists.
         *
         * @param serverLists serverLists
         */
        public void setServerLists(final String serverLists) {
            this.serverLists = serverLists;
        }
    
        /**
         * getProps.
         *
         * @return String props
         */
        public Properties getProps() {
            return props;
        }
    
        /**
         * setProps.
         *
         * @param props props
         */
        public void setProps(final Properties props) {
            this.props = props;
        }
    }
    
    /**
     * The Ribbon Config.
     */
    public static class RibbonConfig {

        /**
         * see {@code com.netflix.client.config.CommonClientConfigKey#ServerListRefreshInterval}.
         */
        private Integer serverListRefreshInterval = 10000;
    
        /**
         * Instantiates a new RibbonConfig.
         */
        public RibbonConfig() {
        }
    
        /**
         * Instantiates a new RibbonConfig.
         *
         * @param serverListRefreshInterval serverListRefreshInterval
         */
        public RibbonConfig(final Integer serverListRefreshInterval) {
            this.serverListRefreshInterval = serverListRefreshInterval;
        }
    
        /**
         * Gets serverListRefreshInterval.
         *
         * @return the serverListRefreshInterval
         */
        public Integer getServerListRefreshInterval() {
            return serverListRefreshInterval;
        }
    
        /**
         * setServerListRefreshInterval.
         *
         * @param serverListRefreshInterval serverListRefreshInterval
         */
        public void setServerListRefreshInterval(final Integer serverListRefreshInterval) {
            this.serverListRefreshInterval = serverListRefreshInterval;
        }
    }
    
    /**
     * The local config.
     */
    public static class Local {
        
        private boolean enabled;
        
        private String sha512Key;
    
        /**
         * Instantiates a new Local.
         */
        public Local() {
        }
    
        /**
         * Instantiates a new Local.
         *
         * @param sha512Key the sha 512 key
         */
        public Local(final String sha512Key) {
            this.sha512Key = sha512Key;
        }
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * Get Sha512Key.
         *
         * @return the key
         */
        public String getSha512Key() {
            return sha512Key;
        }
    
        /**
         * Set Sha512Key.
         *
         * @param sha512Key sha512Key
         */
        public void setSha512Key(final String sha512Key) {
            this.sha512Key = sha512Key;
        }
    }
    
    /**
     * the websocket config.
     */
    public static class WebsocketConfig {

        /**
         * max frame pay load size mb.
         */
        private Integer maxFramePayloadSize = 10;
    
        /**
         * Get max frame payload size.
         *
         * @return the max frame payload szie
         */
        public Integer getMaxFramePayloadSize() {
            return maxFramePayloadSize;
        }
    
        /**
         * Set max frame payload size.
         *
         * @param maxFramePayloadSize the max frame paylod size
         */
        public void setMaxFramePayloadSize(final Integer maxFramePayloadSize) {
            this.maxFramePayloadSize = maxFramePayloadSize;
        }
    }
    
    /**
     * The type Shared Thread Pool.
     */
    public static class SharedPool {

        /**
         * Whether to enable shared thread pool, defaults to false.
         * Note: it is planned to be enabled by default when all RPC/HTTP
         * plugins support this shared thread pool.
         */
        private Boolean enable = Boolean.FALSE;

        /**
         * The the thread name prefix, defaults to shenyu-shared.
         */
        private String prefix = "shenyu-shared";

        /**
         * the number of threads to keep in the thread pool.
         */
        private Integer corePoolSize = 200;

        /**
         * the maximum number of threads to allow in the thread pool.
         */
        private Integer maximumPoolSize = Integer.MAX_VALUE;

        /**
         * when the number of threads is greater than the core,
         * this is the maximum time that excess idle threads
         * will wait for new tasks before terminating.
         * Note: the unit of time is {@link java.util.concurrent.TimeUnit#MILLISECONDS}
         */
        private Long keepAliveTime = 60000L;

        /**
         * Maximum memory allowed to be used by a blocking queue, yes, unlike other
         * implementations of {@link java.util.concurrent.BlockingQueue}
         * (which all control memory based on the length of the blocking queue),
         * {@link org.apache.shenyu.common.concurrent.MemoryLimitedLinkedBlockingQueue}
         * controls memory directly by calculating the memory size used by the blocking queue.
         */
        private Long maxWorkQueueMemory = MemoryLimitCalculator.defaultLimit();

        /**
         * The memory used by the blocking queue is always in the safe range, and there
         * is always an attempt to make the JVM's free memory higher than this value.
         *
         * @see org.apache.shenyu.common.concurrent.MemorySafeLinkedBlockingQueue#getMaxFreeMemory()
         */
        private Integer maxFreeMemory;
    
        /**
         * Whether to enable shared thread pool.
         *
         * @return whether to enable
         */
        public Boolean getEnable() {
            return enable;
        }
    
        /**
         * Set enable.
         *
         * @param enable the enable
         */
        public void setEnable(final Boolean enable) {
            this.enable = enable;
        }
    
        /**
         * Get shared thread pool name prefix.
         *
         * @return the shared thread pool name prefix
         */
        public String getPrefix() {
            return prefix;
        }
    
        /**
         * Set prefix.
         *
         * @param prefix the prefix
         */
        public void setPrefix(final String prefix) {
            this.prefix = prefix;
        }
    
        /**
         * Get shared thread pool core size.
         *
         * @return the shared thread pool core size
         */
        public Integer getCorePoolSize() {
            return corePoolSize;
        }
    
        /**
         * Set core pool size.
         *
         * @param corePoolSize the core pool size
         */
        public void setCorePoolSize(final Integer corePoolSize) {
            this.corePoolSize = corePoolSize;
        }
    
        /**
         * Get shared thread pool maximum size.
         *
         * @return the shared thread pool name prefix
         */
        public Integer getMaximumPoolSize() {
            return maximumPoolSize;
        }
    
        /**
         * Set max pool size.
         *
         * @param maximumPoolSize the max pool size
         */
        public void setMaximumPoolSize(final Integer maximumPoolSize) {
            this.maximumPoolSize = maximumPoolSize;
        }
    
        /**
         * Get shared thread pool keep alive time.
         *
         * @return the shared thread pool keep alive time
         */
        public Long getKeepAliveTime() {
            return keepAliveTime;
        }
    
        /**
         * Set keep alive time.
         *
         * @param keepAliveTime the keep alive time
         */
        public void setKeepAliveTime(final Long keepAliveTime) {
            this.keepAliveTime = keepAliveTime;
        }
    
        /**
         * Get shared thread pool max work queue memory.
         *
         * @return the shared thread pool max work queue memory
         */
        public Long getMaxWorkQueueMemory() {
            return maxWorkQueueMemory;
        }
    
        /**
         * Set max work queue memory.
         *
         * @param maxWorkQueueMemory the max work queue memory
         */
        public void setMaxWorkQueueMemory(final Long maxWorkQueueMemory) {
            this.maxWorkQueueMemory = maxWorkQueueMemory;
        }

        /**
         * Get shared thread pool max work queue free memory.
         *
         * @return the shared thread pool max work queue free memory
         */
        public Integer getMaxFreeMemory() {
            return maxFreeMemory;
        }

        /**
         * Set max work queue free memory.
         *
         * @param maxFreeMemory the max work queue free memory
         */
        public void setMaxFreeMemory(final Integer maxFreeMemory) {
            this.maxFreeMemory = maxFreeMemory;
        }
    }
    
    /**
     * The type Metrics config.
     */
    public static class MetricsConfig {
    
        private boolean enabled;
        
        private String name;
        
        private String host;
        
        private Integer port;
        
        private String jmxConfig;
        
        private Properties props;
    
        /**
         * Instantiates a new Metrics config.
         */
        public MetricsConfig() {
        }
    
        /**
         * Gets enabled.
         *
         * @return the enabled
         */
        public boolean getEnabled() {
            return enabled;
        }
    
        /**
         * Sets enabled.
         *
         * @param enabled the enabled
         */
        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }
    
        /**
         * Gets metrics name.
         *
         * @return the metrics name
         */
        public String getName() {
            return name;
        }
    
        /**
         * Sets metrics name.
         *
         * @param name the metrics name
         */
        public void setName(final String name) {
            this.name = name;
        }
    
        /**
         * Gets host.
         *
         * @return the host
         */
        public String getHost() {
            return host;
        }
    
        /**
         * Sets host.
         *
         * @param host the host
         */
        public void setHost(final String host) {
            this.host = host;
        }
    
        /**
         * Gets port.
         *
         * @return the port
         */
        public Integer getPort() {
            return port;
        }
    
        /**
         * Sets port.
         *
         * @param port the port
         */
        public void setPort(final Integer port) {
            this.port = port;
        }
    
        /**
         * Gets jmx config.
         *
         * @return the jmx config
         */
        public String getJmxConfig() {
            return jmxConfig;
        }
    
        /**
         * Sets jmx config.
         *
         * @param jmxConfig the jmx config
         */
        public void setJmxConfig(final String jmxConfig) {
            this.jmxConfig = jmxConfig;
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
