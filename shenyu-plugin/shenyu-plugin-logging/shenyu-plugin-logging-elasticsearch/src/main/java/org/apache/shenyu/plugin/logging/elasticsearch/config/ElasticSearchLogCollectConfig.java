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

package org.apache.shenyu.plugin.logging.elasticsearch.config;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;

import java.util.Optional;

/**
 * log collect config, include elasticsearch config.
 * Host and port must be included, and others are optional.
 * We should operate the configuration through admin instead of the configuration file.
 */
public class ElasticSearchLogCollectConfig {

    public static final ElasticSearchLogCollectConfig INSTANCE = new ElasticSearchLogCollectConfig();

    private ElasticSearchLogConfig elasticSearchLogConfig;

    /**
     * get elastic search log config.
     *
     * @return log config
     */
    public ElasticSearchLogConfig getElasticSearchLogConfig() {
        return Optional.ofNullable(elasticSearchLogConfig).orElse(new ElasticSearchLogConfig());
    }

    /**
     * set elastic search log config.
     *
     * @param elasticSearchLogConfig log config.
     */
    public void setElasticSearchLogConfig(final ElasticSearchLogConfig elasticSearchLogConfig) {
        this.elasticSearchLogConfig = elasticSearchLogConfig;
    }

    /**
     * elastic search log config.
     */
    public static class ElasticSearchLogConfig extends GenericGlobalConfig {

        private String host;

        private String port;

        private String compressAlg;

        /**
         * whether compress.
         *
         * @return compress or not
         */
        public String getCompressAlg() {
            return compressAlg;
        }

        /**
         * set compress.
         *
         * @param compressAlg compress alg.
         */
        public void setCompressAlg(final String compressAlg) {
            this.compressAlg = compressAlg;
        }

        /**
         * get host.
         *
         * @return host
         */
        public String getHost() {
            return host;
        }

        /**
         * set host.
         *
         * @param host host name
         */
        public void setHost(final String host) {
            this.host = host;
        }

        /**
         * get port.
         *
         * @return port
         */
        public String getPort() {
            return port;
        }

        /**
         * set port.
         *
         * @param port port name
         */
        public void setPort(final String port) {
            this.port = port;
        }

    }

    /**
     * api log config.
     */
    public static class LogApiConfig {

        private String index;

        /**
         * 0 means never sample, 1 means always sample. Minimum probability is 0.01, or 1% of logging
         */
        private String sampleRate;

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
         * get index name.
         *
         * @return index name
         */
        public String getIndex() {
            return index;
        }

        /**
         * set index name.
         *
         * @param index index name
         */
        public void setIndex(final String index) {
            this.index = index;
        }

    }

}
