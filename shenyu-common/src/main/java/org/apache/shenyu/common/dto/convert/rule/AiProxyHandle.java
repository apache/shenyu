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

package org.apache.shenyu.common.dto.convert.rule;

import org.apache.shenyu.common.enums.AiModelProviderEnum;

import java.util.Objects;

/**
 * this is Ai Proxy plugin handle.
 */
public class AiProxyHandle {

    /**
     * provider.
     */
    private String provider;

    /**
     * base url.
     */
    private String baseUrl;

    /**
     * api key.
     */
    private String apiKey;

    /**
     * model.
     */
    private String model;

    /**
     * temperature.
     */
    private Double temperature = 0.8;

    /**
     * max tokens.
     */
    private Integer maxTokens;

    /**
     * stream.
     */
    private Boolean stream = false;

    /**
     * fallback config.
     */
    private FallbackConfig fallbackConfig;

    // flat fallback fields (to fit dashboard flat handle capability)
    private String fallbackEnabled;

    private String fallbackProvider;

    private String fallbackBaseUrl;

    private String fallbackApiKey;

    private String fallbackModel;

    private Double fallbackTemperature;

    private Integer fallbackMaxTokens;

    /**
     * enable proxy apikey authentication. When true, header X-API-KEY is required.
     */
    private String proxyEnabled;

    /**
     * new default instance.
     *
     * @return AiProxyHandle
     */
    public static AiProxyHandle newDefaultInstance() {
        AiProxyHandle aiProxyHandle = new AiProxyHandle();
        aiProxyHandle.setProvider(AiModelProviderEnum.OPEN_AI.getName());
        aiProxyHandle.setBaseUrl("https://api.openai.com");
        aiProxyHandle.setApiKey("your-api-key");
        aiProxyHandle.setModel("gpt-4o-mini");
        aiProxyHandle.setTemperature(0.8);
        aiProxyHandle.setStream(false);
        aiProxyHandle.setFallbackConfig(new FallbackConfig());
        return aiProxyHandle;
    }

    /**
     * get provider.
     *
     * @return provider
     */
    public String getProvider() {
        return provider;
    }

    /**
     * set provider.
     *
     * @param provider provider
     */
    public void setProvider(final String provider) {
        this.provider = provider;
    }

    /**
     * get base url.
     *
     * @return base url
     */
    public String getBaseUrl() {
        return baseUrl;
    }

    /**
     * set base url.
     *
     * @param baseUrl base url
     */
    public void setBaseUrl(final String baseUrl) {
        this.baseUrl = baseUrl;
    }

    /**
     * get api key.
     *
     * @return api key
     */
    public String getApiKey() {
        return apiKey;
    }

    /**
     * set api key.
     *
     * @param apiKey api key
     */
    public void setApiKey(final String apiKey) {
        this.apiKey = apiKey;
    }

    /**
     * get model.
     *
     * @return model
     */
    public String getModel() {
        return model;
    }

    /**
     * set model.
     *
     * @param model model
     */
    public void setModel(final String model) {
        this.model = model;
    }

    /**
     * get temperature.
     *
     * @return temperature
     */
    public Double getTemperature() {
        return temperature;
    }

    /**
     * set temperature.
     *
     * @param temperature temperature
     */
    public void setTemperature(final Double temperature) {
        this.temperature = temperature;
    }

    /**
     * get max tokens.
     *
     * @return max tokens
     */
    public Integer getMaxTokens() {
        return maxTokens;
    }

    /**
     * set max tokens.
     *
     * @param maxTokens max tokens
     */
    public void setMaxTokens(final Integer maxTokens) {
        this.maxTokens = maxTokens;
    }

    /**
     * get stream.
     *
     * @return stream
     */
    public Boolean getStream() {
        return stream;
    }

    /**
     * set stream.
     *
     * @param stream stream
     */
    public void setStream(final Boolean stream) {
        this.stream = stream;
    }

    /**
     * get fallback config.
     *
     * @return fallback config
     */
    public FallbackConfig getFallbackConfig() {
        return fallbackConfig;
    }

    /**
     * set fallback config.
     *
     * @param fallbackConfig fallback config
     */
    public void setFallbackConfig(final FallbackConfig fallbackConfig) {
        this.fallbackConfig = fallbackConfig;
    }

    /**
     * Normalize this handle by assembling nested FallbackConfig from flat fields when enabled.
     *
     * @return this handle after normalization
     */
    public AiProxyHandle normalize() {
        boolean enabled = "true".equalsIgnoreCase(String.valueOf(fallbackEnabled));
        if (!enabled) {
            this.fallbackConfig = null;
            return this;
        }
        FallbackConfig cfg = Objects.nonNull(this.fallbackConfig) ? this.fallbackConfig : new FallbackConfig();
        if (Objects.nonNull(fallbackProvider) && !fallbackProvider.isEmpty()) {
            cfg.setProvider(fallbackProvider);
        }
        if (Objects.nonNull(fallbackBaseUrl) && !fallbackBaseUrl.isEmpty()) {
            cfg.setBaseUrl(fallbackBaseUrl);
        }
        if (Objects.nonNull(fallbackApiKey) && !fallbackApiKey.isEmpty()) {
            cfg.setApiKey(fallbackApiKey);
        }
        if (Objects.nonNull(fallbackModel) && !fallbackModel.isEmpty()) {
            cfg.setModel(fallbackModel);
        }
        if (Objects.nonNull(fallbackTemperature)) {
            cfg.setTemperature(fallbackTemperature);
        }
        if (Objects.nonNull(fallbackMaxTokens)) {
            cfg.setMaxTokens(fallbackMaxTokens);
        }
        this.fallbackConfig = cfg;
        return this;
    }

    /**
     * get proxyEnabled.
     *
     * @return proxyEnabled
     */
    public String getProxyEnabled() {
        return proxyEnabled;
    }

    /**
     * set proxyEnabled.
     *
     * @param proxyEnabled proxyEnabled flag string
     */
    public void setProxyEnabled(final String proxyEnabled) {
        this.proxyEnabled = proxyEnabled;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        AiProxyHandle that = (AiProxyHandle) o;
        return Objects.equals(provider, that.provider)
                && Objects.equals(baseUrl, that.baseUrl)
                && Objects.equals(apiKey, that.apiKey)
                && Objects.equals(model, that.model)
                && Objects.equals(temperature, that.temperature)
                && Objects.equals(maxTokens, that.maxTokens)
                && Objects.equals(stream, that.stream)
                && Objects.equals(fallbackConfig, that.fallbackConfig)
                && Objects.equals(proxyEnabled, that.proxyEnabled);
    }

    @Override
    public int hashCode() {
        return Objects.hash(provider, baseUrl, apiKey, model, temperature, maxTokens, stream, fallbackConfig, proxyEnabled);
    }

    @Override
    public String toString() {
        return "AiProxyHandle{"
                + "provider='" + provider + '\''
                + ", baseUrl='" + baseUrl + '\''
                + ", apiKey='" + maskApiKey(apiKey) + '\''
                + ", model='" + model + '\''
                + ", temperature=" + temperature
                + ", maxTokens=" + maxTokens
                + ", stream=" + stream
                + ", fallbackConfig=" + fallbackConfig
                + ", proxyEnabled=" + proxyEnabled
                + '}';
    }

    public static String maskApiKey(final String apiKey) {
        if (Objects.isNull(apiKey) || apiKey.isEmpty()) {
            return apiKey;
        }
        int len = apiKey.length();
        if (len <= 4) {
            // Show only the first character, mask the rest
            return apiKey.substring(0, 1) + "***";
        } else if (len <= 7) {
            // Show first and last character, mask the middle
            return apiKey.substring(0, 1) + "***" + apiKey.substring(len - 1);
        } else {
            // Show first 3 and last 4 characters, mask the middle
            return apiKey.substring(0, 3) + "****" + apiKey.substring(len - 4);
        }
    }

    /**
     * The type Fallback config.
     */
    public static class FallbackConfig {

        private String provider;

        private String baseUrl;

        private String apiKey;

        private String model;

        private Double temperature;

        private Integer maxTokens;

        /**
         * Gets provider.
         *
         * @return the provider
         */
        public String getProvider() {
            return provider;
        }

        /**
         * Sets provider.
         *
         * @param provider the provider
         */
        public void setProvider(final String provider) {
            this.provider = provider;
        }

        /**
         * Gets base url.
         *
         * @return the base url
         */
        public String getBaseUrl() {
            return baseUrl;
        }

        /**
         * Sets base url.
         *
         * @param baseUrl the base url
         */
        public void setBaseUrl(final String baseUrl) {
            this.baseUrl = baseUrl;
        }

        /**
         * Gets api key.
         *
         * @return the api key
         */
        public String getApiKey() {
            return apiKey;
        }

        /**
         * Sets api key.
         *
         * @param apiKey the api key
         */
        public void setApiKey(final String apiKey) {
            this.apiKey = apiKey;
        }

        /**
         * Gets model.
         *
         * @return the model
         */
        public String getModel() {
            return model;
        }

        /**
         * Sets model.
         *
         * @param model the model
         */
        public void setModel(final String model) {
            this.model = model;
        }

        /**
         * Gets temperature.
         *
         * @return the temperature
         */
        public Double getTemperature() {
            return temperature;
        }

        /**
         * Sets temperature.
         *
         * @param temperature the temperature
         */
        public void setTemperature(final Double temperature) {
            this.temperature = temperature;
        }

        /**
         * Gets max tokens.
         *
         * @return the max tokens
         */
        public Integer getMaxTokens() {
            return maxTokens;
        }

        /**
         * Sets max tokens.
         *
         * @param maxTokens the max tokens
         */
        public void setMaxTokens(final Integer maxTokens) {
            this.maxTokens = maxTokens;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (Objects.isNull(o) || getClass() != o.getClass()) {
                return false;
            }
            FallbackConfig that = (FallbackConfig) o;
            return Objects.equals(provider, that.provider)
                    && Objects.equals(baseUrl, that.baseUrl)
                    && Objects.equals(apiKey, that.apiKey)
                    && Objects.equals(model, that.model)
                    && Objects.equals(temperature, that.temperature)
                    && Objects.equals(maxTokens, that.maxTokens);
        }

        @Override
        public int hashCode() {
            return Objects.hash(provider, baseUrl, apiKey, model, temperature, maxTokens);
        }

        @Override
        public String toString() {
            return "FallbackConfig{"
                    + "provider='"
                    + provider + '\''
                    + ", baseUrl='"
                    + baseUrl + '\''
                    + ", apiKey='"
                    + maskApiKey(apiKey) + '\''
                    + ", model='"
                    + model + '\''
                    + ", temperature="
                    + temperature
                    + ", maxTokens="
                    + maxTokens
                    + '}';
        }
    }
}
