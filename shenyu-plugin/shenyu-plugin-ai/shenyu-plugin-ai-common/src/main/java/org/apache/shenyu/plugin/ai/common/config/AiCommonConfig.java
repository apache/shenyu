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

package org.apache.shenyu.plugin.ai.common.config;

import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;

import java.util.Objects;
import java.util.Optional;

/**
 * this is Ai plugin common config.
 */
public class AiCommonConfig {

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

    public AiCommonConfig() {
    }

    public AiCommonConfig(final AiProxyHandle.FallbackConfig fallbackConfig) {
        this.provider = fallbackConfig.getProvider();
        this.baseUrl = fallbackConfig.getBaseUrl();
        this.apiKey = fallbackConfig.getApiKey();
        this.model = fallbackConfig.getModel();
        this.temperature = fallbackConfig.getTemperature();
        this.maxTokens = fallbackConfig.getMaxTokens();
    }

    public AiCommonConfig(final AiCommonConfig other) {
        this.provider = other.getProvider();
        this.baseUrl = other.getBaseUrl();
        this.apiKey = other.getApiKey();
        this.model = other.getModel();
        this.temperature = other.getTemperature();
        this.maxTokens = other.getMaxTokens();
        this.stream = other.getStream();
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
     * merge with another config.
     *
     * @param overlay overlay config
     * @return merged config
     */
    public AiCommonConfig mergeWith(final AiCommonConfig overlay) {
        if (Objects.isNull(overlay)) {
            return this;
        }
        this.setProvider(Optional.ofNullable(overlay.getProvider()).orElse(this.getProvider()));
        this.setModel(Optional.ofNullable(overlay.getModel()).orElse(this.getModel()));
        this.setApiKey(Optional.ofNullable(overlay.getApiKey()).orElse(this.getApiKey()));
        this.setBaseUrl(Optional.ofNullable(overlay.getBaseUrl()).orElse(this.getBaseUrl()));
        this.setTemperature(Optional.ofNullable(overlay.getTemperature()).orElse(this.getTemperature()));
        this.setMaxTokens(Optional.ofNullable(overlay.getMaxTokens()).orElse(this.getMaxTokens()));
        this.setStream(Optional.ofNullable(overlay.getStream()).orElse(this.getStream()));
        return this;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        AiCommonConfig that = (AiCommonConfig) o;
        return Objects.equals(provider, that.provider)
                && Objects.equals(baseUrl, that.baseUrl)
                && Objects.equals(apiKey, that.apiKey)
                && Objects.equals(model, that.model)
                && Objects.equals(temperature, that.temperature)
                && Objects.equals(maxTokens, that.maxTokens)
                && Objects.equals(stream, that.stream);
    }

    @Override
    public int hashCode() {
        return Objects.hash(provider, baseUrl, apiKey, model, temperature, maxTokens, stream);
    }

    @Override
    public String toString() {
        return "AiCommonConfig{"
                + "provider='" + provider + '\''
                + ", model='" + model + '\''
                + ", apiKey='" + AiProxyHandle.maskApiKey(apiKey) + '\''
                + ", baseUrl='" + baseUrl + '\''
                + ", temperature=" + temperature
                + ", maxTokens=" + maxTokens
                + ", stream=" + stream
                + '}';
    }
}