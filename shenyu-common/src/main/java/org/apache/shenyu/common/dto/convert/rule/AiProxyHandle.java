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

import java.util.Objects;

/**
 * this is Ai Proxy plugin handle.
 */
public class AiProxyHandle {

    /**
     * type.
     */
    private String type;

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
    private Double temperature;

    /**
     * max tokens.
     */
    private Integer maxTokens;

    /**
     * prompt.
     */
    private String prompt;

    /**
     * stream.
     */
    private Boolean stream = false;

    /**
     * get type.
     *
     * @return type
     */
    public String getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
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
     * get prompt.
     *
     * @return prompt
     */
    public String getPrompt() {
        return prompt;
    }

    /**
     * set prompt.
     *
     * @param prompt prompt
     */
    public void setPrompt(final String prompt) {
        this.prompt = prompt;
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        AiProxyHandle that = (AiProxyHandle) o;
        return Objects.equals(type, that.type)
                && Objects.equals(baseUrl, that.baseUrl)
                && Objects.equals(apiKey, that.apiKey)
                && Objects.equals(model, that.model)
                && Objects.equals(temperature, that.temperature)
                && Objects.equals(maxTokens, that.maxTokens)
                && Objects.equals(prompt, that.prompt)
                && Objects.equals(stream, that.stream);
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, baseUrl, apiKey, model, temperature, maxTokens, prompt, stream);
    }

    @Override
    public String toString() {
        return "AiProxyHandle{" + "type='" + type + '\'' + "baseUrl='" + baseUrl + '\'' + ", apiKey='" + apiKey + '\''
                + ", model='" + model
                + '\''
                + ", temperature=" + temperature + ", maxTokens=" + maxTokens
                + ", prompt='" + prompt + '\'' + ", stream=" + stream + '}';
    }
}
