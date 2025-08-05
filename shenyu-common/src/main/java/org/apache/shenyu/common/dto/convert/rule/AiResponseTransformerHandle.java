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
 * this is Ai Response Transformer plugin handler.
 */
public class AiResponseTransformerHandle implements RuleHandle {

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
     * content.
     */
    private String content;

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
     * get content.
     *
     * @return content
     */
    public String getContent() {
        return content;
    }

    /**
     * setContent.
     *
     * @param content content
     */
    public void setContent(final String content) {
        this.content = content;
    }

    /**
     * new default instance.
     *
     * @return AiResponseTransformerHandle
     */
    public static AiResponseTransformerHandle newDefaultInstance() {
        return new AiResponseTransformerHandle();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        AiResponseTransformerHandle that = (AiResponseTransformerHandle) o;
        return Objects.equals(provider, that.provider)
                && Objects.equals(baseUrl, that.baseUrl)
                && Objects.equals(apiKey, that.apiKey)
                && Objects.equals(model, that.model)
                && Objects.equals(content, that.content);
    }

    @Override
    public int hashCode() {
        return Objects.hash(provider, baseUrl, apiKey, model, content);
    }

    @Override
    public String toString() {
        return "AiResponseTransformerHandle{"
                + "provider='" + provider + '\''
                + ", baseUrl='" + baseUrl + '\''
                + ", apiKey='" + apiKey + '\''
                + ", model='" + model + '\''
                + ", content=" + content
                + '}';
    }
} 
