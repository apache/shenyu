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

package org.apache.shenyu.plugin.api.utils;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.DataFormatEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.ObjectTypeUtils;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;

/**
 * The type Shenyu result utils.
 */
public final class WebFluxResultUtils {

    /**
     * result utils log.
     */
    private static final Logger LOG = LoggerFactory.getLogger(WebFluxResultUtils.class);

    /**
     * Response result.
     *
     * @param exchange the exchange
     * @param result    the result
     * @return the result
     */
    public static Mono<Void> result(final ServerWebExchange exchange, final Object result) {
        if (Objects.isNull(result)) {
            return Mono.empty();
        }
        DataFormatEnum dataFormat = exchange.getAttributeOrDefault(Constants.DATA_FORMAT, DataFormatEnum.DEFAULT);
        // return basic data
        if (ObjectTypeUtils.isBasicTypeExceptString(result)) {
            return writeWith(exchange, MediaType.TEXT_PLAIN, result.toString());
        }
        String json;
        String data;
        final ShenyuResult<?> shenyuResult = ShenyuResultWrap.shenyuResult();
        if (dataFormat.isXml()) {
            // was xml
            if (XmlUtils.isValidXml(data = result.toString())) {
                return writeWith(exchange, dataFormat.getMediaType(), data);
            }
            // just a string
            if (!GsonUtils.isValidJson(json = result.toString())
                    && !GsonUtils.isValidJson(json = GsonUtils.getInstance().toJson(result))) {
                return writeWith(exchange, MediaType.TEXT_PLAIN, data);
            }
            // the format is xml, but the upstream is json data. convert the json to xml.
            data = getXmlDataByJson(shenyuResult, json);
        } else {
            // was json
            if (GsonUtils.isValidJson(json = result.toString())
                    || GsonUtils.isValidJson(json = GsonUtils.getInstance().toJson(result))) {
                return writeWith(exchange, dataFormat.getMediaType(), json);
            }
            // just a string
            if (!XmlUtils.isValidXml(data = XmlUtils.toXml(result))) {
                return writeWith(exchange, MediaType.TEXT_PLAIN, data);
            }
            // the format is json, but the upstream is xml data. convert the xml to json.
            data = getJsonDataByXml(shenyuResult, data);
        }
        // reset the map.
        shenyuResult.clear();
        return writeWith(exchange, dataFormat.getMediaType(), data);
    }

    /**
     * get no selector result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> noSelectorResult(final String pluginName, final ServerWebExchange exchange) {
        LOG.error("can not match selector data: {}", pluginName);
        Object error = ShenyuResultWrap.error(ShenyuResultEnum.SELECTOR_NOT_FOUND.getCode(), pluginName + ":" + ShenyuResultEnum.SELECTOR_NOT_FOUND.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * get no rule result.
     *
     * @param pluginName the plugin name
     * @param exchange   the exchange
     * @return the mono
     */
    public static Mono<Void> noRuleResult(final String pluginName, final ServerWebExchange exchange) {
        LOG.error("can not match rule data: {}", pluginName);
        Object error = ShenyuResultWrap.error(ShenyuResultEnum.RULE_NOT_FOUND.getCode(), pluginName + ":" + ShenyuResultEnum.RULE_NOT_FOUND.getMsg(), null);
        return WebFluxResultUtils.result(exchange, error);
    }

    /**
     * Write With data.
     *
     * @param exchange the exchange
     * @param mediaType the data mediaType
     * @param data the data
     * @return response result
     */
    private static Mono<Void> writeWith(final ServerWebExchange exchange, final MediaType mediaType, final String data) {
        exchange.getResponse().getHeaders().setContentType(mediaType);
        return exchange.getResponse().writeWith(Mono.just(exchange.getResponse()
                .bufferFactory().wrap(Objects.requireNonNull(data).getBytes(StandardCharsets.UTF_8))));
    }

    /**
     * Gets xml data.
     *
     * @param shenyuResult the shenyu result
     * @param json the json
     * @return xml data
     */
    private static String getXmlDataByJson(final ShenyuResult<?> shenyuResult, final String json) {
        String data = getData(shenyuResult, json);
        // default error xml.
        if (Objects.isNull(data)) {
            // reset the map.
            shenyuResult.clear();
            // generate default xml error data
            final Object defaultError = shenyuResult.error(
                    ShenyuResultEnum.INVALID_XML_DATA.getCode(),
                    ShenyuResultEnum.INVALID_XML_DATA.getMsg(), null);
            data = getData(shenyuResult, GsonUtils.getInstance().toJson(defaultError));
        }
        return data;
    }

    /**
     * Gets data.
     *
     * @param shenyuResult the shenyu result
     * @param data the data
     * @return wapper data
     */
    private static String getData(final ShenyuResult<?> shenyuResult, final String data) {
        shenyuResult.putAll(GsonUtils.getInstance().convertToMap(data));
        return XmlUtils.toXml(shenyuResult);
    }

    /**
     * Gets json data.
     *
     * @param shenyuResult the shenyu result
     * @param xml the xml
     * @return json data
     */
    private static String getJsonDataByXml(final ShenyuResult<?> shenyuResult, final String xml) {
        String data = Constants.EMPTY_JSON;
        if (StringUtils.isNotBlank(xml)) {
            final Class<?> shenyuResultClass = shenyuResult.getClass();
            String root = Constants.DEFAULT_XML_ROOT;
            if (shenyuResultClass.isAnnotationPresent(JacksonXmlRootElement.class)) {
                root = shenyuResultClass.getAnnotation(JacksonXmlRootElement.class).localName();
            }
            final Map<String, Object> xmlMap = XmlUtils.toMap(xml, root);
            if (MapUtils.isNotEmpty(xmlMap)) {
                data = GsonUtils.getInstance().toJson(xmlMap);
            }
        }
        return data;
    }
}
