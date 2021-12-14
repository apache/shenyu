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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * XmlUtils.
 */
public final class XmlUtils {

    /**
     * xml logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(XmlUtils.class);

    /**
     * xml mapper.
     */
    private static final XmlMapper MAPPER = new XmlMapper();

    /**
     * Object to xml string.
     *
     * @param object the object
     * @return xml string
     */
    public static String toXml(final Object object) {
        try {
            // was xml or not
            if ((object instanceof String)
                    && Objects.nonNull(MAPPER.readValue(object.toString(), Object.class))) {
                return object.toString();
            }
            return convertToXml(object);
        } catch (IOException e) {
            // the data is invalid xml
            return object.toString();
        }
    }

    /**
     * xml string to map.
     *
     * @param xml the xml string
     * @param root the xml root, when nil use the shenyu default
     * @return the xml converted map
     */
    public static Map<String, Object> toMap(final String xml, final String root) {
        final Map<String, Object> map = new HashMap<>();
        if (StringUtils.isBlank(xml)) {
            return map;
        }
        // invalid xml
        try {
            MAPPER.readValue(xml, Object.class);
        } catch (JsonProcessingException e) {
            return map;
        }

        String data = xml;
        String rootName = root;
        if (StringUtils.isBlank(root)) {
            rootName = Constants.DEFAULT_XML_ROOT;
        }
        if (!(xml.startsWith(String.format("<%s>", rootName))
                && xml.endsWith(String.format("</%s>", rootName)))) {
            data = String.format("<%s>%s</%s>", rootName, xml, rootName);
        }
        try {
            return MAPPER.readValue(data, new TypeReference<Map<String, Object>>() {
                @Override
                public Type getType() {
                    return super.getType();
                }
            });
        } catch (IOException e) {
            LOG.warn("xml write to map error: " + xml, e);
        }
        return map;
    }

    /**
     * check xml valid or not.
     *
     * @param xml the xml string
     * @return true is valid
     */
    public static boolean isValidXml(final String xml) {
        if (StringUtils.isBlank(xml)) {
            return false;
        }
        try {
            MAPPER.readValue(xml, Object.class);
        } catch (JsonProcessingException e) {
            return false;
        }
        return true;
    }

    /**
     * convert object to xml.
     * @param object the object
     * @return xml string
     */
    private static String convertToXml(final Object object) {
        try {
            return MAPPER.writeValueAsString(object);
        } catch (IOException e) {
            LOG.warn("write to xml string error: " + object, e);
        }
        return null;
    }
}
