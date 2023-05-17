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

package org.apache.shenyu.admin.utils;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.EncodedResource;
import org.springframework.core.io.support.PropertiesLoaderUtils;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;

/**
 * i18n util.
 */
public class I18nUtil {

    private static final Logger LOG = LoggerFactory.getLogger(I18nUtil.class);

    /**
     * request header params.
     */
    public static final String HEADER_LOCATION = "Location";

    /**
     * default_i18n.
     */
    private static final String DEFAULT_I18N = "en";

    /**
     * loaded i18n properties.
     */
    private static final Map<String, Properties> I18N_MAP = new ConcurrentHashMap<>();

    /**
     * loadI18nProp.
     *
     * @param i18n language
     * @return Properties {@link Properties}
     */
    public static Properties loadI18nProp(final String i18n) {
        try {
            if (I18N_MAP.get(i18n) != null) {
                return I18N_MAP.get(i18n);
            }
            // build i18n prop
            String i18nFile = MessageFormat.format("i18n/message_{0}.properties", i18n);
            // load prop
            Resource resource = new ClassPathResource(i18nFile);
            EncodedResource encodedResource = new EncodedResource(resource, "UTF-8");
            Properties properties = PropertiesLoaderUtils.loadProperties(encodedResource);
            I18N_MAP.put(i18n, properties);
            return properties;
        } catch (IOException e) {
            LOG.error(e.getMessage(), e);
        }
        return null;
    }

    /**
     * get val of i18n key.
     *
     * @param key  i18nKey
     * @param i18n language
     * @return i18n_language
     */
    public static String getString(final String i18n, final String key) {
        Properties properties = null;
        if (StringUtils.isBlank(i18n)) {
            properties = loadI18nProp(DEFAULT_I18N);
        } else {
            properties = loadI18nProp(i18n);
        }
        if (properties == null) {
            LOG.error("i18n language {} is not exists!", i18n);
            return "";
        }
        return properties.getProperty(key);
    }

}
