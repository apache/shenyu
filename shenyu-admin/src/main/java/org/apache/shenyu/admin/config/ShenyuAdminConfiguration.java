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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.service.converter.SelectorHandleConverter;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.i18n.AcceptHeaderLocaleResolver;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Shenyu admin configuration.
 */
@Configuration
@EnableTransactionManagement
public class ShenyuAdminConfiguration {
    
    /**
     * Selector handle converter factor selector handle converter factor.
     *
     * @param converterList the converter list
     * @return the selector handle converter factor
     */
    @Bean
    public SelectorHandleConverterFactor selectorHandleConverterFactor(final List<SelectorHandleConverter> converterList) {
        Map<String, SelectorHandleConverter> converterMap = converterList.stream().collect(Collectors.toMap(SelectorHandleConverter::pluginName, Function.identity()));
        return new SelectorHandleConverterFactor(converterMap);
    }
    
    /**
     * support I18n.
     *
     * @return LocaleResolver
     */
    @Bean
    public LocaleResolver localeResolver() {
        final AcceptHeaderLocaleResolver localeResolver = new AcceptHeaderLocaleResolver();
        localeResolver.setSupportedLocales(Stream.of(Locale.US, Locale.SIMPLIFIED_CHINESE).collect(Collectors.toList()));
        return localeResolver;
    }
}
