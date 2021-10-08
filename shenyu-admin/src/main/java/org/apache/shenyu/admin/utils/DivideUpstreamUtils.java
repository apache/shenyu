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

import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

import java.util.Optional;

/**
 * Build upstream for rpc plugin.
 */
public class DivideUpstreamUtils {


    /**
     * build divide upstream.
     *
     * @param metaDataRegisterDTO metaDataRegisterDTO
     * @return divideUpstream divideUpstream
     */
    public static DivideUpstream buildDivideUpstream(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return DivideUpstream.builder().upstreamHost("localhost").protocol("http://").upstreamUrl(buildUrl(metaDataRegisterDTO)).weight(50).warmup(10).timestamp(System.currentTimeMillis()).build();
    }

    /**
     * build url.
     *
     * @param metaDataRegisterDTO metaDataRegisterDTO
     * @return String String
     */
    public static String buildUrl(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return Optional.of(String.join(":", metaDataRegisterDTO.getHost(), String.valueOf(metaDataRegisterDTO.getPort()))).orElse(null);
    }
}
