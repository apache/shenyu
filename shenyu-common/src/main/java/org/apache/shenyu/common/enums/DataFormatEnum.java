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

package org.apache.shenyu.common.enums;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * DataFormatEnum.
 */
public enum DataFormatEnum {

    /**
     * the default data format: json.
     */
    DEFAULT("json", MediaType.APPLICATION_JSON),

    /**
     * the json data format.
     */
    JSON(DEFAULT.format, DEFAULT.mediaType),

    /**
     * the xml data format.
     */
    XML("xml", MediaType.APPLICATION_XML);

    /**
     * the data format.
     */
    private final String format;

    /**
     * the data media type.
     */
    private final MediaType mediaType;

    /**
     * the constructor.
     *
     * @param format the format
     * @param mediaType the media type
     */
    DataFormatEnum(final String format, final MediaType mediaType) {
        this.format = format;
        this.mediaType = mediaType;
    }

    /**
     * Gets the format.
     *
     * @return the format
     */
    public String getFormat() {
        return format;
    }

    /**
     * Gets the media type.
     *
     * @return the media tpe
     */
    public MediaType getMediaType() {
        return mediaType;
    }

    /**
     * is xml or not.
     *
     * @return true is xml
     */
    public boolean isXml() {
        return XML.equals(this);
    }

    /**
     * Gets by format.
     *
     * @param format the format
     * @return data format enum
     */
    public static DataFormatEnum getByFormat(final String format) {
        if (StringUtils.isBlank(format)) {
            return DEFAULT;
        }
        return Arrays.stream(DataFormatEnum.values())
                .filter(dataFormatEnum -> dataFormatEnum.getFormat().equals(format))
                .findFirst().orElse(DEFAULT);
    }

    /**
     * Gets all format names.
     *
     * @return names
     */
    public static List<String> getFormatNames() {
        return Stream.of(values()).map(DataFormatEnum::getFormat).distinct().collect(Collectors.toList());
    }
}
