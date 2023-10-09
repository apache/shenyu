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

package org.apache.shenyu.plugin.base.utils;

import org.springframework.http.MediaType;

/**
 * MediaTypeUtils.
 */
public class MediaTypeUtils {
    /**
     * is byte type .
     * @param mediaType mediaType
     * @return true is basic
     */
    public static boolean isByteType(final MediaType mediaType) {
        return MediaType.TEXT_EVENT_STREAM.isCompatibleWith(mediaType)
                || MediaType.MULTIPART_MIXED.isCompatibleWith(mediaType)
                || MediaType.IMAGE_PNG.isCompatibleWith(mediaType)
                || MediaType.IMAGE_JPEG.isCompatibleWith(mediaType)
                || MediaType.IMAGE_GIF.isCompatibleWith(mediaType)
                //APPLICATION_STREAM_JSON is deprecated
                || MediaType.APPLICATION_NDJSON.isCompatibleWith(mediaType)
                || MediaType.APPLICATION_PDF.isCompatibleWith(mediaType)
                || MediaType.APPLICATION_OCTET_STREAM.isCompatibleWith(mediaType);
    }
}
