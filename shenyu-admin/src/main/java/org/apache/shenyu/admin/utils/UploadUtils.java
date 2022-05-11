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

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import org.springframework.util.MultiValueMap;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

/**
 * File upload tool.
 */
public class UploadUtils {

    /**
     * getUploadFiles.
     *
     * @param request request
     * @return Collection
     */
    public static Collection<MultipartFile> getUploadFiles(final HttpServletRequest request) {
        MultiValueMap<String, MultipartFile> fileMap = null;
        //Check whether there is：enctype="multipart/form-data" in the form.
        String contentType = request.getContentType();
        if (contentType != null && contentType.toLowerCase().contains("multipart")) {
            //Change the request into a multipart request.
            MultipartHttpServletRequest multiRequest = (MultipartHttpServletRequest) request;
            fileMap = multiRequest.getMultiFileMap();
        }
        return Optional.ofNullable(fileMap)
                .map(Map::entrySet)
                .map(entry -> entry.stream()
                        .flatMap(e -> e.getValue().stream())
                        .collect(Collectors.toList()))
                .orElse(Collections.emptyList());
    }
}
