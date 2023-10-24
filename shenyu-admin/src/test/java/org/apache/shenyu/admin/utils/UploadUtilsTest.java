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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import java.util.Collection;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * UploadUtilsTest.
 */
public class UploadUtilsTest {

    @Test
    public void testListUtil() {
        final MultipartHttpServletRequest httpServletRequest = mock(MultipartHttpServletRequest.class);
        Assertions.assertNotNull(UploadUtils.getUploadFiles(httpServletRequest));
        when(httpServletRequest.getContentType()).thenReturn("multipart");
        final MultiValueMap<String, MultipartFile> fileMultiValueMap = new LinkedMultiValueMap<>();
        fileMultiValueMap.add("file", mock(MultipartFile.class));
        when(httpServletRequest.getMultiFileMap()).thenReturn(fileMultiValueMap);
        final Collection<MultipartFile> uploadFiles = UploadUtils.getUploadFiles(httpServletRequest);
        Assertions.assertTrue(uploadFiles.size() > 0);
    }
}
