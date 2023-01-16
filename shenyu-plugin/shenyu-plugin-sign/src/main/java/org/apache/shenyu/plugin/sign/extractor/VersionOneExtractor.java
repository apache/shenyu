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

package org.apache.shenyu.plugin.sign.extractor;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.springframework.http.HttpRequest;

import java.net.URI;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_1;

public class VersionOneExtractor implements SignParameterExtractor {
    @Override
    public SignParameters extract(final HttpRequest httpRequest) {
        String appKey = httpRequest.getHeaders().getFirst(Constants.APP_KEY);
        String signature = httpRequest.getHeaders().getFirst(Constants.SIGN);
        String timestamp = httpRequest.getHeaders().getFirst(Constants.TIMESTAMP);
        URI uri = httpRequest.getURI();

        return new SignParameters(VERSION_1, appKey, timestamp, signature, uri);
    }
}
