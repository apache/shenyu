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

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.plugin.sign.api.SignParameters;
import org.springframework.http.HttpRequest;

import java.util.Map;
import java.util.Objects;

public class DefaultExtractor implements SignParameterExtractor {

    public static final String VERSION_1 = "1.0.0";

    public static final String VERSION_2 = "2.0.0";

    private static final Map<String, SignParameterExtractor> VERSION_EXTRACTOR =
            ImmutableMap.of(
                    VERSION_1, new VersionOneExtractor(),
                    VERSION_2, new VersionTwoExtractor()
            );

    @Override
    public SignParameters extract(final HttpRequest httpRequest) {

        String version = httpRequest.getHeaders().getFirst(Constants.VERSION);
        if (Objects.isNull(version)) {
            return SignParameters.VERSION_ERROR_PARAMETERS;
        }

        SignParameterExtractor extractor = VERSION_EXTRACTOR.get(version);

        if (Objects.isNull(extractor)) {
            return SignParameters.VERSION_ERROR_PARAMETERS;
        }

        return extractor.extract(httpRequest);
    }
}
