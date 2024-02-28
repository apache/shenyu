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

package org.apache.shenyu.plugin.sign.provider;

import com.google.common.collect.ImmutableMap;
import org.apache.shenyu.plugin.sign.api.SignParameters;

import java.util.Map;

import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_1;
import static org.apache.shenyu.plugin.sign.extractor.DefaultExtractor.VERSION_2;

public class DefaultSignProvider implements SignProvider {

    private static final Map<String, SignProvider> VERSION_SIGN =
            ImmutableMap.of(
                    VERSION_1, new VersionOneSignProvider(),
                    VERSION_2, new VersionTwoSignProvider()
            );

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters, final String requestBody) {
        return VERSION_SIGN.get(signParameters.getVersion())
                .generateSign(signKey, signParameters, requestBody);
    }

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters) {
        return VERSION_SIGN.get(signParameters.getVersion())
                .generateSign(signKey, signParameters);
    }
}
