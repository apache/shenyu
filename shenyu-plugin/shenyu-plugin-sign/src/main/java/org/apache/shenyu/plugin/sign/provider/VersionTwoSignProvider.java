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

import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.plugin.sign.api.SignParameters;

import java.net.URI;
import java.util.Objects;
import java.util.Optional;

public class VersionTwoSignProvider implements SignProvider {

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters, final String requestBody) {

        String data = signParameters.getParameters()
                + getRelativeURL(signParameters.getUri())
                + Optional.ofNullable(requestBody).orElse("");
        return SignUtils.sign(signParameters.getSignAlg(), signKey, data).toUpperCase();
    }

    @Override
    public String generateSign(final String signKey, final SignParameters signParameters) {
        return generateSign(signKey, signParameters, null);
    }

    private String getRelativeURL(final URI uri) {
        if (Objects.isNull(uri.getQuery())) {
            return uri.getPath();
        }
        return uri.getPath() + "?" + uri.getQuery();
    }
}
