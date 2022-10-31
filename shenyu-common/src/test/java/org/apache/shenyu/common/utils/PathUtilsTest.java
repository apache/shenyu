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

package org.apache.shenyu.common.utils;

import org.apache.shenyu.common.constant.AdminConstants;
import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class PathUtilsTest {
    private static final String URI_WRAPPER = "springCloud/test/**";

    private static final String URI = "springCloud/test";

    @Test
    public void testDecoratorPath() {
        String uri = PathUtils.decoratorPath(URI);
        assertThat(uri, is(URI + AdminConstants.URI_SUFFIX));

        uri = PathUtils.decoratorPath(URI_WRAPPER);
        assertThat(uri, is(URI + AdminConstants.URI_SUFFIX));
    }

    @Test
    public void decoratorContextPath() {
        String uri = PathUtils.decoratorContextPath(URI);
        assertThat(uri, is(URI));

        uri = PathUtils.decoratorContextPath(URI_WRAPPER);
        assertThat(uri, is(URI));
    }
}
