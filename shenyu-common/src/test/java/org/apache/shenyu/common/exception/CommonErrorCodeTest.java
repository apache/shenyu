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

package org.apache.shenyu.common.exception;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/**
 * Test case for {@link CommonErrorCode}.
 */
public final class CommonErrorCodeTest {

    @Test
    public void testCommonErrorCode() {
        assertThat(CommonErrorCode.SUCCESSFUL, is(200));
        assertThat(CommonErrorCode.ERROR, is(500));
        assertThat(CommonErrorCode.TOKEN_ERROR, is(600));
        assertThat(CommonErrorCode.TOKEN_NO_PERMISSION, is(601));
        assertThat(CommonErrorCode.NOT_FOUND_EXCEPTION, is(404));
    }
}
