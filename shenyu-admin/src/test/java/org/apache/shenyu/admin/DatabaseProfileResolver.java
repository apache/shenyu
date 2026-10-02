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

package org.apache.shenyu.admin;

import org.springframework.test.context.ActiveProfilesResolver;

/**
 * Resolves the database profile used by admin integration tests.
 * The optional {@code -Dshenyu.test.database.profile} system property selects a profile; H2 is used by default.
 */
public final class DatabaseProfileResolver implements ActiveProfilesResolver {

    static final String PROFILE_PROPERTY = "shenyu.test.database.profile";

    static final String DEFAULT_PROFILE = "h2";

    @Override
    public String[] resolve(final Class<?> testClass) {
        return new String[] {System.getProperty(PROFILE_PROPERTY, DEFAULT_PROFILE)};
    }
}
