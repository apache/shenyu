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

package org.apache.shenyu.admin.service.configs.plan;

import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.springframework.http.HttpStatus;

/**
 * Stable error responses for configuration archive imports.
 */
public final class ConfigsImportErrors {

    /**
     * Stable client-facing message for an invalid archive.
     */
    public static final String INVALID_ARCHIVE = "Import failed: invalid archive";

    private ConfigsImportErrors() {
    }

    /**
     * Build the invalid-archive response.
     *
     * @return error result
     */
    public static ShenyuAdminResult invalidArchive() {
        return ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), INVALID_ARCHIVE);
    }
}
