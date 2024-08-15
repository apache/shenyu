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

package org.apache.shenyu.admin.model.result;

import java.io.Serializable;

/**
 * ConfigImportResult.
 */
public class ConfigImportResult implements Serializable {

    private static final long serialVersionUID = 7527987507527292299L;

    private final Integer successCount;

    private final String failMessage;


    /**
     * Instantiates a new config import result.
     *
     * @param successCount success count
     * @param failMessage  fail message
     */
    public ConfigImportResult(final Integer successCount, final String failMessage) {
        this.successCount = successCount;
        this.failMessage = failMessage;
    }

    /**
     * return success.
     *
     * @return {@linkplain ConfigImportResult}
     */
    public static ConfigImportResult success() {
        return success(0);
    }

    /**
     * return success.
     * @param successCount success count
     * @return {@linkplain ConfigImportResult}
     */
    public static ConfigImportResult success(final Integer successCount) {
        return new ConfigImportResult(successCount, "");
    }

    /**
     * return success.
     *
     * @param successCount success count
     * @param failMessage fail message
     * @return {@linkplain ConfigImportResult}
     */
    public static ConfigImportResult fail(final Integer successCount, final String failMessage) {
        return new ConfigImportResult(successCount, failMessage);
    }

    /**
     * Gets the success count.
     *
     * @return the success count
     */
    public Integer getSuccessCount() {
        return successCount;
    }

    /**
     * Gets the fail message.
     *
     * @return the fail message
     */
    public String getFailMessage() {
        return failMessage;
    }

}
