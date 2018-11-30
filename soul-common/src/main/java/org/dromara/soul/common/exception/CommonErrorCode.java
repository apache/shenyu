/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.common.exception;

import org.apache.commons.lang3.StringUtils;

/**
 * CommonErrorCode.
 *
 * @author xiaoyu
 */
public class CommonErrorCode {

    /**
     * The constant ERROR.
     */
    public static final int ERROR = -2;

    /**
     * The constant SUCCESSFUL.
     */
    public static final int SUCCESSFUL = 200;

    /**
     * The constant PARAMS_ERROR.
     */
    public static final int PARAMS_ERROR = 10000002;

    /**
     * getErrorMsg.
     *
     * @param code code
     * @return msg error msg
     */
    public static String getErrorMsg(final int code) {
        String msg = System.getProperty(String.valueOf(code));
        if (StringUtils.isBlank(msg)) {
            return " code [" + code + "] not match msg.";
        }
        return msg;
    }
}
