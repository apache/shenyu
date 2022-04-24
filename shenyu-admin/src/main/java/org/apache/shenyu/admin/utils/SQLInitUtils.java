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

package org.apache.shenyu.admin.utils;

import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.UUIDUtils;

/**
 * The Shenyu SQLInitUtils.
 */
public class SQLInitUtils {

    /**
     * Connecting sql execution statements.
     * @param originStr  origin sql
     * @param regxValue  add character
     * @param sqlType   sql type
     * @return {@linkplain String}
     */
    public static String concatCharacter(final String originStr, final String regxValue, final String sqlType) {
        StringBuilder str;
        String[] splitStr = originStr.split("\\(");
        if (sqlType.equals(AdminConstants.SQL_TYPE_H2) || sqlType.equals(AdminConstants.SQL_TYPE_MYSQL)) {
            str = new StringBuilder(splitStr[0] + "(`" + regxValue + "`," + splitStr[1] + "('"
                    + UUIDUtils.getInstance().generateShortUuid() + "'," + splitStr[2]);
            if (splitStr.length > 3) {
                for (int i = 3; i < splitStr.length; i++) {
                    str.append(splitStr[i]);
                }
            }
        } else {

            str = new StringBuilder(splitStr[0] + "(" + splitStr[1] + " ( " + regxValue + "," + splitStr[2] + "(''' || '"
                    + UUIDUtils.getInstance().generateShortUuid() + "' || '''," + splitStr[3]);
            if (splitStr.length > 4) {
                for (int i = 4; i < splitStr.length; i++) {
                    str.append(splitStr[4]);
                }
            }
        }
        return str.toString();
    }
}
