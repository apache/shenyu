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

import java.sql.PreparedStatement;
import java.sql.SQLException;

import org.apache.shenyu.common.utils.UUIDUtils;

import org.springframework.util.ObjectUtils;

/**
 * Trigger Base Utils.
 */
public class BaseTrigger {

    /**
     * SQL execute.
     * @param statement {@link PreparedStatement}
     * @param newRow {@link Object}
     * @throws SQLException {@link SQLException}
     */
    public static void sqlExecute(final Object[] newRow, final PreparedStatement statement) throws SQLException {
        if (ObjectUtils.isEmpty(newRow[0])) {
            statement.setObject(1, UUIDUtils.getInstance().generateShortUuid());
            for (int i = 1; i < newRow.length - 2; i++) {
                statement.setObject(i + 1, newRow[i]);
            }
            statement.executeUpdate();
        }
    }
}
