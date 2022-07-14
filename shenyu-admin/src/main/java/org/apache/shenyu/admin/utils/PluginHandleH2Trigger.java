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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import org.apache.shenyu.common.exception.ShenyuException;
import org.h2.api.Trigger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Plugin Handle H2 Trigger.
 */
public class PluginHandleH2Trigger implements Trigger {

    private static final Logger LOG = LoggerFactory.getLogger(PluginHandleH2Trigger.class);

    @Override
    public void init(final Connection connection, final String s, final String s1, final String s2, final boolean b, final int i) throws SQLException {

    }

    @Override
    public void fire(final Connection connection, final Object[] oldRow, final Object[] newRow) throws SQLException {
        try (PreparedStatement statement = connection.prepareStatement(
                "INSERT IGNORE INTO PLUGIN_HANDLE (`ID`,`PLUGIN_ID`,`FIELD`,`LABEL`,`DATA_TYPE`,`TYPE`,`SORT`,`EXT_OBJ`)"
                        + " VALUES ( ?, ?, ?, ?, ?, ?, ?, ?)")) {
            BaseTrigger.sqlExecute(newRow, statement);
        } catch (ShenyuException e) {
            LOG.error("PluginHandleH2Trigger Error:", e);
        }
    }

    @Override
    public void close() throws SQLException {
    }

    @Override
    public void remove() throws SQLException {
    }
}
