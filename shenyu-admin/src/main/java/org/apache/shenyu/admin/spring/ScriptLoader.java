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

package org.apache.shenyu.admin.spring;

import com.google.common.base.Splitter;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.jdbc.ScriptRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.util.List;

/**
 * The base class that initializes the script loader.
 */
public class ScriptLoader {

    private static final Logger LOG = LoggerFactory.getLogger(ScriptLoader.class);

    private static final String PRE_FIX = "file:";

    protected void execute(final Connection conn, final String script) throws Exception {
        ScriptRunner runner = new ScriptRunner(conn);
        try {
            // doesn't print logger
            runner.setLogWriter(null);
            runner.setAutoCommit(true);
            Resources.setCharset(StandardCharsets.UTF_8);
            List<String> initScripts = Splitter.on(";").splitToList(script);
            for (String sqlScript : initScripts) {
                if (sqlScript.startsWith(PRE_FIX)) {
                    String sqlFile = sqlScript.substring(PRE_FIX.length());
                    try (Reader fileReader = getResourceAsReader(sqlFile)) {
                        LOG.info("execute shenyu schema sql: {}", sqlFile);
                        runner.runScript(fileReader);
                    }
                } else {
                    try (Reader fileReader = Resources.getResourceAsReader(sqlScript)) {
                        LOG.info("execute shenyu schema sql: {}", sqlScript);
                        runner.runScript(fileReader);
                    }

                }
            }
        } finally {
            runner.closeConnection();
        }
    }

    private static Reader getResourceAsReader(final String resource) throws IOException {
        return new InputStreamReader(new FileInputStream(resource), StandardCharsets.UTF_8);
    }

}
