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
import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.jdbc.ScriptRunner;
import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import jakarta.annotation.Resource;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.util.List;

/**
 * for execute schema sql file.
 */
@Component
@ConditionalOnExpression("'${shenyu.database.dialect}' == 'h2'")
public class LocalDataSourceLoader implements InstantiationAwareBeanPostProcessor {

    private static final Logger LOG = LoggerFactory.getLogger(LocalDataSourceLoader.class);

    private static final String PRE_FIX = "file:";

    @Resource
    private DataBaseProperties dataBaseProperties;

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        if (bean instanceof DataSourceProperties && Boolean.TRUE.equals(dataBaseProperties.getInitEnable())) {
            this.init((DataSourceProperties) bean);
        }
        return bean;
    }

    protected void init(final DataSourceProperties properties) {
        try {
            // If jdbcUrl in the configuration file specifies the shenyu database, it is removed,
            // because the shenyu database does not need to be specified when executing the SQL file,
            // otherwise the shenyu database will be disconnected when the shenyu database does not exist
            String jdbcUrl = StringUtils.replace(properties.getUrl(), "/shenyu?", "?");
            try (Connection connection = DriverManager.getConnection(jdbcUrl, properties.getUsername(), properties.getPassword())) {
                this.execute(connection, dataBaseProperties.getInitScript());
            }
        } catch (Exception e) {
            LOG.error("Datasource init error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    protected void execute(final Connection conn, final String script) throws Exception {
        ScriptRunner runner = new ScriptRunner(conn);
        // doesn't print logger
        runner.setLogWriter(null);
        runner.setAutoCommit(true);
        runner.setFullLineDelimiter(false);
        runner.setSendFullScript(false);
        runner.setStopOnError(false);
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
        // Note: Connection will be automatically closed by try-with-resources in init() method
        // No need to call runner.closeConnection() here to avoid double closing
    }

    private static Reader getResourceAsReader(final String resource) throws IOException {
        return new InputStreamReader(Files.newInputStream(Paths.get(resource)), StandardCharsets.UTF_8);
    }
}
