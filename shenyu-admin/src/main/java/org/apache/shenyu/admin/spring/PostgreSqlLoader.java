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

import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.jdbc.ScriptRunner;
import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

/**
 * for execute schema sql file. initialize the database.
 * PostgreSql library statements cannot be executed in the same transaction block as table statements.
 */
@Component
@ConditionalOnProperty(name = "shenyu.database.dialect", havingValue = "postgresql")
public class PostgreSqlLoader implements InstantiationAwareBeanPostProcessor {

    private static final Logger LOG = LoggerFactory.getLogger(PostgreSqlLoader.class);

    @Resource
    private DataBaseProperties dataBaseProperties;
    
    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, final String beanName) throws BeansException {
        if ((bean instanceof DataSourceProperties) && dataBaseProperties.getInitEnable()) {
            this.init((DataSourceProperties) bean);
        }
        return bean;
    }

    protected void init(final DataSourceProperties properties) {
        try {
            // If jdbcUrl in the configuration file specifies the shenyu database, It will be replaced by postgres,
            // because postgresSql creates database scripts that require the Postgres database to execute,
            // otherwise the shenyu database will be disconnected when the shenyu database does not exist
            String jdbcUrl = StringUtils.replace(properties.getUrl(), "/shenyu", "/");
            Connection conn = DriverManager.getConnection(jdbcUrl, properties.getUsername(), properties.getPassword());
            this.execute(properties, conn);
        } catch (Exception e) {
            LOG.error("Datasource init error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    private void execute(final DataSourceProperties properties, final Connection conn) throws IOException, SQLException {
        ScriptRunner runner = new ScriptRunner(conn);
        try {
            // doesn't print logger
            runner.setLogWriter(null);
            // doesn't print error
            runner.setErrorLogWriter(null);
            runner.setAutoCommit(false);
            runner.setSendFullScript(true);
            Resources.setCharset(StandardCharsets.UTF_8);
            Reader read = this.fillInfoToSqlFile(properties.getUsername(), properties.getPassword());
            runner.runScript(read);
            conn.commit();
        } finally {
            runner.closeConnection();
            conn.close();
        }
    }

    private Reader fillInfoToSqlFile(final String userName, final String password) throws IOException {
        final BufferedReader reader = new BufferedReader(Resources.getResourceAsReader(dataBaseProperties.getInitScript()));
        final StringBuilder builder = new StringBuilder();
        String str;
        while ((str = reader.readLine()) != null) {
            builder.append(str.replace("_user TEXT := 'userName'", "_user TEXT := '" + userName + "'")
                    .replace("_password TEXT := 'password'", "_password TEXT := '" + password + "'"))
                    .append(System.lineSeparator());
        }
        reader.close();
        return new StringReader(builder.toString());
    }
}
