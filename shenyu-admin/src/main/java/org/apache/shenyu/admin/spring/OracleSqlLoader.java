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
import org.apache.ibatis.jdbc.SqlRunner;
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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

/**
 * for execute schema sql file. initialize the database.
 * PostgreSql library statements cannot be executed in the same transaction block as table statements.
 */
@Component
@ConditionalOnProperty(name = "shenyu.database.dialect", havingValue = "oracle")
public class OracleSqlLoader implements InstantiationAwareBeanPostProcessor {

    private static final Logger LOG = LoggerFactory.getLogger(LocalDataSourceLoader.class);

    private static final String PRE_FIX = "file:";

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
            // If jdbcUrl in the configuration file specifies the shenyu database, it is removed,
            // because the shenyu database does not need to be specified when executing the SQL file,
            // otherwise the shenyu database will be disconnected when the shenyu database does not exist
            String jdbcUrl = StringUtils.replace(properties.getUrl(), "/shenyu?", "?");

            Connection connection = DriverManager.getConnection(jdbcUrl, properties.getUsername(), properties.getPassword());
            //Connection connection = DriverManager.getConnection(jdbcUrl, "system","system");
            this.execute(connection, dataBaseProperties.getInitScript());
        } catch (Exception e) {
            LOG.error("Datasource init error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    protected void execute(final Connection conn, final String script) throws Exception {
        ScriptRunner runner = new ScriptRunner(conn);
        try {
            runner.setEscapeProcessing(false);
            runner.setSendFullScript(false);
            runner.setFullLineDelimiter(false);
            runner.setDelimiter("/");
            // doesn't print logger
            runner.setLogWriter(null);
            runner.setAutoCommit(true);
            //runner.setRemoveCRs(true);
            Resources.setCharset(StandardCharsets.UTF_8);
            String sql = fillInfoToSqlFile(script);
            runner.runScript(new StringReader(sql));
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            conn.close();
        }

    }

    private static Reader getResourceAsReader(final String resource) throws IOException {
        return new InputStreamReader(new FileInputStream(resource), StandardCharsets.UTF_8);
    }

    private String fillInfoToSqlFile(final String sqlFile) throws Exception {
        String sql = "";
        try {
            InputStream sqlFileIn = Resources.getResourceAsStream(sqlFile);
            StringBuilder sqlSb = new StringBuilder();
            byte[] buff = new byte[1024];
            int byteRead = 0;
            while ((byteRead = sqlFileIn.read(buff)) != -1) {
                sqlSb.append(new String(buff, 0, byteRead));
            }
            // Windows is \r\n, Linux is \n
            String[] sqlArr = sqlSb.toString().split("(;\\s*\\r\\n)(;\\s*\\n)");
            for (String s : sqlArr) {
                sql = s.replaceAll("--.*", "").trim();
            }
            return sql;
        } catch (Exception ex) {
            throw new Exception(ex.getMessage());
        }
    }
}
