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
import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.sql.Connection;
import java.sql.DriverManager;

/**
 * for execute schema sql file. initialize the database.
 * PostgreSql library statements cannot be executed in the same transaction block as table statements.
 */
@Component
public class PostgreSqlLoader extends ScriptLoader implements InstantiationAwareBeanPostProcessor {

    private static final Logger LOG = LoggerFactory.getLogger(PostgreSqlLoader.class);

    @Resource
    private DataBaseProperties dataBaseProperties;
    
    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, final String beanName) throws BeansException {
        if ((bean instanceof DataSourceProperties) && dataBaseProperties.getDialect().equals("postgresql")) {
            this.init((DataSourceProperties) bean);
        }
        return bean;
    }

    protected void init(final DataSourceProperties properties) {
        try {
            // If jdbcUrl in the configuration file specifies the shenyu database, It will be replaced by postgres,
            // because postgresSql creates database scripts that require the Postgres database to execute,
            // otherwise the shenyu database will be disconnected when the shenyu database does not exist
            String jdbcUrl = StringUtils.replace(properties.getUrl(), "/shenyu", "/postgres");
            Connection connection = DriverManager.getConnection(jdbcUrl, properties.getUsername(), properties.getPassword());
            super.execute(connection, dataBaseProperties.getDbScript());
        } catch (Exception e) {
            LOG.error("Datasource init error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }
}
