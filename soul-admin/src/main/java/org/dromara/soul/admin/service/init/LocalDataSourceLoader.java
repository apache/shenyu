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

package org.dromara.soul.admin.service.init;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.jdbc.ScriptRunner;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;

/**
 * for execute schema sql file.
 *
 * @author huangxiaofeng
 */
@Slf4j
@Component
public class LocalDataSourceLoader implements InstantiationAwareBeanPostProcessor {

    private @Value("${soul.database.init_script:META-INF/schema.h2.sql}") String schemaSQLFile;

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, final String beanName) throws BeansException {
        if (bean instanceof DataSourceProperties) {
            this.init((DataSourceProperties) bean);
        }
        return bean;
    }

    @SneakyThrows
    protected void init(final DataSourceProperties properties) {
        // If jdbcUrl in the configuration file specifies the soul database, it is removed,
        // because the soul database does not need to be specified when executing the SQL file,
        // otherwise the soul database will be disconnected when the soul database does not exist
        String jdbcUrl = StringUtils.replace(properties.getUrl(), "/soul?", "?");
        Connection connection = DriverManager.getConnection(jdbcUrl, properties.getUsername(), properties.getPassword());
        this.execute(connection);

    }

    private void execute(final Connection conn) throws Exception {
        ScriptRunner runner = new ScriptRunner(conn);
        // doesn't print logger
        runner.setLogWriter(null);
        Resources.setCharset(StandardCharsets.UTF_8);
        Reader read = Resources.getResourceAsReader(schemaSQLFile);
        log.info("execute soul schema sql: {}", schemaSQLFile);
        runner.runScript(read);
        runner.closeConnection();
        conn.close();
    }

}
