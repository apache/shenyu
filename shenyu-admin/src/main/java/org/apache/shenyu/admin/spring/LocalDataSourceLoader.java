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

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.jdbc.ScriptRunner;
import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;

/**
 * for execute schema sql file.
 */
@Slf4j
@Component
public class LocalDataSourceLoader implements InstantiationAwareBeanPostProcessor {

    @Resource
    private DataBaseProperties dataBaseProperties;

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, final String beanName) throws BeansException {
        if ((bean instanceof DataSourceProperties) && dataBaseProperties.getInitEnable()) {
            this.init((DataSourceProperties) bean);
        }
        return bean;
    }

    @SneakyThrows
    protected void init(final DataSourceProperties properties) {
        // If jdbcUrl in the configuration file specifies the shenyu database, it is removed,
        // because the shenyu database does not need to be specified when executing the SQL file,
        // otherwise the shenyu database will be disconnected when the shenyu database does not exist
        String jdbcUrl = StringUtils.replace(properties.getUrl(), "/shenyu?", "?");
        Connection connection = DriverManager.getConnection(jdbcUrl, properties.getUsername(), properties.getPassword());
        this.execute(connection);

    }

    private void execute(final Connection conn) throws Exception {
        ScriptRunner runner = new ScriptRunner(conn);
        // doesn't print logger
        runner.setLogWriter(null);
        Resources.setCharset(StandardCharsets.UTF_8);

        Reader read = StringUtils.isNotBlank(dataBaseProperties.getCustomInitScript()) ? getResourceAsReader(dataBaseProperties.getCustomInitScript())
                : Resources.getResourceAsReader(dataBaseProperties.getInitScript());

        log.info("execute shenyu schema sql: {}", StringUtils.isNotBlank(dataBaseProperties.getCustomInitScript()) ? dataBaseProperties.getCustomInitScript()
                : dataBaseProperties.getInitScript());
        runner.runScript(read);
        runner.closeConnection();
        conn.close();
    }

    /**
     * resource reader.
     *
     * @param resource url
     * @return Reader
     * @throws IOException IOException
     */
    public static Reader getResourceAsReader(final String resource) throws IOException {
        return getResourceAsReader(resource, null);
    }

    /**
     * resource reader.
     *
     * @param resource url
     * @param charset charset
     * @return Reader
     * @throws IOException IOException
     */
    public static Reader getResourceAsReader(final String resource, final Charset charset) throws IOException {
        return new InputStreamReader(new FileInputStream(resource), charset == null ? StandardCharsets.UTF_8 : charset);
    }

}
