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

package org.apache.shenyu.admin.config;

import org.apache.commons.lang3.StringUtils;
import org.apache.ibatis.mapping.DatabaseIdProvider;
import org.apache.ibatis.mapping.VendorDatabaseIdProvider;
import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.sql.DataSource;
import java.sql.SQLException;
import java.util.Locale;
import java.util.Properties;

/**
 * Local Data Source Configuration.
 */
@Configuration
public class DataBaseConfiguration {
    
    /**
     * Register datasourceProperties for LocalDataSourceLoader.
     *
     * @param dialect database dialect
     * @param initScript database init script
     * @param initEnable database init enable
     * @return {@linkplain DataBaseProperties}
     */
    @Bean
    @ConditionalOnMissingBean(value = DataBaseProperties.class)
    public DataBaseProperties dataBaseProperties(@Value("${shenyu.database.dialect:h2}") final String dialect,
                                                   @Value("${shenyu.database.init_script:sql-script/h2/schema.sql}") final String initScript,
                                                   @Value("${shenyu.database.init_enable:true}") final Boolean initEnable) {
        DataBaseProperties dataSourceProperties = new DataBaseProperties();
        dataSourceProperties.setDialect(dialect);
        dataSourceProperties.setInitScript(initScript);
        dataSourceProperties.setInitEnable(initEnable);
        return dataSourceProperties;
    }

    /**
     * Database dialect configuration.
     *
     * <p>opengauss-jdbc reports the product name as PostgreSQL, so the vendor map would select the
     * postgresql statements. The configured {@code shenyu.database.dialect} is the same value that
     * chooses scripts and interceptors, and it wins.
     *
     * @param dialect configured admin dialect
     * @return {@linkplain DatabaseIdProvider}
     */
    @Bean
    public DatabaseIdProvider databaseIdProvider(@Value("${shenyu.database.dialect:h2}") final String dialect) {
        final Properties properties = new Properties();
        properties.setProperty("Oracle", "oracle");
        properties.setProperty("MySQL", "mysql");
        properties.setProperty("PostgreSQL", "postgresql");
        properties.setProperty("H2", "h2");
        properties.setProperty("OpenGauss", "opengauss");
        properties.setProperty("OceanBase", "oceanbase");
        final VendorDatabaseIdProvider vendor = new VendorDatabaseIdProvider();
        vendor.setProperties(properties);
        return new DialectDatabaseIdProvider(dialect, vendor);
    }

    /**
     * Prefer the configured dialect, and consult the JDBC product name only when it is blank.
     */
    static final class DialectDatabaseIdProvider implements DatabaseIdProvider {

        private final String dialect;

        private final DatabaseIdProvider vendor;

        private DialectDatabaseIdProvider(final String dialect, final DatabaseIdProvider vendor) {
            this.dialect = dialect;
            this.vendor = vendor;
        }

        @Override
        public void setProperties(final Properties properties) {
            vendor.setProperties(properties);
        }

        @Override
        public String getDatabaseId(final DataSource dataSource) throws SQLException {
            if (StringUtils.isBlank(dialect)) {
                return vendor.getDatabaseId(dataSource);
            }
            return dialect.trim().toLowerCase(Locale.ROOT);
        }
    }
}
