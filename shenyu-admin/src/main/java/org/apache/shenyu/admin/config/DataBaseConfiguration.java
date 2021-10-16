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

import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
}
