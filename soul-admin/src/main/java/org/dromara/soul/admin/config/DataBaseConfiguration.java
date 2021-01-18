package org.dromara.soul.admin.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Local Data Source Configuration.
 *
 * @author nuo-promise
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
    public DataBaseProperties dataBaseProperties(@Value("${soul.database.dialect:h2}") final String dialect,
                                                   @Value("${soul.database.init_script:META-INF/schema.h2.sql}") final String initScript,
                                                   @Value("${soul.database.init_enable:true}") final Boolean initEnable) {
        DataBaseProperties dataSourceProperties = new DataBaseProperties();
        dataSourceProperties.setDialect(dialect);
        dataSourceProperties.setInitScript(initScript);
        dataSourceProperties.setInitEnable(initEnable);
        return dataSourceProperties;
    }
}
