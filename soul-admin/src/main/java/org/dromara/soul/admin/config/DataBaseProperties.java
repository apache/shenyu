package org.dromara.soul.admin.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Local DataSource configuration.
 *
 * @author nuo-promise
 */
@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "soul.database")
public class DataBaseProperties {

    private String dialect;

    private String initScript;

    private Boolean initEnable;
}
