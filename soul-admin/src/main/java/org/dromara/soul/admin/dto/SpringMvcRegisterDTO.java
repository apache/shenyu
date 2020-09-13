package org.dromara.soul.admin.dto;

import java.io.Serializable;
import lombok.Data;

/**
 * The type Spring mvc register dto.
 *
 * @author xiaoyu
 */
@Data
public class SpringMvcRegisterDTO implements Serializable {
    
    private String appName;
    
    private String context;
    
    private String path;
    
    private String pathDesc;
    
    private String rpcType;
    
    private String host;
    
    private Integer port;
    
    private String ruleName;
    
    private boolean enabled;
    
    private boolean registerMetaData;
}
