package org.dromara.soul.admin.dto;

import java.io.Serializable;
import lombok.Data;

/**
 * The type Spring cloud register dto.
 *
 * @author xiaoyu
 */
@Data
public class SpringCloudRegisterDTO implements Serializable {
    
    private String appName;
    
    private String context;
    
    private String path;
    
    private String pathDesc;
    
    private String rpcType;
    
    private String ruleName;
    
    private boolean enabled;
}
