package org.dromara.soul.common.dto;

import lombok.Data;
import lombok.ToString;

import java.io.Serializable;

@Data
@ToString
public class MetaData implements Serializable {
    
    private String id;
    
    private String appName;
    
    private String contextPath;
    
    private String path;
    
    private String rpcType;
    
    private String serviceName;
    
    private String methodName;
    
    private String parameterTypes;
    
    private String rpcExt;
    
    private Boolean enabled;
}
