package org.dromara.soul.common.dto;

import lombok.Data;

import java.io.Serializable;

@Data
public class MetaData implements Serializable {

    private String appName;

    private String path;

    private String rpcType;

    private String serviceName;

    private String methodName;

    private String parameterTypes;

    private String rpcExt;

    private Boolean enabled;
}
