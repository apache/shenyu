package org.dromara.soul.admin.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * @author xiaoyu
 */
@Data
public class AuthPathApplyDTO implements Serializable {

    private String appName;

    private String path;
}
