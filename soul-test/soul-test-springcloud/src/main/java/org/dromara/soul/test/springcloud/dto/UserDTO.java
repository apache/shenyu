package org.dromara.soul.test.springcloud.dto;

import java.io.Serializable;
import lombok.Data;

/**
 * The type User dto.
 *
 * @author xiaoyu(Myth)
 */
@Data
public class UserDTO implements Serializable {

    private String userId;
    
    private String userName;
}
