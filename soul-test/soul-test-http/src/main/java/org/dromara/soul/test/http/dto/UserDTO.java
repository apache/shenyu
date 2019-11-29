package org.dromara.soul.test.http.dto;

import lombok.Data;

import java.io.Serializable;

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
