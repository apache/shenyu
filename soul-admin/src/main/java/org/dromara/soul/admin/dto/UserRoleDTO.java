package org.dromara.soul.admin.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * this User Role Dto.
 *
 * @author nuo-promise
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserRoleDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user key.
     */
    private String userId;

    /**
     * role key.
     */
    private String roleId;
}
