package org.dromara.soul.admin.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.dromara.soul.admin.entity.UserRoleDO;
import org.dromara.soul.common.utils.DateUtils;

import java.util.Optional;

/**
 * this is user role for web front..
 *
 * @author nuo-promise
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserRoleVO {

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

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build roleDO.
     *
     * @param userRoleDO {@linkplain UserRoleDO}
     * @return {@linkplain UserRoleVO}
     */
    public static UserRoleVO buildUserRoleVO(final UserRoleDO userRoleDO) {
        return Optional.ofNullable(userRoleDO).map(item -> new UserRoleVO(item.getId(), item.getUserId(), item.getRoleId(),
                DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()))).orElse(null);
    }
}
