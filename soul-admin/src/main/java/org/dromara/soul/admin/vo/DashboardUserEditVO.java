package org.dromara.soul.admin.vo;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.util.List;
import java.util.Optional;

/**
 * this is dashboard user for role.
 *
 * @author nuo-promise
 **/
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class DashboardUserEditVO extends DashboardUserVO {

    /**
     * user role list.
     */
    private List<RoleVO> roles;

    /**
     * all role list.
     */
    private List<RoleVO> allRoles;

    /**
     * get edit user info.
     *
     * @param dashboardUserVO {@linkplain DashboardUserVO}
     * @param roles {@linkplain List}
     * @param allRoles {@linkplain List}
     * @return {@linkplain DashboardUserEditVO}
     */
    public static DashboardUserEditVO buildDashboardUserEditVO(final DashboardUserVO dashboardUserVO, final List<RoleVO> roles, final List<RoleVO> allRoles) {
        return Optional.ofNullable(dashboardUserVO).map(item -> {
            DashboardUserEditVO vo = new DashboardUserEditVO();
            BeanUtils.copyProperties(item, vo);
            vo.setRoles(roles);
            vo.setAllRoles(allRoles);
            return vo;
        }).orElse(null);
    }
}
