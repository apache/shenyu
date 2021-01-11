package org.dromara.soul.admin.service;

import org.dromara.soul.admin.vo.PermissionMenuVO;

import java.util.Set;

/**
 * this is permission service.
 *
 * @author nuo-promise
 **/
public interface PermissionService {

    /**
     * get user permission menu by token.
     *
     * @param token logon ack token.
     * @return {@linkplain PermissionMenuVO}
     */
    PermissionMenuVO getPermissionMenu(String token);

    /**
     * get AuthPerm By UserName.
     *
     * @param userName user name.
     * @return {@linkplain Set}
     */
    Set<String> getAuthPermByUserName(String userName);
}
