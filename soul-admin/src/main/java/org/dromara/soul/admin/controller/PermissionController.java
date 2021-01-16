package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.PermissionService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.admin.vo.PermissionMenuVO;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;

/**
 * this is permission controller.
 *
 * @author nuo-promise
 **/
@RestController
@RequestMapping("/permission")
public class PermissionController {

    private final PermissionService permissionService;

    public PermissionController(final PermissionService permissionService) {
        this.permissionService = permissionService;
    }

    /**
     * get menu by token.
     *
     * @param token login success ack token
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/getUserPermissionByToken")
    public SoulAdminResult getUserPermissionByToken(@RequestParam(name = "token", required = true) final String token) {
        PermissionMenuVO permissionMenuVO = permissionService.getPermissionMenu(token);
        return Optional.ofNullable(permissionMenuVO).map(item -> SoulAdminResult.success(SoulResultMessage.MENU_SUCCESS, item)).orElse(SoulAdminResult.error(SoulResultMessage.MENU_FAILED));
    }
}
