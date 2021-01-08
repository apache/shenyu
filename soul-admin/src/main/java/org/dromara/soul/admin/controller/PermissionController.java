package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.result.SoulAdminResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * this is permission controller.
 *
 * @author nuo-promise
 **/
@RestController
@RequestMapping("/permission")
public class PermissionController {

    /**
     * get menu by token.
     *
     * @param token login success ack token
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/getUserPermissionByToken")
    public SoulAdminResult getUserPermissionByToken(@RequestParam(name = "token", required = true) final String token) {
        return null;
    }
}
