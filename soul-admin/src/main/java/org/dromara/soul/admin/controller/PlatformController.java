package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.service.EnumService;
import org.dromara.soul.common.result.SoulResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

/**
 * this is platform controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/platform")
public class PlatformController {

    private final EnumService enumService;

    @Autowired(required = false)
    public PlatformController(final EnumService enumService) {
        this.enumService = enumService;
    }

    /**
     * query enums.
     *
     * @return {@linkplain Mono}
     */
    @GetMapping("")
    public Mono<SoulResult> queryEnums() {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(enumService.list())));
    }
}
