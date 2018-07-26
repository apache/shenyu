package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.vo.PluginVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

/**
 * this is plugin controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/plugin")
public class PluginController {

    @Autowired
    private PluginService pluginService;

    @GetMapping("")
    public Mono<CommonPager<PluginVO>> get(Integer currentPage, Integer pageSize) {
        return Mono.create(commonPager -> commonPager.success(pluginService.listByPage(
                new PluginQuery(new PageParameter(currentPage, pageSize)))));
    }
}
