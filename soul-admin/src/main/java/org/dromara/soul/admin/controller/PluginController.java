package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.vo.PluginVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

/**
 * this is plugin controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/plugin")
public class PluginController {

    private final PluginService pluginService;

    @Autowired(required = false)
    public PluginController(final PluginService pluginService) {
        this.pluginService = pluginService;
    }

    /**
     * search plugins.
     *
     * @param currentPage current page
     * @param pageSize    page size
     * @return {@linkplain Mono}
     */
    @GetMapping("/search")
    public Mono<CommonPager<PluginVO>> searchPlugins(final Integer currentPage, final Integer pageSize) {
        return Mono.create(commonPager ->
                commonPager.success(pluginService.listByPage(
                        new PluginQuery(new PageParameter(currentPage, pageSize)))));
    }
}
