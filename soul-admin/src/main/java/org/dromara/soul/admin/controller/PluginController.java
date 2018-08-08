package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.common.result.SoulResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.DeleteMapping;
import reactor.core.publisher.Mono;

import java.util.Objects;

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
     * query plugins.
     *
     * @param name        plugin name.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain Mono}
     */
    @GetMapping("")
    public Mono<SoulResult> queryPlugins(final String name, final Integer currentPage, final Integer pageSize) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(pluginService.listByPage(
                new PluginQuery(name, new PageParameter(currentPage, pageSize))))));
    }

    /**
     * detail plugin.
     *
     * @param id plugin id.
     * @return {@linkplain Mono}
     */
    @GetMapping("/{id}")
    public Mono<SoulResult> detailPlugin(@PathVariable("id") final String id) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(pluginService.findById(id))));
    }

    /**
     * create plugin.
     *
     * @param pluginDTO plugin.
     * @return {@linkplain Mono}
     */
    @PostMapping("")
    public Mono<SoulResult> createPlugin(@RequestBody final PluginDTO pluginDTO) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(pluginService.createOrUpdate(pluginDTO))));
    }

    /**
     * update plugin.
     *
     * @param id        primary key.
     * @param pluginDTO plugin.
     * @return {@linkplain Mono}
     */
    @PutMapping("/{id}")
    public Mono<SoulResult> updatePlugin(@PathVariable("id") final String id, @RequestBody final PluginDTO pluginDTO) {
        Objects.requireNonNull(pluginDTO);
        pluginDTO.setId(id);
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(pluginService.createOrUpdate(pluginDTO))));
    }

    /**
     * delete plugin.
     *
     * @param id primary key.
     * @return {@linkplain Mono}
     */
    @DeleteMapping("/{id}")
    public Mono<SoulResult> deletePlugin(@PathVariable("id") final String id) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(pluginService.delete(id))));
    }
}
