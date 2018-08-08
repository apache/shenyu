package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.SelectorQuery;
import org.dromara.soul.admin.service.SelectorService;
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
 * this is selector controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/selector")
public class SelectorController {

    private final SelectorService selectorService;

    @Autowired(required = false)
    public SelectorController(final SelectorService selectorService) {
        this.selectorService = selectorService;
    }

    /**
     * query Selectors.
     *
     * @param pluginId    plugin id.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain Mono}
     */
    @GetMapping("")
    public Mono<SoulResult> querySelectors(final String pluginId, final Integer currentPage, final Integer pageSize) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(selectorService.listByPage(
                new SelectorQuery(pluginId, new PageParameter(currentPage, pageSize))))));
    }

    /**
     * detail selector.
     *
     * @param id selector id.
     * @return {@linkplain Mono}
     */
    @GetMapping("/{id}")
    public Mono<SoulResult> detailSelector(@PathVariable("id") final String id) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(selectorService.findById(id))));
    }

    /**
     * create selector.
     *
     * @param selectorDTO selector.
     * @return {@linkplain Mono}
     */
    @PostMapping("")
    public Mono<SoulResult> createSelector(@RequestBody final SelectorDTO selectorDTO) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(selectorService.createOrUpdate(selectorDTO))));
    }

    /**
     * update Selector.
     *
     * @param id          primary key.
     * @param selectorDTO selector.
     * @return {@linkplain Mono}
     */
    @PutMapping("/{id}")
    public Mono<SoulResult> updateSelector(@PathVariable("id") final String id, @RequestBody final SelectorDTO selectorDTO) {
        Objects.requireNonNull(selectorDTO);
        selectorDTO.setId(id);
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(selectorService.createOrUpdate(selectorDTO))));
    }

    /**
     * delete Selector.
     *
     * @param id primary key.
     * @return {@linkplain Mono}
     */
    @DeleteMapping("/{id}")
    public Mono<SoulResult> deleteSelector(@PathVariable("id") final String id) {
        return Mono.create(soulResult -> soulResult.success(SoulResult.success(selectorService.delete(id))));
    }
}
