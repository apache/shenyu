package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.vo.RuleVO;
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
 * this is rule controller.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@RestController
@RequestMapping("/rule")
public class RuleController {

    private final RuleService ruleService;

    @Autowired(required = false)
    public RuleController(final RuleService ruleService) {
        this.ruleService = ruleService;
    }

    /**
     * query rules.
     *
     * @param selectorId  selector id.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain Mono}
     */
    @GetMapping("")
    public Mono<SoulResult> queryRules(final String selectorId, final Integer currentPage, final Integer pageSize) {
        try {
            CommonPager<RuleVO> commonPager = ruleService.listByPage(new RuleQuery(selectorId, new PageParameter(currentPage, pageSize)));
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("query rules success", commonPager)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("query rules exception")));
        }
    }

    /**
     * detail rule.
     *
     * @param id rule id.
     * @return {@linkplain Mono}
     */
    @GetMapping("/{id}")
    public Mono<SoulResult> detailRule(@PathVariable("id") final String id) {
        try {
            RuleVO ruleVO = ruleService.findById(id);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("detail rule success", ruleVO)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("detail rule exception")));
        }
    }

    /**
     * create rule.
     *
     * @param ruleDTO rule.
     * @return {@linkplain Mono}
     */
    @PostMapping("")
    public Mono<SoulResult> createRule(@RequestBody final RuleDTO ruleDTO) {
        try {
            Integer createCount = ruleService.createOrUpdate(ruleDTO);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("create rule success", createCount)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("create rule exception")));
        }
    }

    /**
     * update rule.
     *
     * @param id      primary key.
     * @param ruleDTO rule.
     * @return {@linkplain Mono}
     */
    @PutMapping("/{id}")
    public Mono<SoulResult> updateRule(@PathVariable("id") final String id, @RequestBody final RuleDTO ruleDTO) {
        try {
            Objects.requireNonNull(ruleDTO);
            ruleDTO.setId(id);
            Integer updateCount = ruleService.createOrUpdate(ruleDTO);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("update rule success", updateCount)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("update rule exception")));
        }
    }

    /**
     * delete rule.
     *
     * @param id primary key.
     * @return {@linkplain Mono}
     */
    @DeleteMapping("/{id}")
    public Mono<SoulResult> deleteRule(@PathVariable("id") final String id) {
        try {
            Integer deleteCount = ruleService.delete(id);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("delete rule success", deleteCount)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("delete rule exception")));
        }
    }
}
