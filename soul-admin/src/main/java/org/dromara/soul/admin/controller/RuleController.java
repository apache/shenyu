package org.dromara.soul.admin.controller;

import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.vo.RuleVO;
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
    public Mono<CommonPager<RuleVO>> queryRules(final String selectorId, final Integer currentPage, final Integer pageSize) {
        return Mono.create(commonPager -> commonPager.success(ruleService.listByPage(
                new RuleQuery(selectorId, new PageParameter(currentPage, pageSize)))));
    }

    /**
     * detail rule.
     *
     * @param id rule id.
     * @return {@linkplain Mono}
     */
    @GetMapping("/{id}")
    public Mono<RuleVO> detailRule(@PathVariable("id") final String id) {
        return Mono.create(commonPager -> commonPager.success(ruleService.findById(id)));
    }

    /**
     * create rule.
     *
     * @param ruleDTO rule.
     * @return {@linkplain Mono}
     */
    @PostMapping("")
    public Mono<Integer> createRule(@RequestBody final RuleDTO ruleDTO) {
        return Mono.create(commonPager -> commonPager.success(ruleService.createOrUpdate(ruleDTO)));
    }

    /**
     * update rule.
     *
     * @param id      primary key.
     * @param ruleDTO rule.
     * @return {@linkplain Mono}
     */
    @PutMapping("/{id}")
    public Mono<Integer> updateRule(@PathVariable("id") final String id, @RequestBody final RuleDTO ruleDTO) {
        Objects.requireNonNull(ruleDTO);
        ruleDTO.setId(id);
        return Mono.create(commonPager -> commonPager.success(ruleService.createOrUpdate(ruleDTO)));
    }

    /**
     * delete rule.
     *
     * @param id primary key.
     * @return {@linkplain Mono}
     */
    @DeleteMapping("/{id}")
    public Mono<Integer> deleteRule(@PathVariable("id") final String id) {
        return Mono.create(commonPager -> commonPager.success(ruleService.delete(id)));
    }
}
