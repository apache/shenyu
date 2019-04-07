/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.controller;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.BatchCommonDTO;
import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.vo.PluginVO;
import org.dromara.soul.common.result.SoulResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.util.List;
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

    /**
     * Instantiates a new Plugin controller.
     *
     * @param pluginService the plugin service
     */
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
        try {
            CommonPager<PluginVO> commonPager = pluginService.listByPage(new PluginQuery(name, new PageParameter(currentPage, pageSize)));
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("query plugins success", commonPager)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("query plugins exception")));
        }
    }

    /**
     * detail plugin.
     *
     * @param id plugin id.
     * @return {@linkplain Mono}
     */
    @GetMapping("/{id}")
    public Mono<SoulResult> detailPlugin(@PathVariable("id") final String id) {
        try {
            PluginVO pluginVO = pluginService.findById(id);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("detail plugin success", pluginVO)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("detail plugin exception")));
        }
    }

    /**
     * create plugin.
     *
     * @param pluginDTO plugin.
     * @return {@linkplain Mono}
     */
    @PostMapping("")
    public Mono<SoulResult> createPlugin(@RequestBody final PluginDTO pluginDTO) {
        try {
            String result = pluginService.createOrUpdate(pluginDTO);
            if (StringUtils.isNoneBlank()) {
                return Mono.create(soulResult -> soulResult.success(SoulResult.error(result)));
            }
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("create plugin success")));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("create plugin exception")));
        }
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
        try {
            Objects.requireNonNull(pluginDTO);
            pluginDTO.setId(id);
            final String result = pluginService.createOrUpdate(pluginDTO);
            if (StringUtils.isNoneBlank(result)) {
                return Mono.create(soulResult -> soulResult.success(SoulResult.error(result)));
            }
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("update plugin success")));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("update plugin exception")));
        }
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return {@linkplain Mono}
     */
    @DeleteMapping("/batch")
    public Mono<SoulResult> deletePlugins(@RequestBody final List<String> ids) {
        try {
            final String result = pluginService.delete(ids);
            if (StringUtils.isNoneBlank(result)) {
                return Mono.create(soulResult -> soulResult.success(SoulResult.error(result)));
            }
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("delete plugins success")));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("delete plugins exception")));
        }
    }


    /**
     * Enable mono.
     *
     * @param batchCommonDTO the batch common dto
     * @return the mono
     */
    @PostMapping("/enabled")
    public Mono<SoulResult> enabled(@RequestBody final BatchCommonDTO batchCommonDTO) {
        try {
            final String result = pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
            if (StringUtils.isNoneBlank(result)) {
                return Mono.create(soulResult -> soulResult.success(SoulResult.error(result)));
            }
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("enable plugins success")));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("enable plugins exception")));
        }
    }


    /**
     * sync plugins.
     *
     * @return {@linkplain Mono}
     */
    @PostMapping("/syncPluginAll")
    public Mono<SoulResult> syncPluginAll() {
        try {
            Integer syncCount = pluginService.syncPluginAll();
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("sync plugins success", syncCount)));
        } catch (Exception e) {
            e.printStackTrace();
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("sync plugins exception")));
        }
    }


    /**
     * Sync plugin data mono.
     *
     * @param id the id
     * @return the mono
     */
    @PutMapping("/syncPluginData/{id}")
    public Mono<SoulResult> syncPluginData(@PathVariable("id") final String id) {
        try {
            Integer syncCount = pluginService.syncPluginData(id);
            return Mono.create(soulResult -> soulResult.success(SoulResult.success("sync plugins success", syncCount)));
        } catch (Exception e) {
            return Mono.create(soulResult -> soulResult.success(SoulResult.error("sync plugins exception")));
        }
    }
}
