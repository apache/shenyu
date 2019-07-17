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
import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.admin.vo.PluginVO;
import org.dromara.soul.common.enums.DataEventTypeEnum;
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

    private final SyncDataService syncDataService;

    /**
     * Instantiates a new Plugin controller.
     *
     * @param pluginService   the plugin service
     * @param syncDataService the sync data service
     */
    @Autowired(required = false)
    public PluginController(final PluginService pluginService,
                            final SyncDataService syncDataService) {
        this.pluginService = pluginService;
        this.syncDataService = syncDataService;
    }

    /**
     * query plugins.
     *
     * @param name        plugin name.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @return {@linkplain SoulResult}
     */
    @GetMapping("")
    public SoulResult queryPlugins(final String name, final Integer currentPage, final Integer pageSize) {
        try {
            CommonPager<PluginVO> commonPager = pluginService.listByPage(new PluginQuery(name, new PageParameter(currentPage, pageSize)));
            return SoulResult.success("query plugins success", commonPager);
        } catch (Exception e) {
            return SoulResult.error("query plugins exception");
        }
    }

    /**
     * detail plugin.
     *
     * @param id plugin id.
     * @return {@linkplain SoulResult}
     */
    @GetMapping("/{id}")
    public SoulResult detailPlugin(@PathVariable("id") final String id) {
        try {
            PluginVO pluginVO = pluginService.findById(id);
            return SoulResult.success("detail plugin success", pluginVO);
        } catch (Exception e) {
            return SoulResult.error("detail plugin exception");
        }
    }

    /**
     * create plugin.
     *
     * @param pluginDTO plugin.
     * @return {@linkplain SoulResult}
     */
    @PostMapping("")
    public SoulResult createPlugin(@RequestBody final PluginDTO pluginDTO) {
        try {
            String result = pluginService.createOrUpdate(pluginDTO);
            if (StringUtils.isNoneBlank()) {
                return SoulResult.error(result);
            }
            return SoulResult.success("create plugin success");
        } catch (Exception e) {
            return SoulResult.error("create plugin exception");
        }
    }

    /**
     * update plugin.
     *
     * @param id        primary key.
     * @param pluginDTO plugin.
     * @return {@linkplain SoulResult}
     */
    @PutMapping("/{id}")
    public SoulResult updatePlugin(@PathVariable("id") final String id, @RequestBody final PluginDTO pluginDTO) {
        try {
            Objects.requireNonNull(pluginDTO);
            pluginDTO.setId(id);
            final String result = pluginService.createOrUpdate(pluginDTO);
            if (StringUtils.isNoneBlank(result)) {
                return SoulResult.error(result);
            }
            return SoulResult.success("update plugin success");
        } catch (Exception e) {
            return SoulResult.error("update plugin exception");
        }
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return {@linkplain SoulResult}
     */
    @DeleteMapping("/batch")
    public SoulResult deletePlugins(@RequestBody final List<String> ids) {
        try {
            final String result = pluginService.delete(ids);
            if (StringUtils.isNoneBlank(result)) {
                return SoulResult.error(result);
            }
            return SoulResult.success("delete plugins success");
        } catch (Exception e) {
            return SoulResult.error("delete plugins exception");
        }
    }


    /**
     * Enable mono.
     *
     * @param batchCommonDTO the batch common dto
     * @return the mono
     */
    @PostMapping("/enabled")
    public SoulResult enabled(@RequestBody final BatchCommonDTO batchCommonDTO) {
        try {
            final String result = pluginService.enabled(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
            if (StringUtils.isNoneBlank(result)) {
                return SoulResult.error(result);
            }
            return SoulResult.success("enable plugins success");
        } catch (Exception e) {
            return SoulResult.error("enable plugins exception");
        }
    }


    /**
     * sync plugins.
     *
     * @return {@linkplain SoulResult}
     */
    @PostMapping("/syncPluginAll")
    public SoulResult syncPluginAll() {
        try {
            boolean success = syncDataService.syncAll(DataEventTypeEnum.REFRESH);
            if (success) {
                return SoulResult.success("sync plugins success");
            } else {
                return SoulResult.success("sync plugins fail");
            }
        } catch (Exception e) {
            return SoulResult.error("sync plugins exception");
        }
    }


    /**
     * Sync plugin data.
     *
     * @param id the id
     * @return the mono
     */
    @PutMapping("/syncPluginData/{id}")
    public SoulResult syncPluginData(@PathVariable("id") final String id) {
        try {
            boolean success = syncDataService.syncPluginData(id);
            if (success) {
                return SoulResult.success("sync plugins success");
            } else {
                return SoulResult.success("sync plugins fail");
            }
        } catch (Exception e) {
            e.printStackTrace();
            return SoulResult.error("sync plugins exception{}");
        }
    }
}
