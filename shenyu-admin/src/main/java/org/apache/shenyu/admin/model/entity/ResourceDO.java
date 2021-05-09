/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.model.entity;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * The Resource Entity.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class ResourceDO extends BaseDO {

    private static final long serialVersionUID = 4663697054300237200L;

    /**
     * resource parent key.
     */
    private String parentId;

    /**
     * resource title.
     */
    private String title;

    /**
     * resource name.
     */
    private String name;

    /**
     * resource url.
     */
    private String url;

    /**
     * resource component.
     */
    private String component;

    /**
     * resource type.
     */
    private Integer resourceType;

    /**
     * resource sort.
     */
    private Integer sort;

    /**
     * resource icon.
     */
    private String icon;

    /**
     * resource is leaf.
     */
    private Boolean isLeaf;

    /**
     * resource is route.
     */
    private Integer isRoute;

    /**
     * resource perms.
     */
    private String perms;

    /**
     * resource status.
     */
    private Integer status;

    /**
     * build ResourceDO.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return {@linkplain ResourceDO}
     */
    public static ResourceDO buildResourceDO(final ResourceDTO resourceDTO) {
        return Optional.ofNullable(resourceDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            ResourceDO resourceDO = ResourceDO.builder()
                    .parentId(item.getParentId())
                    .title(item.getTitle())
                    .name(item.getName())
                    .url(item.getUrl())
                    .component(item.getComponent())
                    .resourceType(item.getResourceType())
                    .sort(item.getSort())
                    .icon(item.getIcon())
                    .isLeaf(item.getIsLeaf())
                    .isRoute(item.getIsRoute())
                    .perms(item.getPerms())
                    .status(item.getStatus())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                resourceDO.setId(UUIDUtils.getInstance().generateShortUuid());
                resourceDO.setDateCreated(currentTime);
            } else {
                resourceDO.setId(item.getId());
            }
            return resourceDO;
        }).orElse(null);
    }
}
