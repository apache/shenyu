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

package org.apache.shenyu.admin.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * this is role edit for web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RoleEditVO implements Serializable {

    private static final long serialVersionUID = -292613858092450065L;

    /**
     * role have permission list.
     */
    private List<String> rolePermissionList;

    /**
     * role info.
     */
    private RoleVO sysRole;

    /**
     *  all permission info.
     */
    private PermissionInfo allPermissionInfo;

    @Builder
    @Data
    public static class PermissionInfo {

        /**
         * permission tree list.
         */
        private List<ResourceInfo> treeList;

        /**
         * permission ids.
         */
        private List<String> permissionIds;
    }

    @Builder
    @Data
    public static class ResourceInfo {

        /**
         * resource id.
         */
        private String id;

        /**
         * resource title.
         */
        private String title;

        /**
         * resource name.
         */
        private String name;

        /**
         * resource children.
         */
        private List<ResourceInfo> children;

        /**
         * resource leaf.
         */
        private Boolean isLeaf;

        /**
         * resource parentId.
         */
        private String parentId;

        /**
         * build resource info.
         *
         * @param resourceVO {@linkplain ResourceVO}
         * @return {@linkplain ResourceInfo}
         */
        public static ResourceInfo buildResourceInfo(final ResourceVO resourceVO) {
            return Optional.ofNullable(resourceVO).map(item -> {
                ResourceInfo resourceInfo = ResourceInfo.builder()
                        .id(item.getId())
                        .title(item.getTitle())
                        .name(item.getName())
                        .parentId(item.getParentId())
                        .isLeaf(item.getIsLeaf())
                        .build();
                if (item.getIsLeaf().equals(Boolean.FALSE)) {
                    resourceInfo.setChildren(new ArrayList<>());
                }
                return resourceInfo;
            }).orElse(null);
        }
    }

}
