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
import org.apache.shenyu.common.constant.ResourceTypeConstants;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * this is logon get user permission role.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PermissionMenuVO implements Serializable {

    private static final long serialVersionUID = 8260199815923117085L;

    /**
     * menu list.
     */
    private List<MenuInfo> menu;

    /**
     * current user auth perms.
     */
    private List<AuthPerm> currentAuth;

    /**
     * system all auth perms.
     */
    private List<AuthPerm> allAuth;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class MenuInfo {

        /**
         * menu key.
         */
        private String id;

        /**
         * menu name.
         */
        private String name;

        /**
         * menu url.
         */
        private String url;

        /**
         * menu component.
         */
        private String component;

        /**
         * route meta.
         * web front meta {@linkplain Meta}
         */
        private Meta meta;

        /**
         * menu children.
         */
        private List<MenuInfo> children;

        /**
         * menu sort.
         */
        private Integer sort;

        /**
         * build MenuInfo.
         *
         * @param resourceVO {@linkplain ResourceVO}
         * @return {@linkplain MenuInfo}
         */
        public static MenuInfo buildMenuInfo(final ResourceVO resourceVO) {
            return Optional.ofNullable(resourceVO).map(item -> {
                MenuInfo menuInfo = null;
                if (!resourceVO.getResourceType().equals(ResourceTypeConstants.MENU_TYPE_2)) {
                    menuInfo = MenuInfo.builder()
                            .id(item.getId())
                            .name(item.getName())
                            .url(item.getUrl())
                            .component(item.getComponent())
                            .meta(new Meta(item.getIcon(), item.getTitle()))
                            .sort(item.getSort())
                            .build();
                    if (item.getIsLeaf().equals(Boolean.FALSE)) {
                        menuInfo.setChildren(new ArrayList<>());
                    }
                }
                return menuInfo;
            }).orElse(null);
        }
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class AuthPerm {

        /**
         * perm name.
         */
        private String perms;

        /**
         * description.
         */
        private String description;

        /**
         * resource icon.
         */
        private String icon;

        /**
         * build AuthPerm.
         *
         * @param resourceVO {@linkplain ResourceVO}
         * @return {@linkplain AuthPerm}
         */
        public static AuthPerm buildAuthPerm(final ResourceVO resourceVO) {
            return Optional.ofNullable(resourceVO).map(item -> new AuthPerm(resourceVO.getPerms(), resourceVO.getTitle(), resourceVO.getIcon())).orElse(null);
        }
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Meta {

        /**
         * resource icon.
         */
        private String icon;

        /**
         * resource title.
         */
        private String title;
    }
}
