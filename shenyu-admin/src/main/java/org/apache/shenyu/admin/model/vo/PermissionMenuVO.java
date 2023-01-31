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

import org.apache.shenyu.common.constant.ResourceTypeConstants;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * this is logon get user permission role.
 */
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

    public PermissionMenuVO() {
    }

    public PermissionMenuVO(final List<MenuInfo> menu, final List<AuthPerm> currentAuth, final List<AuthPerm> allAuth) {
        this.menu = menu;
        this.currentAuth = currentAuth;
        this.allAuth = allAuth;
    }

    /**
     * Gets the value of menu.
     *
     * @return the value of menu
     */
    public List<MenuInfo> getMenu() {
        return menu;
    }

    /**
     * Sets the menu.
     *
     * @param menu menu
     */
    public void setMenu(final List<MenuInfo> menu) {
        this.menu = menu;
    }

    /**
     * Gets the value of currentAuth.
     *
     * @return the value of currentAuth
     */
    public List<AuthPerm> getCurrentAuth() {
        return currentAuth;
    }

    /**
     * Sets the currentAuth.
     *
     * @param currentAuth currentAuth
     */
    public void setCurrentAuth(final List<AuthPerm> currentAuth) {
        this.currentAuth = currentAuth;
    }

    /**
     * Gets the value of allAuth.
     *
     * @return the value of allAuth
     */
    public List<AuthPerm> getAllAuth() {
        return allAuth;
    }

    /**
     * Sets the allAuth.
     *
     * @param allAuth allAuth
     */
    public void setAllAuth(final List<AuthPerm> allAuth) {
        this.allAuth = allAuth;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof PermissionMenuVO)) {
            return false;
        }
        PermissionMenuVO that = (PermissionMenuVO) o;
        return Objects.equals(menu, that.menu) && Objects.equals(currentAuth, that.currentAuth) && Objects.equals(allAuth, that.allAuth);
    }

    @Override
    public int hashCode() {
        return Objects.hash(menu, currentAuth, allAuth);
    }

    @Override
    public String toString() {
        return "PermissionMenuVO{"
                + "menu=" + menu
                + ", currentAuth=" + currentAuth
                + ", allAuth=" + allAuth
                + '}';
    }

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

        public MenuInfo() {
        }

        public MenuInfo(final String id, final String name, final String url, final String component, final Meta meta, final List<MenuInfo> children, final Integer sort) {
            this.id = id;
            this.name = name;
            this.url = url;
            this.component = component;
            this.meta = meta;
            this.children = children;
            this.sort = sort;
        }

        /**
         * Gets the value of id.
         *
         * @return the value of id
         */
        public String getId() {
            return id;
        }

        /**
         * Sets the id.
         *
         * @param id id
         */
        public void setId(final String id) {
            this.id = id;
        }

        /**
         * Gets the value of name.
         *
         * @return the value of name
         */
        public String getName() {
            return name;
        }

        /**
         * Sets the name.
         *
         * @param name name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * Gets the value of url.
         *
         * @return the value of url
         */
        public String getUrl() {
            return url;
        }

        /**
         * Sets the url.
         *
         * @param url url
         */
        public void setUrl(final String url) {
            this.url = url;
        }

        /**
         * Gets the value of component.
         *
         * @return the value of component
         */
        public String getComponent() {
            return component;
        }

        /**
         * Sets the component.
         *
         * @param component component
         */
        public void setComponent(final String component) {
            this.component = component;
        }

        /**
         * Gets the value of meta.
         *
         * @return the value of meta
         */
        public Meta getMeta() {
            return meta;
        }

        /**
         * Sets the meta.
         *
         * @param meta meta
         */
        public void setMeta(final Meta meta) {
            this.meta = meta;
        }

        /**
         * Gets the value of children.
         *
         * @return the value of children
         */
        public List<MenuInfo> getChildren() {
            return children;
        }

        /**
         * Sets the children.
         *
         * @param children children
         */
        public void setChildren(final List<MenuInfo> children) {
            this.children = children;
        }

        /**
         * Gets the value of sort.
         *
         * @return the value of sort
         */
        public Integer getSort() {
            return sort;
        }

        /**
         * Sets the sort.
         *
         * @param sort sort
         */
        public void setSort(final Integer sort) {
            this.sort = sort;
        }

        /**
         * builder method.
         *
         * @return builder object.
         */
        public static MenuInfo.MenuInfoBuilder builder() {
            return new MenuInfo.MenuInfoBuilder();
        }

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

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof MenuInfo)) {
                return false;
            }
            MenuInfo menuInfo = (MenuInfo) o;
            return Objects.equals(id, menuInfo.id)
                    && Objects.equals(name, menuInfo.name)
                    && Objects.equals(url, menuInfo.url)
                    && Objects.equals(component, menuInfo.component)
                    && Objects.equals(meta, menuInfo.meta)
                    && Objects.equals(children, menuInfo.children)
                    && Objects.equals(sort, menuInfo.sort);
        }

        @Override
        public int hashCode() {
            return Objects.hash(id, name, url, component, meta, children, sort);
        }

        @Override
        public String toString() {
            return "MenuInfo{"
                    + "id='" + id + '\''
                    + ", name='" + name + '\''
                    + ", url='" + url + '\''
                    + ", component='" + component + '\''
                    + ", meta=" + meta
                    + ", children=" + children
                    + ", sort=" + sort
                    + '}';
        }

        public static final class MenuInfoBuilder {

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

            private MenuInfoBuilder() {
            }

            /**
             * id.
             *
             * @param id the id.
             * @return MenuInfoBuilder.
             */
            public MenuInfoBuilder id(final String id) {
                this.id = id;
                return this;
            }

            /**
             * name.
             *
             * @param name the name.
             * @return MenuInfoBuilder.
             */
            public MenuInfoBuilder name(final String name) {
                this.name = name;
                return this;
            }

            /**
             * url.
             *
             * @param url the url.
             * @return MenuInfoBuilder.
             */
            public MenuInfoBuilder url(final String url) {
                this.url = url;
                return this;
            }

            /**
             * component.
             *
             * @param component the component.
             * @return MenuInfoBuilder.
             */
            public MenuInfoBuilder component(final String component) {
                this.component = component;
                return this;
            }

            /**
             * meta.
             *
             * @param meta the meta.
             * @return MenuInfoBuilder.
             */
            public MenuInfoBuilder meta(final Meta meta) {
                this.meta = meta;
                return this;
            }

            /**
             * children.
             *
             * @param children the children.
             * @return MenuInfoBuilder.
             */
            public MenuInfoBuilder children(final List<MenuInfo> children) {
                this.children = children;
                return this;
            }

            /**
             * sort.
             *
             * @param sort the sort.
             * @return MenuInfoBuilder.
             */
            public MenuInfoBuilder sort(final Integer sort) {
                this.sort = sort;
                return this;
            }

            /**
             * build method.
             *
             * @return build object.
             */
            public MenuInfo build() {
                MenuInfo menuInfo = new MenuInfo(id, name, url, component, meta, children, sort);
                return menuInfo;
            }
        }
    }

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

        public AuthPerm() {
        }

        public AuthPerm(final String perms, final String description, final String icon) {
            this.perms = perms;
            this.description = description;
            this.icon = icon;
        }

        /**
         * Gets the value of perms.
         *
         * @return the value of perms
         */
        public String getPerms() {
            return perms;
        }

        /**
         * Sets the perms.
         *
         * @param perms perms
         */
        public void setPerms(final String perms) {
            this.perms = perms;
        }

        /**
         * Gets the value of description.
         *
         * @return the value of description
         */
        public String getDescription() {
            return description;
        }

        /**
         * Sets the description.
         *
         * @param description description
         */
        public void setDescription(final String description) {
            this.description = description;
        }

        /**
         * Gets the value of icon.
         *
         * @return the value of icon
         */
        public String getIcon() {
            return icon;
        }

        /**
         * Sets the icon.
         *
         * @param icon icon
         */
        public void setIcon(final String icon) {
            this.icon = icon;
        }

        /**
         * build AuthPerm.
         *
         * @param resourceVO {@linkplain ResourceVO}
         * @return {@linkplain AuthPerm}
         */
        public static AuthPerm buildAuthPerm(final ResourceVO resourceVO) {
            return Optional.ofNullable(resourceVO).map(item -> new AuthPerm(resourceVO.getPerms(), resourceVO.getTitle(), resourceVO.getIcon())).orElse(null);
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof AuthPerm)) {
                return false;
            }
            AuthPerm authPerm = (AuthPerm) o;
            return Objects.equals(perms, authPerm.perms) && Objects.equals(description, authPerm.description) && Objects.equals(icon, authPerm.icon);
        }

        @Override
        public int hashCode() {
            return Objects.hash(perms, description, icon);
        }

        @Override
        public String toString() {
            return "AuthPerm{"
                    + "perms='" + perms + '\''
                    + ", description='" + description + '\''
                    + ", icon='" + icon + '\''
                    + '}';
        }
    }

    public static class Meta {

        /**
         * resource icon.
         */
        private String icon;

        /**
         * resource title.
         */
        private String title;

        public Meta() {
        }

        public Meta(final String icon, final String title) {
            this.icon = icon;
            this.title = title;
        }

        /**
         * Gets the value of icon.
         *
         * @return the value of icon
         */
        public String getIcon() {
            return icon;
        }

        /**
         * Sets the icon.
         *
         * @param icon icon
         */
        public void setIcon(final String icon) {
            this.icon = icon;
        }

        /**
         * Gets the value of title.
         *
         * @return the value of title
         */
        public String getTitle() {
            return title;
        }

        /**
         * Sets the title.
         *
         * @param title title
         */
        public void setTitle(final String title) {
            this.title = title;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof Meta)) {
                return false;
            }
            Meta meta = (Meta) o;
            return Objects.equals(icon, meta.icon) && Objects.equals(title, meta.title);
        }

        @Override
        public int hashCode() {
            return Objects.hash(icon, title);
        }

        @Override
        public String toString() {
            return "Meta{"
                    + "icon='" + icon + '\''
                    + ", title='" + title + '\''
                    + '}';
        }
    }
}
