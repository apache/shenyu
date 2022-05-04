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

import java.util.List;

/**
 * MenuModule.
 * MenuModule
 */
public class MenuModule {

    private String label;

    private List<MenuDocItem> children;

    /**
     * getLabel.
     *
     * @return String
     */
    public String getLabel() {
        return label;
    }

    /**
     * setLabel.
     *
     * @param label label
     */
    public void setLabel(final String label) {
        this.label = label;
    }

    /**
     * getChildren.
     *
     * @return List
     */
    public List<MenuDocItem> getChildren() {
        return children;
    }

    /**
     * setChildren.
     *
     * @param children children
     */
    public void setChildren(final List<MenuDocItem> children) {
        this.children = children;
    }
}
