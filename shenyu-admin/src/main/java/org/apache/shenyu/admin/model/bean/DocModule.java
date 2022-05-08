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

package org.apache.shenyu.admin.model.bean;

import java.util.List;

/**
 * DocModule.
 *
 */
public class DocModule {

    private String module;

    private int order;

    private List<DocItem> docItems;

    /**
     * getModule.
     *
     * @return String
     */
    public String getModule() {
        return module;
    }

    /**
     * setModule.
     *
     * @param module module
     */
    public void setModule(final String module) {
        this.module = module;
    }

    /**
     * getDocItems.
     *
     * @return List
     */
    public List<DocItem> getDocItems() {
        return docItems;
    }

    /**
     * setDocItems.
     *
     * @param docItems docItems
     */
    public void setDocItems(final List<DocItem> docItems) {
        this.docItems = docItems;
    }

    /**
     * getOrder.
     *
     * @return int
     */
    public int getOrder() {
        return order;
    }

    /**
     * setOrder.
     *
     * @param order order
     */
    public void setOrder(final int order) {
        this.order = order;
    }
}
