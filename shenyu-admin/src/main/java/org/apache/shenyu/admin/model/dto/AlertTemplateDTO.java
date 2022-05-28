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

package org.apache.shenyu.admin.model.dto;

import javax.validation.constraints.NotNull;

public class AlertTemplateDTO {

    /**
     * alert template id.
     */
    private Long id;

    /**
     * alert template name.
     */
    @NotNull
    private String name;

    /**
     * alert template strategyName.
     */
    @NotNull
    private String strategyName;

    /**
     * alert template content.
     */
    @NotNull
    private String content;

    /**
     * get id.
     * @return id
     */
    public Long getId() {
        return id;
    }

    /**
     * set id.
     * @param id id
     */
    public void setId(final Long id) {
        this.id = id;
    }

    /**
     * get alert template name.
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set alert template name.
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get alert template strategy name.
     * @return strategyName
     */
    public String getStrategyName() {
        return strategyName;
    }

    /**
     * set alert template strategy name.
     * @param strategyName strategyName
     */
    public void setStrategyName(final String strategyName) {
        this.strategyName = strategyName;
    }

    /**
     * get content.
     * @return content
     */
    public String getContent() {
        return content;
    }

    /**
     * set content.
     * @param content content
     */
    public void setContent(final String content) {
        this.content = content;
    }
}
