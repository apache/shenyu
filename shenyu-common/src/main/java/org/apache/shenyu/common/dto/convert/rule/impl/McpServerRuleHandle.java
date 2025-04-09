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

package org.apache.shenyu.common.dto.convert.rule.impl;

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;

import java.util.Objects;

/**
 * The type Mcp server rule handle.
 */
public class McpServerRuleHandle implements RuleHandle {

    /**
     * description.
     */
    private String description;
    
    /**
     * get description.
     *
     * @return description
     */
    public String getDescription() {
        return description;
    }
    
    /**
     * set description.
     *
     * @param description description
     */
    public void setDescription(final String description) {
        this.description = description;
    }
    
    public static McpServerRuleHandle newInstance() {
        McpServerRuleHandle mcpServerRuleHandle = new McpServerRuleHandle();
        mcpServerRuleHandle.setDescription("");
        return mcpServerRuleHandle;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        McpServerRuleHandle that = (McpServerRuleHandle) o;
        return Objects.equals(description, that.description);
    }

    @Override
    public int hashCode() {
        return Objects.hash(description);
    }

    @Override
    public String toString() {
        return "McpServerRuleHandle{" + "description='" + description + '\'' + '}';
    }
}
