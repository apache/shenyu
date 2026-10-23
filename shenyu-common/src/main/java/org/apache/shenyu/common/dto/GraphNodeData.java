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

package org.apache.shenyu.common.dto;

import java.util.List;
import java.util.Objects;

/**
 * A vertex of a plugin orchestration graph.
 *
 * <p>The {@code type} field is the canonical name of
 * {@linkplain org.apache.shenyu.common.enums.GraphNodeTypeEnum} ({@code PLUGIN}, {@code CHOICE} or
 * {@code END}). Field applicability per type mirrors {@code docs/plugin-graph-orchestration.md}
 * (issue #7994) section 4:</p>
 * <ul>
 *     <li>{@code PLUGIN}: binds {@code pluginName} and has exactly one successor {@code next};
 *     {@code required} marks a mandatory dependency that fails closed.</li>
 *     <li>{@code CHOICE}: ordered {@code conditions} branches (first match wins) plus a mandatory
 *     {@code defaultTarget}.</li>
 *     <li>{@code END}: terminal; no successor.</li>
 * </ul>
 *
 * @since 2.7.2
 */
public class GraphNodeData {

    /**
     * unique node id within the graph.
     */
    private String nodeId;

    /**
     * node type.
     * {@linkplain org.apache.shenyu.common.enums.GraphNodeTypeEnum}.
     */
    private String type;

    /**
     * the bound plugin name (PLUGIN nodes only).
     * {@linkplain org.apache.shenyu.common.enums.PluginEnum#getName()}.
     */
    private String pluginName;

    /**
     * the single successor node id (PLUGIN nodes only).
     */
    private String next;

    /**
     * ordered branches, first match wins (CHOICE nodes only).
     */
    private List<GraphBranchData> conditions;

    /**
     * mandatory fallback target when no branch matches (CHOICE nodes only).
     */
    private String defaultTarget;

    /**
     * whether the bound plugin is a mandatory dependency that fails closed (PLUGIN nodes only).
     */
    private boolean required;

    /**
     * no args constructor.
     */
    public GraphNodeData() {
    }

    /**
     * get nodeId.
     *
     * @return nodeId
     */
    public String getNodeId() {
        return nodeId;
    }

    /**
     * set nodeId.
     *
     * @param nodeId nodeId
     */
    public void setNodeId(final String nodeId) {
        this.nodeId = nodeId;
    }

    /**
     * get type.
     *
     * @return type
     */
    public String getType() {
        return type;
    }

    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * get pluginName.
     *
     * @return pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * set pluginName.
     *
     * @param pluginName pluginName
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * get next.
     *
     * @return next
     */
    public String getNext() {
        return next;
    }

    /**
     * set next.
     *
     * @param next next
     */
    public void setNext(final String next) {
        this.next = next;
    }

    /**
     * get conditions.
     *
     * @return conditions
     */
    public List<GraphBranchData> getConditions() {
        return conditions;
    }

    /**
     * set conditions.
     *
     * @param conditions conditions
     */
    public void setConditions(final List<GraphBranchData> conditions) {
        this.conditions = conditions;
    }

    /**
     * get defaultTarget.
     *
     * @return defaultTarget
     */
    public String getDefaultTarget() {
        return defaultTarget;
    }

    /**
     * set defaultTarget.
     *
     * @param defaultTarget defaultTarget
     */
    public void setDefaultTarget(final String defaultTarget) {
        this.defaultTarget = defaultTarget;
    }

    /**
     * is required.
     *
     * @return required
     */
    public boolean isRequired() {
        return required;
    }

    /**
     * set required.
     *
     * @param required required
     */
    public void setRequired(final boolean required) {
        this.required = required;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        GraphNodeData that = (GraphNodeData) o;
        return required == that.required && Objects.equals(nodeId, that.nodeId) && Objects.equals(type, that.type)
                && Objects.equals(pluginName, that.pluginName) && Objects.equals(next, that.next)
                && Objects.equals(conditions, that.conditions) && Objects.equals(defaultTarget, that.defaultTarget);
    }

    @Override
    public int hashCode() {
        return Objects.hash(nodeId, type, pluginName, next, conditions, defaultTarget, required);
    }

    @Override
    public String toString() {
        return "GraphNodeData{"
                + "nodeId='"
                + nodeId
                + '\''
                + ", type='"
                + type
                + '\''
                + ", pluginName='"
                + pluginName
                + '\''
                + ", next='"
                + next
                + '\''
                + ", conditions="
                + conditions
                + ", defaultTarget='"
                + defaultTarget
                + '\''
                + ", required="
                + required
                + '}';
    }
}
