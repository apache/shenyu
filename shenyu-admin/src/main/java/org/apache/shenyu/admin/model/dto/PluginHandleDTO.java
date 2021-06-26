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

import lombok.Data;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * this plugin handle from web front.
 */
@Data
public class PluginHandleDTO implements Serializable {

    private static final long serialVersionUID = 8010034956423631265L;

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin id.
     */
    @NotNull
    private String pluginId;

    /**
     * the attribute name.
     */
    @NotNull
    private String field;

    /**
     * the attribute label.
     */
    private String label;

    /**
     * the data type.
     * 1 indicates number
     * 2 indicates string
     * 3 indicates select box.
     */
    @NotNull
    private Integer dataType;

    /**
     *  the attribute type.
     *  1  selector,
     *  2  rule.
     */
    private Integer type;

    /**
     * the attribute sort.
     */
    private Integer sort;

    /**
     * the attribute extObj.
     */
    private String extObj;
}
