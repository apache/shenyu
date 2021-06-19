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
 * this shenyu dict from web front.
 */
@Data
public class ShenyuDictDTO implements Serializable {

    private static final long serialVersionUID = -8639439741130267709L;

    /**
     * primary key.
     */
    private String id;

    /**
     * dict type.
     */
    @NotNull
    private String type;

    /**
     * dict code.
     */
    private String dictCode;

    /**
     * dict name.
     */
    @NotNull
    private String dictName;

    /**
     * dict value.
     */
    @NotNull
    private String dictValue;

    /**
     * dict desc.
     */
    private String desc;

    /**
     * sort no.
     */
    @NotNull
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

}
