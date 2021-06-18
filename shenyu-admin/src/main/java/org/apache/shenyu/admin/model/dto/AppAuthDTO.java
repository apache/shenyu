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

import java.io.Serializable;
import java.util.List;

/**
 * this is application authority from by web front.
 */
@Data
public class AppAuthDTO implements Serializable {

    private static final long serialVersionUID = 3906547569699874743L;

    /**
     * primary key.
     */
    private String id;

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    private String userId;

    private String phone;

    private String extInfo;

    /**
     * whether open authPath.
     */
    private Boolean open;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    private List<AuthParamDTO> authParamDTOList;

    private List<AuthPathDTO> authPathDTOList;
}
