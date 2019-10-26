/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.dashboard.result;

import lombok.Getter;

/**
 * The enum Common code.
 *
 * @author xiaoyu
 */
@Getter
public enum CommonCode {

    /**
     * 所有成功操作的返回.
     */
    SUCCESS(200, "操作成功"),
    /**
     * Failure common code.
     */
    FAILURE(-1, "操作失败"),
    /**
     * Request is null common code.
     */
    REQUEST_IS_NULL(102, "请求参数为空,{0}");

    /**
     * code.
     */
    private int code;
    /**
     * msg.
     */
    private String msg;

    CommonCode(int code, String msg) {
        this.code = code;
        this.msg = msg;
    }
}
