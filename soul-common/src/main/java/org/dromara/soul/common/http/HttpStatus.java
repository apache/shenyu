/*
 *
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.common.http;

import lombok.Data;

/**
 * HttpStatus.
 *
 * @author sixh
 */
@Data
public class HttpStatus {
    private Integer code;
    private String codeAsText;

    public HttpStatus(Integer code, String codeAsText) {
        this.code = code;
        this.codeAsText = codeAsText;
    }

    /**
     * time out.
     */
    public static final HttpStatus TIME_OUT = new HttpStatus(504, "");
    public static final HttpStatus OK = new HttpStatus(200, "ok");
    /**
     * 400 Bad Request
     */
    public static final HttpStatus BAD_REQUEST = new HttpStatus(400, "Bad Request");
    public static final HttpStatus NOT_FOUND = new HttpStatus(404, "Not Found");
}
