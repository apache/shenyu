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

package org.apache.shenyu.e2e.model;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;

/**
 * ShenYu result.
 */
public class ShenYuResult {
    
    private static final ObjectMapper MAPPER = new ObjectMapper();

    private int code;

    private String message;
    
    private JsonNode data;
    
    /**
     * transform to data object.
     * @param type type
     * @param <DATA> target type
     * @return Data
     * @throws IOException IOException
     */
    public <DATA> DATA toObject(final Class<DATA> type) throws IOException {
        return MAPPER.readValue(data.traverse(), type);
    }
    
    /**
     * get code.
     *
     * @return code
     */
    public int getCode() {
        return code;
    }

    /**
     * set code.
     *
     * @param code code
     */
    public void setCode(final int code) {
        this.code = code;
    }

    /**
     * get message.
     *
     * @return message
     */
    public String getMessage() {
        return message;
    }

    /**
     * set message.
     *
     * @param message message
     */
    public void setMessage(final String message) {
        this.message = message;
    }

    /**
     * get data.
     *
     * @return data
     */
    public JsonNode getData() {
        return data;
    }

    /**
     * set data.
     *
     * @param data data
     */
    public void setData(final JsonNode data) {
        this.data = data;
    }
}
