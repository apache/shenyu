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

package org.apache.shenyu.plugin.param.mapping.strategy;

import org.apache.shenyu.common.constant.Constants;
import org.springframework.http.MediaType;

import java.util.HashMap;
import java.util.Map;

/**
 * OperatorFactory.
 */
public class OperatorFactory {

    private static final Map<String, Operator> FATCORY = new HashMap<>();

    static {
        FATCORY.put(Constants.DEFAULT, new DefaultOperator());
        FATCORY.put(MediaType.APPLICATION_JSON.toString(), new ApplicationJsonOperator());
        FATCORY.put(MediaType.APPLICATION_FORM_URLENCODED.toString(), new ApplicationFormOperator());
    }

    /**
     * OperatorFactory match.
     *
     * @param mediaType mediaType
     * @return operator
     */
    public static Operator match(final MediaType mediaType) {
        if (MediaType.APPLICATION_JSON.isCompatibleWith(mediaType)) {
            return FATCORY.get(MediaType.APPLICATION_JSON.toString());
        } else if (MediaType.APPLICATION_FORM_URLENCODED.isCompatibleWith(mediaType)) {
            return FATCORY.get(MediaType.APPLICATION_FORM_URLENCODED.toString());
        } else {
            return FATCORY.get(Constants.DEFAULT);
        }
    }
}
