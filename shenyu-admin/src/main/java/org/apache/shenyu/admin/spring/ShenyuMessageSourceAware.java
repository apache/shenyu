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

package org.apache.shenyu.admin.spring;

import org.springframework.context.MessageSource;
import org.springframework.context.MessageSourceAware;

/**
 * Get messageSource Bean.
 */
public class ShenyuMessageSourceAware implements MessageSourceAware {

    private static MessageSource messageSource;

    /**
     * set messageSource.
     *
     * @param messageSource messageSource
     */
    @Override
    public void setMessageSource(final MessageSource messageSource) {
        ShenyuMessageSourceAware.messageSource = messageSource;
    }

    /**
     * get messageSource.
     *
     * @return messageSource
     */
    public static MessageSource getMessageSource() {
        return ShenyuMessageSourceAware.messageSource;
    }
}
