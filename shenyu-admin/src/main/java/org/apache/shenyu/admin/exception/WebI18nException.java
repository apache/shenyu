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

package org.apache.shenyu.admin.exception;

import org.springframework.context.i18n.LocaleContextHolder;

/**
 * WebI18nException.
 * <p>Used for internationalized message exceptions in the web environment.</p>
 * <p>Exceptions that support internationalized messages</p>
 */
public class WebI18nException extends I18nException {
    
    public WebI18nException(final Throwable e) {
        super(e);
    }
    
    public WebI18nException(final String message) {
        super(LocaleContextHolder.getLocale(), message);
    }
    
    public WebI18nException(final String message, final Object... objects) {
        super(LocaleContextHolder.getLocale(), message, objects);
    }
    
    public WebI18nException(final String message, final Throwable throwable) {
        super(LocaleContextHolder.getLocale(), message, throwable);
    }
    
}
