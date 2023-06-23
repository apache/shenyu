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

import java.util.Locale;

/**
 * ValidFailException.
 * <p>valid fail.</p>
 * <p>Exceptions that support internationalized messages</p>
 */
public class I18nException extends ShenyuAdminException {
    
    private final Locale locale;
    
    private final Object[] args;
    
    public I18nException(final Throwable e) {
        super(e);
        locale = Locale.getDefault();
        args = null;
    }
    
    public I18nException(final Locale locale, final String message, final Object... objects) {
        super(message);
        this.locale = locale;
        args = objects;
    }
    
    public I18nException(final Locale locale, final String message, final Throwable throwable, final Object... objects) {
        super(message, throwable);
        this.locale = locale;
        args = objects;
    }
    
    /**
     * get locale.
     *
     * @return locale
     */
    public Locale getLocale() {
        return locale;
    }
    
    /**
     * get args.
     *
     * @return args
     */
    public Object[] getArgs() {
        return args;
    }
}
