/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.core.bind;

/**
 * Convert .
 * 类型转换实现.
 *
 * @author sixh
 */
public interface Converter {

    /**
     * 类型转换器.
     *
     * @param <T>   转换后的类型
     * @param clazz 需要转换后的class.
     * @param value 需要转换的值.
     * @return t
     */
    <T> T convert(Class<?> clazz, Object value);

}
