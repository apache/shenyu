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

package org.dromara.soul.common.enums;

/**
 * This interface should only be implements by enum.
 * <p>
 * example:
 * </p>
 * <pre>
 * import org.dromara.soul.common.enums.BaseEnum;
 *
 * public enum RateLimitEnum implements BaseEnum&#60;String, String&#62; {
 *      SLIDING_WINDOW("request_rate_limiter.lua", "request_rate_limiter");
 *
 *      private final String scriptName;
 *
 *      private final String keyName;
 *
 *      RateLimitEnum(final String scriptName, final String keyName) {
 *          this.keyName = keyName;
 *          this.scriptName = scriptName;
 *      }
 *
 *      public String getOne() {
 *          return scriptName;
 *      }
 *
 *      public String getTwo() {
 *          return keyName;
 *      }
 * }
 * </pre>
 * An enum needs to be represented by both K and V, if you only use One of them, maybe you can define them as the same type.
 * @param <V> T should be defined to describe the intent of this enumeration, like description, introduce.
 * @param <K> You should define C to represent this enumeration，like code, key, name.
 *
 * @author zhoutzzz
 * @since 2.3.1-SNAPSHOT
 */
public interface BaseEnum<K, V> {

    /**
     * get the K.
     *
     * @return K
     */
    K getKey();

    /**
     * get the V.
     *
     * @return V
     */
    V getValue();
}
