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

package org.apache.shenyu.plugin.cryptor.common.chain;

import org.apache.shenyu.plugin.cryptor.common.strategies.CryptorStrategy;

/**
 * 加解密过滤链.
 * @author sinsy.
 */
public interface CryptorChain {

    /**
     * 解密.
     * @param cryptorStrategy 解密策略
     * @param key 秘钥
     * @param encryptData 加密数据
     * @return 解密字符串.
     */
    String decryptExecute(CryptorStrategy cryptorStrategy, String key, String encryptData);

    /**
     * 加密.
     * @param cryptorStrategy 加密策略
     * @param key 秘钥
     * @param data 数据
     * @return 加密字符串.
     */
    String encryptExecute(CryptorStrategy cryptorStrategy, String key, String data);

}
