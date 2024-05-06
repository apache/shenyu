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

package org.apache.shenyu.admin.lock.util;

import org.springframework.core.NamedThreadLocal;
import org.springframework.transaction.TransactionStatus;

/**
 * The type Register transaction util.
 * @deprecated this class is deprecated and will be removed in the next major version.
 *
 * @since 2.6.1
 */
@Deprecated
public final class RegisterTransactionUtil {
    private static final ThreadLocal<TransactionStatus> TRANSACTION_INFO_HOLDER = new NamedThreadLocal<>("Current Shenyu Register transaction");

    /**
     * set the transaction status.
     * @param status the transaction status
     */
    public static void set(final TransactionStatus status) {
        TRANSACTION_INFO_HOLDER.set(status);
    }

    /**
     * get the transaction status.
     * @return transaction status.
     */
    public static TransactionStatus get() {
        return TRANSACTION_INFO_HOLDER.get();
    }

    /**
     *  remove transaction status.
     */
    public static void remove() {
        TRANSACTION_INFO_HOLDER.remove();
    }

}
