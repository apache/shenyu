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

package org.apache.shenyu.common.dto.convert.rule.impl;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.RetryEnum;

import java.util.Objects;

/**
 * The type Divide rule handle.
 */
public class DivideRuleHandle implements RuleHandle {

    /**
     * loadBalance.
     * {@linkplain LoadBalanceEnum}
     */
    private String loadBalance = LoadBalanceEnum.RANDOM.getName();

    /**
     * retryStrategy.
     * {@linkplain RetryEnum}
     */
    private String retryStrategy = RetryEnum.CURRENT.getName();

    /**
     * http retry.
     */
    private int retry = 3;

    /**
     * timeout is required.
     */
    private long timeout = Constants.TIME_OUT;

    /**
     * headerMaxSize.
     */
    private long headerMaxSize;

    /**
     * requestMaxSize.
     */
    private long requestMaxSize;

    /**
     * whether gray routing is active.
     */
    private boolean grayEnabled;

    /**
     * traffic percentage routed to gray (1-100). Must be greater than 0.
     */
    private int grayPercent;

    /**
     * gray condition param type (header, cookie, query, ip).
     */
    private String grayConditionParamType;

    /**
     * gray condition param name.
     */
    private String grayConditionParamName;

    /**
     * gray condition operator (=, regex, contains).
     */
    private String grayConditionOperator;

    /**
     * gray condition param value.
     */
    private String grayConditionParamValue;

    /**
     * gray metadata key for upstream partitioning.
     */
    private String grayMetadataKey;

    /**
     * gray metadata value for upstream partitioning.
     */
    private String grayMetadataValue;

    /**
     * New instance divide rule handle.
     *
     * @return the divide rule handle
     */
    public static DivideRuleHandle newInstance() {
        return new DivideRuleHandle();
    }
    
    /**
     * get loadBalance.
     *
     * @return loadBalance load balance
     */
    public String getLoadBalance() {
        return loadBalance;
    }
    
    /**
     * set loadBalance.
     *
     * @param loadBalance loadBalance
     */
    public void setLoadBalance(final String loadBalance) {
        this.loadBalance = loadBalance;
    }
    
    /**
     * get retryStrategy.
     *
     * @return retryStrategy retry strategy
     */
    public String getRetryStrategy() {
        return retryStrategy;
    }
    
    /**
     * set retryStrategy.
     *
     * @param retryStrategy retryStrategy
     */
    public void setRetryStrategy(final String retryStrategy) {
        this.retryStrategy = retryStrategy;
    }
    
    /**
     * get retry.
     *
     * @return retry retry
     */
    public int getRetry() {
        return retry;
    }
    
    /**
     * set retry.
     *
     * @param retry retry
     */
    public void setRetry(final int retry) {
        this.retry = retry;
    }
    
    /**
     * get timeout.
     *
     * @return timeout timeout
     */
    public long getTimeout() {
        return timeout;
    }
    
    /**
     * set timeout.
     *
     * @param timeout timeout
     */
    public void setTimeout(final long timeout) {
        this.timeout = timeout;
    }
    
    /**
     * get headerMaxSize.
     *
     * @return headerMaxSize header max size
     */
    public long getHeaderMaxSize() {
        return headerMaxSize;
    }
    
    /**
     * set headerMaxSize.
     *
     * @param headerMaxSize headerMaxSize
     */
    public void setHeaderMaxSize(final long headerMaxSize) {
        this.headerMaxSize = headerMaxSize;
    }
    
    /**
     * get requestMaxSize.
     *
     * @return requestMaxSize request max size
     */
    public long getRequestMaxSize() {
        return requestMaxSize;
    }
    
    /**
     * set requestMaxSize.
     *
     * @param requestMaxSize requestMaxSize
     */
    public void setRequestMaxSize(final long requestMaxSize) {
        this.requestMaxSize = requestMaxSize;
    }

    /**
     * is grayEnabled.
     *
     * @return grayEnabled
     */
    public boolean isGrayEnabled() {
        return grayEnabled;
    }

    /**
     * set grayEnabled.
     *
     * @param grayEnabled grayEnabled
     */
    public void setGrayEnabled(final boolean grayEnabled) {
        this.grayEnabled = grayEnabled;
    }

    /**
     * get grayPercent.
     *
     * @return grayPercent
     */
    public int getGrayPercent() {
        return grayPercent;
    }

    /**
     * set grayPercent.
     *
     * @param grayPercent grayPercent
     */
    public void setGrayPercent(final int grayPercent) {
        this.grayPercent = grayPercent;
    }

    /**
     * get grayConditionParamType.
     *
     * @return grayConditionParamType
     */
    public String getGrayConditionParamType() {
        return grayConditionParamType;
    }

    /**
     * set grayConditionParamType.
     *
     * @param grayConditionParamType grayConditionParamType
     */
    public void setGrayConditionParamType(final String grayConditionParamType) {
        this.grayConditionParamType = grayConditionParamType;
    }

    /**
     * get grayConditionParamName.
     *
     * @return grayConditionParamName
     */
    public String getGrayConditionParamName() {
        return grayConditionParamName;
    }

    /**
     * set grayConditionParamName.
     *
     * @param grayConditionParamName grayConditionParamName
     */
    public void setGrayConditionParamName(final String grayConditionParamName) {
        this.grayConditionParamName = grayConditionParamName;
    }

    /**
     * get grayConditionOperator.
     *
     * @return grayConditionOperator
     */
    public String getGrayConditionOperator() {
        return grayConditionOperator;
    }

    /**
     * set grayConditionOperator.
     *
     * @param grayConditionOperator grayConditionOperator
     */
    public void setGrayConditionOperator(final String grayConditionOperator) {
        this.grayConditionOperator = grayConditionOperator;
    }

    /**
     * get grayConditionParamValue.
     *
     * @return grayConditionParamValue
     */
    public String getGrayConditionParamValue() {
        return grayConditionParamValue;
    }

    /**
     * set grayConditionParamValue.
     *
     * @param grayConditionParamValue grayConditionParamValue
     */
    public void setGrayConditionParamValue(final String grayConditionParamValue) {
        this.grayConditionParamValue = grayConditionParamValue;
    }

    /**
     * get grayMetadataKey.
     *
     * @return grayMetadataKey
     */
    public String getGrayMetadataKey() {
        return grayMetadataKey;
    }

    /**
     * set grayMetadataKey.
     *
     * @param grayMetadataKey grayMetadataKey
     */
    public void setGrayMetadataKey(final String grayMetadataKey) {
        this.grayMetadataKey = grayMetadataKey;
    }

    /**
     * get grayMetadataValue.
     *
     * @return grayMetadataValue
     */
    public String getGrayMetadataValue() {
        return grayMetadataValue;
    }

    /**
     * set grayMetadataValue.
     *
     * @param grayMetadataValue grayMetadataValue
     */
    public void setGrayMetadataValue(final String grayMetadataValue) {
        this.grayMetadataValue = grayMetadataValue;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        DivideRuleHandle that = (DivideRuleHandle) o;
        return retry == that.retry && timeout == that.timeout && headerMaxSize == that.headerMaxSize
                && requestMaxSize == that.requestMaxSize && grayEnabled == that.grayEnabled
                && grayPercent == that.grayPercent
                && Objects.equals(loadBalance, that.loadBalance)
                && Objects.equals(retryStrategy, that.retryStrategy)
                && Objects.equals(grayConditionParamType, that.grayConditionParamType)
                && Objects.equals(grayConditionParamName, that.grayConditionParamName)
                && Objects.equals(grayConditionOperator, that.grayConditionOperator)
                && Objects.equals(grayConditionParamValue, that.grayConditionParamValue)
                && Objects.equals(grayMetadataKey, that.grayMetadataKey)
                && Objects.equals(grayMetadataValue, that.grayMetadataValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(loadBalance, retryStrategy, retry, timeout, headerMaxSize, requestMaxSize,
                grayEnabled, grayPercent, grayConditionParamType, grayConditionParamName,
                grayConditionOperator, grayConditionParamValue, grayMetadataKey, grayMetadataValue);
    }

    @Override
    public String toString() {
        return "DivideRuleHandle{"
                + "loadBalance='"
                + loadBalance
                + '\''
                + ", retryStrategy='"
                + retryStrategy
                + '\''
                + ", retry="
                + retry
                + ", timeout="
                + timeout
                + ", headerMaxSize="
                + headerMaxSize
                + ", requestMaxSize="
                + requestMaxSize
                + ", grayEnabled="
                + grayEnabled
                + ", grayPercent="
                + grayPercent
                + ", grayConditionParamType='"
                + grayConditionParamType
                + '\''
                + ", grayConditionParamName='"
                + grayConditionParamName
                + '\''
                + ", grayConditionOperator='"
                + grayConditionOperator
                + '\''
                + ", grayConditionParamValue='"
                + grayConditionParamValue
                + '\''
                + ", grayMetadataKey='"
                + grayMetadataKey
                + '\''
                + ", grayMetadataValue='"
                + grayMetadataValue
                + '\''
                + '}';
    }
}
