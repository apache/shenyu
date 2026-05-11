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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.List;
import java.util.Objects;

/**
 * Gray release configuration, transferred via LoadBalanceData.attributes.
 */
public class GrayConfig {

    /**
     * gray conditions (OR relationship).
     */
    private List<GrayCondition> conditions;

    /**
     * traffic percentage routed to gray (1-100). Must be greater than 0.
     */
    private int percent;

    /**
     * delegate load balance algorithm name.
     */
    private String loadBalance;

    /**
     * metadata match for upstream filtering.
     */
    private MetadataMatch metadataMatch;

    /**
     * Instantiates a new Gray config.
     */
    public GrayConfig() {
    }

    /**
     * Instantiates a new Gray config.
     *
     * @param conditions  the conditions
     * @param percent     the percent
     * @param loadBalance the load balance
     */
    public GrayConfig(final List<GrayCondition> conditions, final int percent, final String loadBalance) {
        this.conditions = conditions;
        this.percent = percent;
        this.loadBalance = loadBalance;
    }

    /**
     * Instantiates a new Gray config with metadata match.
     *
     * @param conditions    the conditions
     * @param percent       the percent
     * @param loadBalance   the load balance
     * @param metadataMatch the metadata match
     */
    public GrayConfig(final List<GrayCondition> conditions, final int percent,
                      final String loadBalance, final MetadataMatch metadataMatch) {
        this(conditions, percent, loadBalance);
        this.metadataMatch = metadataMatch;
    }

    /**
     * Gets conditions.
     *
     * @return the conditions
     */
    public List<GrayCondition> getConditions() {
        return conditions;
    }

    /**
     * Sets conditions.
     *
     * @param conditions the conditions
     */
    public void setConditions(final List<GrayCondition> conditions) {
        this.conditions = conditions;
    }

    /**
     * Gets percent.
     *
     * @return the percent
     */
    public int getPercent() {
        return percent;
    }

    /**
     * Sets percent.
     *
     * @param percent the percent
     */
    public void setPercent(final int percent) {
        this.percent = percent;
    }

    /**
     * Gets load balance.
     *
     * @return the load balance
     */
    public String getLoadBalance() {
        return loadBalance;
    }

    /**
     * Sets load balance.
     *
     * @param loadBalance the load balance
     */
    public void setLoadBalance(final String loadBalance) {
        this.loadBalance = loadBalance;
    }

    /**
     * Gets metadata match.
     *
     * @return the metadata match
     */
    public MetadataMatch getMetadataMatch() {
        return metadataMatch;
    }

    /**
     * Sets metadata match.
     *
     * @param metadataMatch the metadata match
     */
    public void setMetadataMatch(final MetadataMatch metadataMatch) {
        this.metadataMatch = metadataMatch;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GrayConfig)) {
            return false;
        }
        GrayConfig that = (GrayConfig) o;
        return percent == that.percent
                && Objects.equals(conditions, that.conditions)
                && Objects.equals(loadBalance, that.loadBalance)
                && Objects.equals(metadataMatch, that.metadataMatch);
    }

    @Override
    public int hashCode() {
        return Objects.hash(conditions, percent, loadBalance, metadataMatch);
    }

    @Override
    public String toString() {
        return "GrayConfig{"
                + "conditions=" + conditions
                + ", percent=" + percent
                + ", loadBalance='" + loadBalance + '\''
                + ", metadataMatch=" + metadataMatch
                + '}';
    }
}
