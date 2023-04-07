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

package org.apache.shenyu.common.dto.convert.selector;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * this is zombie divide upstream.
 */
public class ZombieUpstream {

    /**
     * common upstream.
     */
    private CommonUpstream commonUpstream;

    /**
     * total check times.
     */
    private int zombieCheckTimes;

    /**
     * origin selector name.
     */
    private String selectorId;

    /**
     * no args constructor.
     */
    public ZombieUpstream() {
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private ZombieUpstream(final Builder builder) {
        this.commonUpstream = builder.commonUpstream;
        this.zombieCheckTimes = builder.zombieCheckTimes;
        this.selectorId = builder.selectorId;
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * get commonUpstream.
     *
     * @return commonUpstream
     */
    public CommonUpstream getCommonUpstream() {
        return commonUpstream;
    }

    /**
     * set commonUpstream.
     *
     * @param commonUpstream commonUpstream
     */
    public void setCommonUpstream(final CommonUpstream commonUpstream) {
        this.commonUpstream = commonUpstream;
    }

    /**
     * get zombieCheckTimes.
     *
     * @return zombieCheckTimes
     */
    public int getZombieCheckTimes() {
        return zombieCheckTimes;
    }

    /**
     * set zombieCheckTimes.
     *
     * @param zombieCheckTimes zombieCheckTimes
     */
    public void setZombieCheckTimes(final int zombieCheckTimes) {
        this.zombieCheckTimes = zombieCheckTimes;
    }

    /**
     * get selectorId.
     *
     * @return selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * set selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorName(final String selectorId) {
        this.selectorId = selectorId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ZombieUpstream that = (ZombieUpstream) o;
        return new EqualsBuilder()
                .append(commonUpstream, that.commonUpstream)
                .append(selectorId, that.selectorId)
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .append(commonUpstream)
                .append(selectorId)
                .toHashCode();
    }

    @Override
    public String toString() {
        return "ZombieUpstream{"
                + "commonUpstream="
                + commonUpstream
                + ", zombieCheckTimes="
                + zombieCheckTimes
                + ", selectorId='"
                + selectorId
                + '\''
                + '}';
    }

    /**
     * create zombie upstream with Common upstream.
     *
     * @param commonUpstream   {@linkplain CommonUpstream} origin divide upstream.
     * @param zombieCheckTimes total check times.
     * @param selectorId     origin selector id.
     * @return new zombie upstream.
     */
    public static ZombieUpstream transform(final CommonUpstream commonUpstream, final int zombieCheckTimes, final String selectorId) {
        return ZombieUpstream.builder().commonUpstream(commonUpstream).zombieCheckTimes(zombieCheckTimes).selectorId(selectorId).build();
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * commonUpstream.
         */
        private CommonUpstream commonUpstream;

        /**
         * zombieCheckTimes.
         */
        private int zombieCheckTimes;

        /**
         * selectorId.
         */
        private String selectorId;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return ZombieUpstream
         */
        public ZombieUpstream build() {
            return new ZombieUpstream(this);
        }

        /**
         * build commonUpstream.
         *
         * @param commonUpstream commonUpstream
         * @return this
         */
        public Builder commonUpstream(final CommonUpstream commonUpstream) {
            this.commonUpstream = commonUpstream;
            return this;
        }

        /**
         * build zombieCheckTimes.
         *
         * @param zombieCheckTimes zombieCheckTimes
         * @return this
         */
        public Builder zombieCheckTimes(final int zombieCheckTimes) {
            this.zombieCheckTimes = zombieCheckTimes;
            return this;
        }

        /**
         * build selectorId.
         *
         * @param selectorId selectorId
         * @return this
         */
        public Builder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }
    }
}
