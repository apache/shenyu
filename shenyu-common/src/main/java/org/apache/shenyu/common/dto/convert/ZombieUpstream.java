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

package org.apache.shenyu.common.dto.convert;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * this is zombie divide upstream.
 */
public class ZombieUpstream {

    /**
     * divide upstream.
     */
    private DivideUpstream divideUpstream;

    /**
     * total check times.
     */
    private int zombieCheckTimes;

    /**
     * origin selector name.
     */
    private String selectorName;

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
        this.divideUpstream = builder.divideUpstream;
        this.zombieCheckTimes = builder.zombieCheckTimes;
        this.selectorName = builder.selectorName;
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
     * get divideUpstream.
     *
     * @return divideUpstream
     */
    public DivideUpstream getDivideUpstream() {
        return divideUpstream;
    }

    /**
     * set divideUpstream.
     *
     * @param divideUpstream divideUpstream
     */
    public void setDivideUpstream(final DivideUpstream divideUpstream) {
        this.divideUpstream = divideUpstream;
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
     * get selectorName.
     *
     * @return selectorName
     */
    public String getSelectorName() {
        return selectorName;
    }

    /**
     * set selectorName.
     *
     * @param selectorName selectorName
     */
    public void setSelectorName(final String selectorName) {
        this.selectorName = selectorName;
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
                .append(zombieCheckTimes, that.zombieCheckTimes)
                .append(divideUpstream, that.divideUpstream)
                .append(selectorName, that.selectorName)
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .append(divideUpstream)
                .append(zombieCheckTimes)
                .append(selectorName)
                .toHashCode();
    }

    @Override
    public String toString() {
        return "ZombieUpstream{"
                + "divideUpstream="
                + divideUpstream
                + ", zombieCheckTimes="
                + zombieCheckTimes
                + ", selectorName='"
                + selectorName
                + '\''
                + '}';
    }

    /**
     * create zombie upstream with divide upstream.
     *
     * @param divideUpstream   {@linkplain DivideUpstream} origin divide upstream.
     * @param zombieCheckTimes total check times.
     * @param selectorName     origin selector name.
     * @return new zombie upstream.
     */
    public static ZombieUpstream transform(final DivideUpstream divideUpstream, final int zombieCheckTimes, final String selectorName) {
        return ZombieUpstream.builder().divideUpstream(divideUpstream).zombieCheckTimes(zombieCheckTimes).selectorName(selectorName).build();
    }

    /**
     * class builder.
     */
    public static final class Builder {

        /**
         * divideUpstream.
         */
        private DivideUpstream divideUpstream;

        /**
         * zombieCheckTimes.
         */
        private int zombieCheckTimes;

        /**
         * selectorName.
         */
        private String selectorName;

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
         * build divideUpstream.
         *
         * @param divideUpstream divideUpstream
         * @return this
         */
        public Builder divideUpstream(final DivideUpstream divideUpstream) {
            this.divideUpstream = divideUpstream;
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
         * build selectorName.
         *
         * @param selectorName selectorName
         * @return this
         */
        public Builder selectorName(final String selectorName) {
            this.selectorName = selectorName;
            return this;
        }
    }
}
