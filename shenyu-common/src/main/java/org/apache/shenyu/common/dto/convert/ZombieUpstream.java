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

import java.util.Objects;

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
     * all args constructor.
     *
     * @param divideUpstream   divideUpstream
     * @param zombieCheckTimes zombieCheckTimes
     * @param selectorName     selectorName
     */
    public ZombieUpstream(final DivideUpstream divideUpstream, final int zombieCheckTimes, final String selectorName) {
        this.divideUpstream = divideUpstream;
        this.zombieCheckTimes = zombieCheckTimes;
        this.selectorName = selectorName;
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
        return zombieCheckTimes == that.zombieCheckTimes
                && Objects.equals(divideUpstream, that.divideUpstream)
                && Objects.equals(selectorName, that.selectorName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(divideUpstream, zombieCheckTimes, selectorName);
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
        return new ZombieUpstream(divideUpstream, zombieCheckTimes, selectorName);
    }
}
