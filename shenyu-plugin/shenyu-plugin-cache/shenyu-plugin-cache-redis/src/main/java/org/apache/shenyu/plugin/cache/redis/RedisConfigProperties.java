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

package org.apache.shenyu.plugin.cache.redis;

import org.apache.shenyu.common.enums.RedisModeEnum;

import java.time.Duration;
import java.util.Objects;
import java.util.Optional;

/**
 * The configuration for redis.
 */
public class RedisConfigProperties {

    private Integer database = 0;

    private String master;

    private String mode = RedisModeEnum.STANDALONE.getName();

    /**
     * If it is cluster or sentinel mode, separated with `;`.
     */
    private String url;

    /**
     * the password.
     */
    private String password;

    /**
     * Maximum number of "idle" connections in the pool. Use a negative value to
     * indicate an unlimited number of idle connections.
     */
    private Integer maxIdle = 8;

    /**
     * Target for the minimum number of idle connections to maintain in the pool. This
     * setting only has an effect if it is positive.
     */
    private Integer minIdle = 0;

    /**
     * Maximum number of connections that can be allocated by the pool at a given
     * time. Use a negative value for no limit.
     */
    private Integer maxActive = 8;

    /**
     * Maximum amount of time a connection allocation should block before throwing an
     * exception when the pool is exhausted. Use a negative value to block
     * indefinitely.
     */
    private Integer maxWait = -1;

    /**
     * Gets database.
     *
     * @return the database
     */
    public Integer getDatabase() {
        return database;
    }

    /**
     * Sets database.
     *
     * @param database the database
     */
    public void setDatabase(final Integer database) {
        this.database = database;
    }

    /**
     * Gets master.
     *
     * @return the master
     */
    public String getMaster() {
        return master;
    }

    /**
     * Sets master.
     *
     * @param master the master
     */
    public void setMaster(final String master) {
        this.master = master;
    }

    /**
     * Gets mode.
     *
     * @return the mode
     */
    public String getMode() {
        return mode;
    }

    /**
     * Sets mode.
     *
     * @param mode the mode
     */
    public void setMode(final String mode) {
        this.mode = mode;
    }

    /**
     * Gets url.
     *
     * @return the url
     */
    public String getUrl() {
        return url;
    }

    /**
     * Sets url.
     *
     * @param url the url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * Gets password.
     *
     * @return the password
     */
    public String getPassword() {
        return password;
    }

    /**
     * Sets password.
     *
     * @param password the password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * Gets max idle.
     *
     * @return the max idle
     */
    public int getMaxIdle() {
        return maxIdle;
    }

    /**
     * Sets max idle.
     *
     * @param maxIdle the max idle
     */
    public void setMaxIdle(final int maxIdle) {
        this.maxIdle = maxIdle;
    }

    /**
     * Gets min idle.
     *
     * @return the min idle
     */
    public Integer getMinIdle() {
        return minIdle;
    }

    /**
     * Sets min idle.
     *
     * @param minIdle the min idle
     */
    public void setMinIdle(final int minIdle) {
        this.minIdle = minIdle;
    }

    /**
     * Gets max active.
     *
     * @return the max active
     */
    public Integer getMaxActive() {
        return maxActive;
    }

    /**
     * Sets max active.
     *
     * @param maxActive the max active
     */
    public void setMaxActive(final int maxActive) {
        this.maxActive = maxActive;
    }

    /**
     * Gets max wait.
     *
     * @return the max wait
     */
    public Duration getMaxWait() {
        return Optional.ofNullable(maxWait)
                .map(it -> Duration.ofMillis(maxWait)).orElse(Duration.ofMillis(-1));
    }

    /**
     * Sets max wait.
     *
     * @param maxWait the max wait
     */
    public void setMaxWait(final Integer maxWait) {
        this.maxWait = maxWait;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final RedisConfigProperties that = (RedisConfigProperties) o;
        return Objects.equals(database, that.database)
                && Objects.equals(master, that.master)
                && Objects.equals(mode, that.mode)
                && Objects.equals(url, that.url)
                && Objects.equals(password, that.password)
                && Objects.equals(maxIdle, that.maxIdle)
                && Objects.equals(minIdle, that.minIdle)
                && Objects.equals(maxActive, that.maxActive)
                && Objects.equals(maxWait, that.maxWait);
    }

    @Override
    public int hashCode() {
        return Objects.hash(database, master, mode, url, password, maxIdle, minIdle, maxActive, maxWait);
    }
}
