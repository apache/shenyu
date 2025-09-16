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

package org.apache.shenyu.sync.data.http.config;

import java.util.Objects;

/**
 * The type Http config.
 */
public class HttpConfig {

    private String url;

    private Integer delayTime;

    private Integer connectionTimeout;

    private Integer readTimeout;

    private Integer writeTimeout;

    private String username;

    private String password;

    private String aesSecretKey;

    private String aesSecretIv;

    /**
     * get aesSecretKey.
     * @return  aesSecretKey
     */
    public String getAesSecretKey() {
        return aesSecretKey;
    }

    /**
     * set aesSecretKey.
     * @param aesSecretKey aesSecretKey
     */
    public void setAesSecretKey(final String aesSecretKey) {
        this.aesSecretKey = aesSecretKey;
    }

    /**
     * get  aesSecretIv.
     * @return aesSecretIv
     */
    public String getAesSecretIv() {
        return aesSecretIv;
    }

    /**
     * set aesSecretIv.
     * @param aesSecretIv aesSecretIv
     */
    public void setAesSecretIv(final String aesSecretIv) {
        this.aesSecretIv = aesSecretIv;
    }

    /**
     * get username.
     *
     * @return username
     */
    public String getUsername() {
        return username;
    }

    /**
     * set username.
     *
     * @param username username
     */
    public void setUsername(final String username) {
        this.username = username;
    }

    /**
     * get password.
     *
     * @return password
     */
    public String getPassword() {
        return password;
    }

    /**
     * set password.
     *
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * get url.
     *
     * @return url
     */
    public String getUrl() {
        return url;
    }

    /**
     * set url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * get delayTime.
     *
     * @return delayTime
     */
    public Integer getDelayTime() {
        return delayTime;
    }

    /**
     * set delayTime.
     *
     * @param delayTime delayTime
     */
    public void setDelayTime(final Integer delayTime) {
        this.delayTime = delayTime;
    }

    /**
     * get connectionTimeout.
     *
     * @return connectionTimeout
     */
    public Integer getConnectionTimeout() {
        return connectionTimeout;
    }

    /**
     * set connectionTimeout.
     *
     * @param connectionTimeout connectionTimeout
     */
    public void setConnectionTimeout(final Integer connectionTimeout) {
        this.connectionTimeout = connectionTimeout;
    }

    /**
     * Gets the value of readTimeout.
     *
     * @return the value of readTimeout
     */
    public Integer getReadTimeout() {
        return readTimeout;
    }

    /**
     * Sets the readTimeout.
     *
     * @param readTimeout readTimeout
     */
    public void setReadTimeout(final Integer readTimeout) {
        this.readTimeout = readTimeout;
    }

    /**
     * Gets the value of writeTimeout.
     *
     * @return the value of writeTimeout
     */
    public Integer getWriteTimeout() {
        return writeTimeout;
    }

    /**
     * Sets the writeTimeout.
     *
     * @param writeTimeout writeTimeout
     */
    public void setWriteTimeout(final Integer writeTimeout) {
        this.writeTimeout = writeTimeout;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        HttpConfig that = (HttpConfig) o;
        return Objects.equals(url, that.url)
                && Objects.equals(delayTime, that.delayTime)
                && Objects.equals(connectionTimeout, that.connectionTimeout)
                && Objects.equals(readTimeout, that.readTimeout)
                && Objects.equals(writeTimeout, that.writeTimeout);
    }

    @Override
    public int hashCode() {
        return Objects.hash(url, delayTime, connectionTimeout, readTimeout, writeTimeout);
    }

    @Override
    public String toString() {
        return "HttpConfig{"
                + "url='"
                + url
                + '\''
                + ", delayTime="
                + delayTime
                + ", connectionTimeout="
                + connectionTimeout
                + ", readTimeout="
                + readTimeout
                + ", writeTimeout="
                + writeTimeout
                + '}';
    }
}
