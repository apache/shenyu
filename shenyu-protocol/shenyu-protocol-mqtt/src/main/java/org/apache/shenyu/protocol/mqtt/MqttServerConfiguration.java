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

package org.apache.shenyu.protocol.mqtt;

import org.apache.shenyu.protocol.mqtt.utils.EncryptUtil;

/**
 * mqtt server configuration.
 */
public class MqttServerConfiguration {

    private int port;

    private int bossGroupThreadCount;

    private int maxPayloadSize;

    private int workerGroupThreadCount;

    private String userName;

    private String password;

    private Boolean isEncryptPassword = false;

    private String encryptMode;

    /**
     * init mqtt env.
     */
    public void afterPropertiesSet() {

        if (isEncryptPassword()) {
            setPassword(encryptPassword());
        }
        MqttEnv env = new MqttEnv();
        env.setPassword(getPassword());
        env.setPort(getPort());
        env.setMaxPayloadSize(getMaxPayloadSize());
        env.setUserName(getUserName());
        env.setWorkerGroupThreadCount(getWorkerGroupThreadCount());
    }

    private String encryptPassword() {
        return EncryptUtil.choose(getEncryptMode(), getPassword());
    }

    /**
     * get port.
     * @return port
     */
    public int getPort() {
        return port;
    }

    /**
     * set port.
     * @param port port
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * get bossGroupThreadCount.
     * @return bossGroupThreadCount
     */
    public int getBossGroupThreadCount() {
        return bossGroupThreadCount;
    }

    /**
     * set bossGroupThreadCount.
     * @param bossGroupThreadCount bossGroupThreadCount
     */
    public void setBossGroupThreadCount(final int bossGroupThreadCount) {
        this.bossGroupThreadCount = bossGroupThreadCount;
    }

    /**
     * get maxPayloadSize.
     * @return maxPayloadSize
     */
    public int getMaxPayloadSize() {
        return maxPayloadSize;
    }

    /**
     * set maxPayloadSize.
     * @param maxPayloadSize maxPayloadSize
     */
    public void setMaxPayloadSize(final int maxPayloadSize) {
        this.maxPayloadSize = maxPayloadSize;
    }

    /**
     * get workerGroupThreadCount.
     * @return workerGroupThreadCount
     */
    public int getWorkerGroupThreadCount() {
        return workerGroupThreadCount;
    }

    /**
     * set workerGroupThreadCount.
     * @param workerGroupThreadCount workerGroupThreadCount
     */
    public void setWorkerGroupThreadCount(final int workerGroupThreadCount) {
        this.workerGroupThreadCount = workerGroupThreadCount;
    }

    /**
     * get userName.
     * @return userName
     */
    public String getUserName() {
        return userName;
    }

    /**
     * set userName.
     * @param userName userName
     */
    public void setUserName(final String userName) {
        this.userName = userName;
    }

    /**
     * get password.
     * @return password
     */
    public String getPassword() {
        return password;
    }

    /**
     * set password.
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * is encrypt password.
     * @return isEncryptPassword.
     */
    public boolean isEncryptPassword() {
        return isEncryptPassword;
    }

    /**
     * set encryptPassword.
     * @param encryptPassword encryptPassword
     */
    public void setEncryptPassword(final boolean encryptPassword) {
        isEncryptPassword = encryptPassword;
    }

    /**
     * get encryptMode.
     * @return encryptMode
     */
    public String getEncryptMode() {
        return encryptMode;
    }

    /**
     * set encryptMode.
     * @param encryptMode encryptMode
     */
    public void setEncryptMode(final String encryptMode) {
        this.encryptMode = encryptMode;
    }
}
