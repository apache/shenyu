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

package org.apache.shenyu.common.dto.convert.plugin;

import java.io.Serializable;
import java.util.Objects;

/**
 * The type dubbo register config.
 */
public class DubboRegisterConfig implements Serializable {

    private static final long serialVersionUID = -4156204056244019979L;

    private String register;

    private String group;

    private String protocol;

    /**
     * get register.
     *
     * @return register
     */
    public String getRegister() {
        return register;
    }

    /**
     * set register.
     *
     * @param register register
     */
    public void setRegister(final String register) {
        this.register = register;
    }

    /**
     * get group.
     *
     * @return group
     */
    public String getGroup() {
        return group;
    }

    /**
     * set group.
     *
     * @param group group
     */
    public void setGroup(final String group) {
        this.group = group;
    }

    /**
     * get protocol.
     *
     * @return protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * set protocol.
     *
     * @param protocol protocol
     */
    public void setProtocol(final String protocol) {
        this.protocol = protocol;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        DubboRegisterConfig that = (DubboRegisterConfig) o;
        return Objects.equals(register, that.register) && Objects.equals(group, that.group) && Objects.equals(protocol, that.protocol);
    }

    @Override
    public int hashCode() {
        return Objects.hash(register, group, protocol);
    }

    @Override
    public String toString() {
        return "DubboRegisterConfig{"
                + "register='"
                + register
                + '\''
                + ", group='"
                + group
                + '\''
                + ", protocol='"
                + protocol
                + '\''
                + '}';
    }
}
