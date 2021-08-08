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

package org.apache.shenyu.register.server.etcd.client;


/**
 * etcd data node.
 */
public class Node {

    private String key;

    private String value;

    private long createReversion;

    private long modReversion;

    private long version;

    public Node(final String key, final String value, final long createReversion, final long modReversion,
                final long version) {
        this.key = key;
        this.value = value;
        this.createReversion = createReversion;
        this.modReversion = modReversion;
        this.version = version;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Node nodeInfo = (Node) o;

        if (createReversion != nodeInfo.createReversion) {
            return false;
        }
        if (modReversion != nodeInfo.modReversion) {
            return false;
        }
        if (version != nodeInfo.version) {
            return false;
        }
        if (!key.equals(nodeInfo.key)) {
            return false;
        }
        return value.equals(nodeInfo.value);
    }

    @Override
    public int hashCode() {
        int result = key.hashCode();
        result = 31 * result + value.hashCode();
        result = 31 * result + (int) (createReversion ^ (createReversion >>> 32));
        result = 31 * result + (int) (modReversion ^ (modReversion >>> 32));
        result = 31 * result + (int) (version ^ (version >>> 32));
        return result;
    }

    @Override
    public String toString() {
        return "NodeInfo{"
                + "key='" + key + '\''
                + ", value='" + value + '\''
                + ", createReversion=" + createReversion
                + ", modReversion=" + modReversion
                + ", version=" + version
                + '}';
    }

    /**
     * getKeygetKey.
     *
     * @return String
     */
    public String getKey() {
        return key;
    }

    /**
     * setKey.
     *
     * @param key key
     */
    public void setKey(final String key) {
        this.key = key;
    }

    /**
     * getValue.
     *
     * @return String
     */
    public String getValue() {
        return value;
    }

    /**
     * setValue.
     *
     * @param value value
     */
    public void setValue(final String value) {
        this.value = value;
    }

    /**
     * getCreateReversion.
     *
     * @return String
     */
    public long getCreateReversion() {
        return createReversion;
    }

    /**
     * setCreateReversion.
     *
     * @param createReversion createReversion
     */
    public void setCreateReversion(final long createReversion) {
        this.createReversion = createReversion;
    }

    /**
     * getModReversion.
     *
     * @return long
     */
    public long getModReversion() {
        return modReversion;
    }

    /**
     * setModReversion.
     *
     * @param modReversion modReversion
     */
    public void setModReversion(final long modReversion) {
        this.modReversion = modReversion;
    }

    /**
     * getVersion.
     *
     * @return long
     */
    public long getVersion() {
        return version;
    }

    /**
     * setVersion.
     *
     * @param version version
     */
    public void setVersion(final long version) {
        this.version = version;
    }
}
