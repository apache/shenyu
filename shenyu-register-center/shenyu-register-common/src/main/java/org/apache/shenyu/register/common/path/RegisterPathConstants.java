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

package org.apache.shenyu.register.common.path;

/**
 * zookeeper register center.
 *
 * @author xiaoyu
 */
public class RegisterPathConstants {
    
    /**
     * root path of zookeeper register center.
     */
    public static final String ROOT_PATH = "/soul/register";

    /**
     * constants of separator.
     */
    private static final String SEPARATOR = "/";
    
    /**
     * build child path of "/soul/register/metadata/{rpcType}/".
     *
     * @param rpcType rpc type
     * @return path string
     */
    public static String buildMetaDataContextPathParent(final String rpcType) {
        return String.join(SEPARATOR, ROOT_PATH, "metadata", rpcType);
    }
    
    /**
     * build child path of "/soul/register/metadata/{rpcType}/{contextPath}/".
     *
     * @param rpcType rpc type
     * @param contextPath context path
     * @return path string
     */
    public static String buildMetaDataParentPath(final String rpcType, final String contextPath) {
        return String.join(SEPARATOR, ROOT_PATH, "metadata", rpcType, contextPath);
    }
    
    /**
     * Build uri path string.
     * build child path of "/soul/register/uri/{rpcType}/".
     *
     * @param rpcType the rpc type
     * @return the string
     */
    public static String buildURIContextPathParent(final String rpcType) {
        return String.join(SEPARATOR, ROOT_PATH, "uri", rpcType);
    }
    
    /**
     * Build uri path string.
     * build child path of "/soul/register/uri/{rpcType}/{contextPath}/".
     *
     * @param rpcType the rpc type
     * @param contextPath the context path
     * @return the string
     */
    public static String buildURIParentPath(final String rpcType, final String contextPath) {
        return String.join(SEPARATOR, ROOT_PATH, "uri", rpcType, contextPath);
    }
    
    /**
     * Build uri read node string.
     *
     * @param rpcType the rpc type
     * @param contextPath the context path
     * @param nodeName the node name
     * @return the string
     */
    public static String buildURIReadNode(final String rpcType, final String contextPath, final String nodeName) {
        return buildRealNode(buildURIParentPath(rpcType, contextPath), nodeName);
    }
    
    /**
     * Build meta data child path string.
     *
     * @param rpcType the rpc type
     * @param contextPath the context path
     * @param nodeName the node name
     * @return the string
     */
    public static String buildMetaDataReadNode(final String rpcType, final String contextPath, final String nodeName) {
        return buildRealNode(buildMetaDataParentPath(rpcType, contextPath), nodeName);
    }
    
    /**
     * Build real node string.
     *
     * @param nodePath the node path
     * @param nodeName the node name
     * @return the string
     */
    public static String buildRealNode(final String nodePath, final String nodeName) {
        return String.join(SEPARATOR, nodePath, nodeName);
    }

    /**
     * Build nacos instance service path string.
     * build child path of "soul.register.service.{rpcType}".
     *
     * @param rpcType the rpc type
     * @return the string
     */
    public static String buildServiceInstancePath(final String rpcType) {
        return String.join(SEPARATOR, ROOT_PATH, "service", rpcType)
                .replace("/", ".").substring(1);
    }

    /**
     * Build nacos config service path string.
     * build child path of "soul.register.service.{rpcType}.{contextPath}".
     *
     * @param rpcType the rpc type
     * @param contextPath the context path
     * @return the string
     */
    public static String buildServiceConfigPath(final String rpcType, final String contextPath) {
        return String.join(SEPARATOR, ROOT_PATH, "service", rpcType, contextPath)
                .replace("/", ".").substring(1);
    }
}
