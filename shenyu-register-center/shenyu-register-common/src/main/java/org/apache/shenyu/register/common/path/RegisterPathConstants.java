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
 */
public class RegisterPathConstants {

    /**
     * uri register path pattern.
     * e.g. /shenyu/register/uri/{rpcType}/{context}/{urlInstance}
     */
    public static final String REGISTER_URI_INSTANCE_PATH = "/shenyu/register/uri/*/*/*";

    /**
     * metadata register path pattern.
     * e.g. /shenyu/register/metadata/{rpcType}/{context}/{metadata}
     */
    public static final String REGISTER_METADATA_INSTANCE_PATH = "/shenyu/register/metadata/*/*/*";

    /**
     * root path of zookeeper register center.
     */
    public static final String ROOT_PATH = "/shenyu/register";

    /**
     * constants of separator.
     */
    private static final String SEPARATOR = "/";

    /**
     * Dot separator.
     */
    private static final String DOT_SEPARATOR = ".";

    /**
     * build child path of "/shenyu/register/metadata/{rpcType}/".
     *
     * @param rpcType rpc type
     * @return path string
     */
    public static String buildMetaDataContextPathParent(final String rpcType) {
        return String.join(SEPARATOR, ROOT_PATH, "metadata", rpcType);
    }
    
    /**
     * build child path of "/shenyu/register/metadata/{rpcType}/{contextPath}/".
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
     * build child path of "/shenyu/register/uri/{rpcType}/".
     *
     * @param rpcType the rpc type
     * @return the string
     */
    public static String buildURIContextPathParent(final String rpcType) {
        return String.join(SEPARATOR, ROOT_PATH, "uri", rpcType);
    }
    
    /**
     * Build uri path string.
     * build child path of "/shenyu/register/uri/{rpcType}/{contextPath}/".
     *
     * @param rpcType the rpc type
     * @param contextPath the context path
     * @return the string
     */
    public static String buildURIParentPath(final String rpcType, final String contextPath) {
        return String.join(SEPARATOR, ROOT_PATH, "uri", rpcType, contextPath);
    }
    
    /**
     * Build instance parent path string.
     * build child path of "/shenyu/register/instance/
     *
     * @return the string
     */
    public static String buildInstanceParentPath() {
        return String.join(SEPARATOR, ROOT_PATH, "instance");
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
     * build child path of "shenyu.register.service.{rpcType}".
     *
     * @param rpcType the rpc type
     * @return the string
     */
    public static String buildServiceInstancePath(final String rpcType) {
        return String.join(SEPARATOR, ROOT_PATH, "service", rpcType)
                .replace("/", DOT_SEPARATOR).substring(1);
    }
    
    /**
     * Build nacos config service path string.
     * build child path of "shenyu.register.service.{rpcType}.{contextPath}".
     *
     * @param rpcType the rpc type
     * @param contextPath the context path
     * @return the string
     */
    public static String buildServiceConfigPath(final String rpcType, final String contextPath) {
        final String serviceConfigPathOrigin = String.join(SEPARATOR, ROOT_PATH, "service", rpcType, contextPath)
                .replace("/", DOT_SEPARATOR).replace("*", "");
        final String serviceConfigPathAfterSubstring = serviceConfigPathOrigin.substring(1);
        if (serviceConfigPathAfterSubstring.endsWith(".")) {
            return serviceConfigPathAfterSubstring.substring(0, serviceConfigPathAfterSubstring.length() - 1);
        }
        return serviceConfigPathAfterSubstring;
    }
    
    /**
     * Build node name by DOT_SEPARATOR.
     *
     * @param serviceName the service name
     * @param methodName the method name
     * @return the string
     */
    public static String buildNodeName(final String serviceName, final String methodName) {
        return String.join(DOT_SEPARATOR, serviceName, methodName);
    }
}
