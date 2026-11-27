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

package org.apache.shenyu.register.client.api;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.register.client.api.ClientRegistrationDiagnostics.FailureCategory;
import org.apache.shenyu.register.client.api.retry.FailureRegistryTask;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeoutException;

/**
 * FailbackRegistryRepository .
 */
public abstract class FailbackRegistryRepository implements ShenyuClientRegisterRepository {

    private final Logger logger = LoggerFactory.getLogger(FailbackRegistryRepository.class);

    private final Map<String, Holder> concurrentHashMap = new ConcurrentHashMap<>();

    private final Timer timer;

    /**
     * Instantiates a new Failback registry repository.
     */
    public FailbackRegistryRepository() {
        this.timer = WheelTimerFactory.getSharedTimer();
    }

    /**
     * Persist metadata.
     *
     * @param metadata metadata
     */
    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        String identity = metaDataKey(metadata);
        try {
            recordPersisted(identity, this.doPersistInterface(metadata));
        } catch (Exception ex) {
            //If a failure occurs, it needs to be added to the retry list.
            logger.warn("Failed to persistInterface {}, cause:{}", metadata, ex.getMessage());
            ClientRegistrationDiagnostics.getInstance().recordRetrying(identity, categoryOf(ex), ex.getMessage(), this);
            this.addFailureMetaDataRegister(metadata);
        }
    }

    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        String identity = uriKey(registerDTO);
        try {
            recordPersisted(identity, this.doPersistURI(registerDTO));
        } catch (Exception ex) {
            //If a failure occurs, it needs to be added to the retry list.
            logger.warn("Failed to persistURI {}, cause:{}", registerDTO, ex.getMessage());
            ClientRegistrationDiagnostics.getInstance().recordRetrying(identity, categoryOf(ex), ex.getMessage(), this);
            this.addFailureUriDataRegister(registerDTO);
        }
    }

    /**
     * Persist apiDoc.
     *
     * @param registerDTO registerDTO
     */
    @Override
    public void persistApiDoc(final ApiDocRegisterDTO registerDTO) {
        String identity = apiDocKey(registerDTO);
        try {
            recordPersisted(identity, this.doPersistApiDoc(registerDTO));
        } catch (Exception ex) {
            logger.warn("Failed to persistApiDoc {}, cause:{}", registerDTO, ex.getMessage());
            ClientRegistrationDiagnostics.getInstance().recordRetrying(identity, categoryOf(ex), ex.getMessage(), this);
            this.addFailureApiDocRegister(registerDTO);
        }
    }

    @Override
    public void persistMcpTools(final McpToolsRegisterDTO registerDTO) {
        String identity = mcpKey(registerDTO);
        try {
            recordPersisted(identity, this.doPersistMcpTools(registerDTO));
        } catch (Exception ex) {
            logger.warn("Failed to persistMcpTools {}, cause:{}", registerDTO, ex.getMessage());
            ClientRegistrationDiagnostics.getInstance().recordRetrying(identity, categoryOf(ex), ex.getMessage(), this);
            this.addFailureMcpDocRegister(registerDTO);
        }
    }

    /**
     * doPersistApiDoc.
     *
     * @param apiDocRegisterDTO apiDocRegisterDTO
     * @return whether the registration was actually sent (false when deliberately skipped)
     */
    protected abstract boolean doPersistApiDoc(ApiDocRegisterDTO apiDocRegisterDTO);

    /**
     * Add failure meta data register.
     *
     * @param <T> the type parameter
     * @param t   the t
     */
    protected <T> void addFailureMetaDataRegister(final T t) {
        if (t instanceof MetaDataRegisterDTO) {
            MetaDataRegisterDTO dto = (MetaDataRegisterDTO) t;
            addToFail(new Holder(t, metaDataIdentity(dto), Constants.META_TYPE));
        }
    }

    /**
     * Add failure uri data register.
     *
     * @param <T> the type parameter
     * @param t   the t
     */
    protected <T> void addFailureUriDataRegister(final T t) {
        if (t instanceof URIRegisterDTO) {
            URIRegisterDTO dto = (URIRegisterDTO) t;
            addToFail(new Holder(t, uriPathIdentity(dto), Constants.URI));
        }
    }

    /**
     * Add failure ApiDoc data register.
     *
     * @param <T> the type parameter
     * @param t   the t
     */
    protected <T> void addFailureApiDocRegister(final T t) {
        if (t instanceof ApiDocRegisterDTO) {
            ApiDocRegisterDTO dto = (ApiDocRegisterDTO) t;
            addToFail(new Holder(t, apiDocIdentity(dto), Constants.API_DOC_TYPE));
        }
    }

    /**
     * Add failure mcp data register.
     *
     * @param <T> the type parameter
     * @param t   the t
     */
    protected <T> void addFailureMcpDocRegister(final T t) {
        if (t instanceof McpToolsRegisterDTO) {
            McpToolsRegisterDTO dto = (McpToolsRegisterDTO) t;
            addToFail(new Holder(dto, mcpPath(dto), Constants.MCP_TOOLS_TYPE));
        }
    }

    private static String metaDataIdentity(final MetaDataRegisterDTO dto) {
        return String.join(":", value(dto.getNamespaceId()), value(dto.getRpcType()), value(dto.getAppName()),
                value(dto.getContextPath()), value(dto.getServiceName()), value(dto.getMethodName()),
                value(dto.getParameterTypes()), value(dto.getRuleName()), value(dto.getHost()), value(dto.getPort()), value(dto.getPath()));
    }

    private static String uriPathIdentity(final URIRegisterDTO dto) {
        return String.join(":", value(dto.getNamespaceId()), value(dto.getProtocol()), value(dto.getAppName()),
                value(dto.getContextPath()), value(dto.getRpcType()), value(dto.getHost()), value(dto.getPort()));
    }

    private static String apiDocIdentity(final ApiDocRegisterDTO dto) {
        return String.join(":", value(dto.getContextPath()), value(dto.getApiPath()), value(dto.getHttpMethod()),
                value(dto.getRpcType()), value(dto.getVersion()));
    }

    static String uriKey(final URIRegisterDTO dto) {
        return uriPathIdentity(dto) + ":" + Constants.URI;
    }

    static String metaDataKey(final MetaDataRegisterDTO dto) {
        return metaDataIdentity(dto) + ":" + Constants.META_TYPE;
    }

    static String apiDocKey(final ApiDocRegisterDTO dto) {
        return apiDocIdentity(dto) + ":" + Constants.API_DOC_TYPE;
    }

    static String mcpKey(final McpToolsRegisterDTO dto) {
        return mcpPath(dto) + ":" + Constants.MCP_TOOLS_TYPE;
    }

    private static String mcpPath(final McpToolsRegisterDTO dto) {
        MetaDataRegisterDTO metadata = dto.getMetaDataRegisterDTO();
        return Objects.isNull(metadata)
                ? value(dto.getNamespaceId())
                : String.join(":", value(dto.getNamespaceId()), metaDataIdentity(metadata));
    }

    private static FailureCategory categoryOf(final Exception ex) {
        Throwable cursor = ex;
        while (Objects.nonNull(cursor)) {
            if (cursor instanceof TimeoutException) {
                return FailureCategory.TIMEOUT;
            }
            cursor = cursor.getCause();
        }
        return FailureCategory.TRANSPORT;
    }

    private static String value(final Object value) {
        return Objects.toString(value, "");
    }

    private void addToFail(final Holder t) {
        FailureRegistryTask registryTask = FailureRegistryTask.createOwned(t.getKey(), this);
        Holder newHolder = new Holder(t.getObj(), t.getPath(), t.getType(), registryTask);
        Holder oldObj = concurrentHashMap.put(t.getKey(), newHolder);
        if (Objects.nonNull(oldObj)) {
            if (Objects.nonNull(oldObj.getRetryTask())) {
                oldObj.getRetryTask().cancel();
            }
            logger.debug("Updated failback registration payload, {}", t.getPath());
        }
        timer.add(registryTask);
        if (concurrentHashMap.get(t.getKey()) != newHolder) {
            registryTask.cancel();
        }
        if (Objects.isNull(oldObj)) {
            logger.warn("Add to failback and wait for execution, {}", t.getPath());
        }
    }

    /**
     * Unconditionally remove a pending registration, retained for compatibility with custom retry tasks.
     * Do not pair this with {@link #accept(String)}: use {@link #retry(String)} to preserve concurrent failures.
     *
     * @param key the key
     */
    public void remove(final String key) {
        if (Objects.nonNull(concurrentHashMap.remove(key))) {
            ClientRegistrationDiagnostics.getInstance().recordRemoved(key, this);
        }
    }

    /**
     * Remove a pending registration only when the task still owns its holder.
     *
     * @param key the registration key
     * @param retryTask the retry task
     */
    public void remove(final String key, final FailureRegistryTask retryTask) {
        Holder holder = concurrentHashMap.get(key);
        if (Objects.nonNull(holder) && holder.getRetryTask() == retryTask) {
            concurrentHashMap.remove(key, holder);
            ClientRegistrationDiagnostics.getInstance().recordExhausted(key, this);
        }
    }

    /**
     * Attempt a pending registration without claiming it, retained for compatibility with custom retry tasks.
     * New retry tasks should use {@link #retry(String)} instead of an accept/remove pair.
     *
     * @param key the key
     */
    public void accept(final String key) {
        Holder holder = concurrentHashMap.get(key);
        if (Objects.isNull(holder)) {
            return;
        }
        ClientRegistrationDiagnostics.getInstance().recordAttempt(key, this);
        try {
            recordPersisted(key, persist(holder));
        } catch (RuntimeException ex) {
            ClientRegistrationDiagnostics.getInstance().recordRetrying(key, categoryOf(ex), ex.getMessage(), this);
            throw ex;
        }
    }

    /**
     * Retry a pending registration without removing failures queued during the attempt.
     *
     * @param key the registration key
     */
    public void retry(final String key) {
        Holder holder = concurrentHashMap.get(key);
        if (Objects.nonNull(holder)) {
            retry(key, holder.getRetryTask());
        }
    }

    /**
     * Retry a pending registration only when the task still owns its holder.
     *
     * @param key the registration key
     * @param retryTask the retry task
     */
    public void retry(final String key, final FailureRegistryTask retryTask) {
        Holder holder = concurrentHashMap.get(key);
        if (Objects.isNull(holder) || holder.getRetryTask() != retryTask || !concurrentHashMap.remove(key, holder)) {
            return;
        }
        ClientRegistrationDiagnostics.getInstance().recordAttempt(key, this);
        try {
            recordPersisted(key, persist(holder));
        } catch (RuntimeException ex) {
            ClientRegistrationDiagnostics.getInstance().recordRetrying(key, categoryOf(ex), ex.getMessage(), this);
            // A newer failure has its own timer task; otherwise retain this task's retry.
            if (Objects.isNull(concurrentHashMap.putIfAbsent(key, holder))) {
                throw ex;
            }
        }
    }

    /**
     * Retire the entries observed by this repository on orderly close. Entries observed by
     * another repository in the same process are left untouched. Subclasses overriding this
     * must call {@code super.closeRepository()}.
     */
    @Override
    public void closeRepository() {
        ClientRegistrationDiagnostics.getInstance().retireAll(this);
    }

    /**
     * Failure keys currently owned by the failback layer (package-private for tests).
     *
     * @return the pending failure keys
     */
    Set<String> pendingFailureKeys() {
        return Set.copyOf(concurrentHashMap.keySet());
    }

    /**
     * The retry task currently owning the pending registration (package-private for tests).
     *
     * @param key the registration key
     * @return the owning retry task, or {@code null} when nothing is pending
     */
    FailureRegistryTask pendingFailureTask(final String key) {
        Holder holder = concurrentHashMap.get(key);
        return Objects.isNull(holder) ? null : holder.getRetryTask();
    }

    private void recordPersisted(final String identity, final boolean sent) {
        if (sent) {
            ClientRegistrationDiagnostics.getInstance().recordRegistered(identity, this);
        } else {
            ClientRegistrationDiagnostics.getInstance().recordSkipped();
        }
    }

    private boolean persist(final Holder holder) {
        String type = holder.getType();
        switch (type) {
            case Constants.URI:
                return this.doPersistURI((URIRegisterDTO) holder.getObj());
            case Constants.META_TYPE:
                return this.doPersistInterface((MetaDataRegisterDTO) holder.getObj());
            case Constants.API_DOC_TYPE:
                return this.doPersistApiDoc((ApiDocRegisterDTO) holder.getObj());
            case Constants.MCP_TOOLS_TYPE:
                return this.doPersistMcpTools((McpToolsRegisterDTO) holder.getObj());
            default:
                return false;
        }
    }

    /**
     * Do persist uri.
     *
     * @param registerDTO the register dto
     */
    protected abstract boolean doPersistURI(URIRegisterDTO registerDTO);

    /**
     * Do persist interface.
     *
     * @param registerDTO the register dto
     */
    protected abstract boolean doPersistInterface(MetaDataRegisterDTO registerDTO);

    /**
     * Do persist interface.
     *
     * @param registerDTO registerDTO the register dto
     */
    protected abstract boolean doPersistMcpTools(McpToolsRegisterDTO registerDTO);

    private static class Holder {

        private final Object obj;

        private final String path;

        private final String type;

        private final FailureRegistryTask retryTask;

        /**
         * Instantiates a new Holder.
         *
         * @param obj  the obj
         * @param path the path
         * @param type the type
         */
        Holder(final Object obj, final String path, final String type) {
            this(obj, path, type, null);
        }

        /**
         * Instantiates a new Holder.
         *
         * @param obj the registration object
         * @param path the registration path
         * @param type the registration type
         * @param retryTask the retry task
         */
        Holder(final Object obj, final String path, final String type, final FailureRegistryTask retryTask) {
            this.obj = obj;
            this.path = path;
            this.type = type;
            this.retryTask = retryTask;
        }

        /**
         * Gets obj.
         *
         * @return the obj
         */
        public Object getObj() {
            return obj;
        }

        /**
         * Gets path.
         *
         * @return the path
         */
        public String getPath() {
            return path;
        }

        /**
         * Gets type.
         *
         * @return the type
         */
        public String getType() {
            return type;
        }

        public FailureRegistryTask getRetryTask() {
            return retryTask;
        }

        private String getKey() {
            return String.join(":", path, type);
        }
    }
}
