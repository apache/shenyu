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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthParamMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.mapper.AppAuthRequestMapper;
import org.apache.shenyu.admin.model.dto.AppAuthApplyRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthResultQueryDTO;
import org.apache.shenyu.admin.model.dto.AppAuthRevokeRequestDTO;
import org.apache.shenyu.admin.model.dto.AppAuthReplacePathsRequestDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AuthParamDO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.model.entity.AppAuthRequestDO;
import org.apache.shenyu.admin.model.vo.AppAuthResultVO;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.AuthParamData;
import org.apache.shenyu.common.dto.AuthPathData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.SignUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Transaction service: apply performs reservation, app_auth/param/path creation and association backfill
 * in a single database transaction. Lock order: idempotency key -> app_auth -> param/path; a CREATED row with authId=NULL is never committed.
 */
@Service
public class AppAuthTransactionService {

    private static final Logger LOG = LoggerFactory.getLogger(AppAuthTransactionService.class);

    private static final String STATE_CREATED = "CREATED";

    private static final String STATE_REVOKED = "REVOKED";

    private final AppAuthRequestMapper requestMapper;

    private final AppAuthMapper appAuthMapper;

    private final AuthParamMapper authParamMapper;

    private final AuthPathMapper authPathMapper;

    private final ApplicationEventPublisher eventPublisher;

    public AppAuthTransactionService(final AppAuthRequestMapper requestMapper,
                                        final AppAuthMapper appAuthMapper,
                                        final AuthParamMapper authParamMapper,
                                        final AuthPathMapper authPathMapper,
                                        final ApplicationEventPublisher eventPublisher) {
        this.requestMapper = requestMapper;
        this.appAuthMapper = appAuthMapper;
        this.authParamMapper = authParamMapper;
        this.authPathMapper = authPathMapper;
        this.eventPublisher = eventPublisher;
    }

    /**
     * Apply transaction body: reserve the key first, then create; a database error inside a transaction that already created an auth must never surface as success.
     *
     * @param callerKey caller key (passed in after facade authentication)
     * @param request apply request (already validated by the facade)
     * @param requestHash canonical request digest
     * @return the auth result (first creation and same-parameter replay return the same auth)
     */
    @Transactional(rollbackFor = Exception.class)
    public AppAuthResultVO apply(final String callerKey, final AppAuthApplyRequestDTO request, final String requestHash) {
        final AppAuthRequestDO candidate = new AppAuthRequestDO();
        candidate.setId(UUIDUtils.getInstance().generateShortUuid());
        candidate.setCallerKey(callerKey);
        candidate.setNamespaceId(request.getNamespaceId());
        candidate.setRequestId(request.getRequestId());
        candidate.setRequestHash(requestHash);
        candidate.setState(STATE_CREATED);
        reserveIfAbsent(candidate);

        final AppAuthRequestDO locked = requestMapper.selectByKeyForUpdate(
                callerKey, request.getNamespaceId(), request.getRequestId());
        if (Objects.isNull(locked)) {
            throw new ShenyuAdminException("Apply failed: idempotency record not found");
        }
        if (STATE_REVOKED.equals(locked.getState())) {
            throw new ShenyuAdminException("The request has been revoked; no auth may be created for it");
        }
        if (!candidate.getId().equals(locked.getId())) {
            // not the first reservation: same-parameter replay returns the same auth, different parameters are rejected
            if (!Objects.equals(requestHash, locked.getRequestHash())) {
                throw new ShenyuAdminException("The same requestId already exists with different parameters; use a new requestId");
            }
            if (Objects.nonNull(locked.getAuthId())) {
                return loadExistingResult(request, locked.getAuthId());
            }
            throw new ShenyuAdminException("Idempotency record has no auth association (inconsistent state); inspect manually requestId=" + request.getRequestId());
        }

        // first reservation: create the auth (fixed open=true, enabled=false) with its child rows
        final Timestamp now = new Timestamp(System.currentTimeMillis());
        final AppAuthDO auth = AppAuthDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .userId(request.getUserId())
                .phone(request.getPhone())
                .extInfo(request.getExtInfo())
                .appKey(SignUtils.generateKey())
                .appSecret(SignUtils.generateKey())
                .open(true)
                .enabled(false)
                .namespaceId(request.getNamespaceId())
                .dateCreated(now)
                .dateUpdated(now)
                .build();
        appAuthMapper.insert(auth);

        final AuthParamDO authParamDO = AuthParamDO.create(auth.getId(), request.getAppName(), request.getAppParam());
        authParamMapper.save(authParamDO);

        final List<AuthPathDO> paths = request.getPathList().stream()
                .map(path -> AuthPathDO.create(path, auth.getId(), request.getAppName()))
                .collect(Collectors.toList());
        if (!paths.isEmpty()) {
            authPathMapper.batchSave(paths);
        }

        final int completed = requestMapper.completeCreated(candidate.getId(), auth.getId(), STATE_CREATED);
        if (completed != 1) {
            throw new ShenyuAdminException("Apply failed: association backfill matched no row");
        }

        registerAfterCommitCreateEvent(auth, authParamDO, paths);
        return AppAuthResultVO.builder()
                .state(STATE_CREATED)
                .namespaceId(request.getNamespaceId())
                .requestId(request.getRequestId())
                .id(auth.getId())
                .appKey(auth.getAppKey())
                .appSecret(auth.getAppSecret())
                .enabled(auth.getEnabled())
                .open(auth.getOpen())
                .pathList(request.getPathList())
                .build();
    }

    /**
     * Result read-only transaction: no reservation, no credential generation, no apply call; consistent read by the caller's own triple key.
     *
     * @param callerKey caller key
     * @param query result query
     * @return NOT_FOUND / REVOKED (no credentials) or a complete CREATED (with credentials)
     */
    @Transactional(readOnly = true)
    public AppAuthResultVO result(final String callerKey, final AppAuthResultQueryDTO query) {
        final AppAuthRequestDO record = requestMapper.selectByKey(
                callerKey, query.getNamespaceId(), query.getRequestId());
        if (Objects.isNull(record)) {
            return AppAuthResultVO.builder()
                    .state("NOT_FOUND")
                    .namespaceId(query.getNamespaceId())
                    .requestId(query.getRequestId())
                    .build();
        }
        if (STATE_REVOKED.equals(record.getState())) {
            return AppAuthResultVO.builder()
                    .state(STATE_REVOKED)
                    .namespaceId(query.getNamespaceId())
                    .requestId(query.getRequestId())
                    .build();
        }
        if (Objects.isNull(record.getAuthId())) {
            throw new ShenyuAdminException("Request record has no auth association (inconsistent state); inspect manually requestId=" + query.getRequestId());
        }
        return loadExistingResult(query.getNamespaceId(), query.getRequestId(), record.getAuthId());
    }

    /**
     * Revoke transaction body: reserve the REVOKED tombstone first, then lock the same idempotency key; if created, delete the auth and child rows under the unified lock order and mark it.
     * requestId/hash and the historical authId are kept; late applies are rejected by the tombstone.
     *
     * @param callerKey caller key
     * @param request revoke request
     * @return the REVOKED result (no credentials)
     */
    @Transactional(rollbackFor = Exception.class)
    public AppAuthResultVO revoke(final String callerKey, final AppAuthRevokeRequestDTO request) {
        final AppAuthRequestDO candidate = new AppAuthRequestDO();
        candidate.setId(UUIDUtils.getInstance().generateShortUuid());
        candidate.setCallerKey(callerKey);
        candidate.setNamespaceId(request.getNamespaceId());
        candidate.setRequestId(request.getRequestId());
        candidate.setRequestHash(null);
        candidate.setAuthId(null);
        candidate.setState(STATE_REVOKED);
        reserveIfAbsent(candidate);

        final AppAuthRequestDO locked = requestMapper.selectByKeyForUpdate(
                callerKey, request.getNamespaceId(), request.getRequestId());
        if (Objects.isNull(locked)) {
            throw new ShenyuAdminException("Revoke failed: idempotency record not found");
        }
        if (candidate.getId().equals(locked.getId()) || STATE_REVOKED.equals(locked.getState())) {
            // new tombstone or repeated revoke: idempotent success with no sync event (nothing was deleted)
            return revokedResult(request);
        }
        // CREATED: delete the auth and child rows (lock order: key already locked -> app_auth -> param/path); any failure rolls everything back
        final String authId = locked.getAuthId();
        if (Objects.isNull(authId)) {
            throw new ShenyuAdminException("Revoke failed: CREATED record has no auth association (inconsistent state); inspect manually");
        }
        final List<String> ids = List.of(authId);
        final List<AppAuthDO> deletedAuths = appAuthMapper.selectByIds(ids);
        final int affectCount = appAuthMapper.deleteByIds(ids);
        if (affectCount <= 0) {
            throw new ShenyuAdminException("Revoke failed: auth no longer exists (inconsistent state); inspect manually");
        }
        authParamMapper.deleteByAuthIds(ids);
        authPathMapper.deleteByAuthIds(ids);
        requestMapper.markRevoked(locked.getId(), request.getReason(), STATE_REVOKED);
        registerAfterCommitDeleteEvent(deletedAuths);
        return revokedResult(request);
    }

    /**
     * ReplacePaths transaction body: re-validate ownership and state inside the transaction under the key -> app_auth -> path lock order, then replace as a whole;
     * an empty array really clears; REVOKED and open=false are always rejected without any write.
     *
     * @param callerKey caller key
     * @param request replacePaths request
     * @return the updated CREATED result (credentials are not resent; fetch them via result)
     */
    @Transactional(rollbackFor = Exception.class)
    public AppAuthResultVO replacePaths(final String callerKey, final AppAuthReplacePathsRequestDTO request) {
        if (Objects.isNull(request.getPathList())) {
            throw new ShenyuAdminException("pathList must not be null (empty array means clearing all paths)");
        }
        // lock order: lock the idempotency key first (triple key looked up via authId)
        final AppAuthRequestDO byAuth = requestMapper.selectByAuthId(request.getAuthId());
        if (Objects.isNull(byAuth)) {
            throw new ShenyuAdminException("App auth ownership check failed");
        }
        final AppAuthRequestDO locked = requestMapper.selectByKeyForUpdate(
                callerKey, request.getNamespaceId(), byAuth.getRequestId());
        if (Objects.isNull(locked) || !request.getAuthId().equals(locked.getAuthId())
                || !STATE_CREATED.equals(locked.getState())) {
            throw new ShenyuAdminException("App auth ownership check failed");
        }
        final AppAuthDO auth = appAuthMapper.selectById(request.getAuthId());
        if (Objects.isNull(auth) || !request.getNamespaceId().equals(auth.getNamespaceId())
                || !Boolean.TRUE.equals(auth.getOpen())) {
            throw new ShenyuAdminException("App auth ownership check failed (auth with open=false does not accept path replacement)");
        }
        final List<AuthParamDO> params = authParamMapper.findByAuthId(request.getAuthId());
        if (params.isEmpty()) {
            throw new ShenyuAdminException("Auth has no app parameters (inconsistent state); inspect manually");
        }
        final String appName = params.get(0).getAppName();

        // replace as a whole (the old updateDetailPath delete-only-if-nonempty semantics is deliberately not reused)
        authPathMapper.deleteByAuthId(request.getAuthId());
        final List<AuthPathDO> newPaths = request.getPathList().stream()
                .distinct()
                .map(path -> AuthPathDO.create(path, request.getAuthId(), appName))
                .collect(Collectors.toList());
        if (!newPaths.isEmpty()) {
            authPathMapper.batchSave(newPaths);
        }

        registerAfterCommitUpdateEvent(auth, params, newPaths);
        final List<String> pathList = newPaths.stream().map(AuthPathDO::getPath).collect(Collectors.toList());
        return AppAuthResultVO.builder()
                .state(STATE_CREATED)
                .namespaceId(request.getNamespaceId())
                .requestId(locked.getRequestId())
                .id(auth.getId())
                .appKey(null)
                .appSecret(null)
                .enabled(auth.getEnabled())
                .open(auth.getOpen())
                .pathList(pathList)
                .build();
    }

    /**
     * Publish the update event as a complete snapshot: the gateway subscribers replace the whole cached auth with
     * the payload, so the untouched application parameters must be carried along with the replaced paths.
     *
     * @param auth the auth being updated
     * @param params the persisted application parameters, part of the snapshot because replacePaths never touches them
     * @param paths the new path records
     */
    private void registerAfterCommitUpdateEvent(final AppAuthDO auth, final List<AuthParamDO> params,
                                                final List<AuthPathDO> paths) {
        try {
            final AppAuthData data = AppAuthData.builder()
                    .appKey(auth.getAppKey())
                    .appSecret(auth.getAppSecret())
                    .open(auth.getOpen())
                    .enabled(auth.getEnabled())
                    .namespaceId(auth.getNamespaceId())
                    .paramDataList(params.stream()
                            .map(param -> new AuthParamData(param.getAppName(), param.getAppParam()))
                            .collect(Collectors.toList()))
                    .pathDataList(paths.stream()
                            .map(path -> AuthPathData.builder()
                                    .appName(path.getAppName()).path(path.getPath()).enabled(path.getEnabled()).build())
                            .collect(Collectors.toList()))
                    .build();
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                    DataEventTypeEnum.UPDATE, List.of(data)));
        } catch (Exception e) {
            LOG.error("APP_AUTH_SYNC_FAILED: update event authId={}, errorType={}",
                    auth.getId(), e.getClass().getName());
        }
    }

    /**
     * Insert the reservation. A concurrent insert of the same unique key (Oracle MERGE can raise ORA-00001)
     * is not a failure: the following lock reads the winning row and the existing validation decides replay,
     * conflict, or revoke. Other database errors still roll the transaction back.
     *
     * @param candidate reservation candidate
     */
    private void reserveIfAbsent(final AppAuthRequestDO candidate) {
        try {
            requestMapper.reserveIfAbsent(candidate);
        } catch (DuplicateKeyException ex) {
            LOG.debug("APP_AUTH_RESERVE: unique key already inserted, callerKey={}, namespaceId={}, requestId={}",
                    candidate.getCallerKey(), candidate.getNamespaceId(), candidate.getRequestId());
        }
    }

    private AppAuthResultVO revokedResult(final AppAuthRevokeRequestDTO request) {
        return AppAuthResultVO.builder()
                .state(STATE_REVOKED)
                .namespaceId(request.getNamespaceId())
                .requestId(request.getRequestId())
                .build();
    }

    private AppAuthResultVO loadExistingResult(final AppAuthApplyRequestDTO request, final String authId) {
        return loadExistingResult(request.getNamespaceId(), request.getRequestId(), authId);
    }

    private AppAuthResultVO loadExistingResult(final String namespaceId, final String requestId, final String authId) {
        final AppAuthDO auth = appAuthMapper.selectById(authId);
        if (Objects.isNull(auth)) {
            throw new ShenyuAdminException("Auth associated with the idempotency record does not exist (inconsistent state); inspect manually");
        }
        final List<AuthPathDO> pathRecords = authPathMapper.findByAuthId(authId);
        final List<String> pathList = Objects.isNull(pathRecords)
                ? List.of()
                : pathRecords.stream().map(AuthPathDO::getPath).collect(Collectors.toList());
        return AppAuthResultVO.builder()
                .state(STATE_CREATED)
                .namespaceId(namespaceId)
                .requestId(requestId)
                .id(auth.getId())
                .appKey(auth.getAppKey())
                .appSecret(auth.getAppSecret())
                .enabled(auth.getEnabled())
                .open(auth.getOpen())
                .pathList(pathList)
                .build();
    }

    private void registerAfterCommitDeleteEvent(final List<AppAuthDO> deletedAuths) {
        try {
            final List<AppAuthData> dataList = deletedAuths.stream()
                    .map(auth -> AppAuthData.builder()
                            .appKey(auth.getAppKey())
                            .appSecret(auth.getAppSecret())
                            .open(auth.getOpen())
                            .enabled(auth.getEnabled())
                            .namespaceId(auth.getNamespaceId())
                            .build())
                    .collect(Collectors.toList());
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                    DataEventTypeEnum.DELETE, dataList));
        } catch (Exception e) {
            LOG.error("APP_AUTH_SYNC_FAILED: delete event authCount={}, errorType={}",
                    deletedAuths.size(), e.getClass().getName());
        }
    }

    private void registerAfterCommitCreateEvent(final AppAuthDO auth, final AuthParamDO authParamDO, final List<AuthPathDO> paths) {
        try {
            final AppAuthData data = AppAuthData.builder()
                    .appKey(auth.getAppKey())
                    .appSecret(auth.getAppSecret())
                    .open(auth.getOpen())
                    .enabled(auth.getEnabled())
                    .namespaceId(auth.getNamespaceId())
                    .paramDataList(List.of(new AuthParamData(authParamDO.getAppName(), authParamDO.getAppParam())))
                    .pathDataList(paths.stream()
                            .map(path -> AuthPathData.builder()
                                    .appName(path.getAppName()).path(path.getPath()).enabled(path.getEnabled()).build())
                            .collect(Collectors.toList()))
                    .build();
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH,
                    DataEventTypeEnum.CREATE, List.of(data)));
        } catch (Exception e) {
            // event-sending failures never masquerade as a database rollback: log a sanitized warning; recovery goes through the full-sync entry
            LOG.error("APP_AUTH_SYNC_FAILED: authId={}, namespaceId={}, errorType={}",
                    auth.getId(), auth.getNamespaceId(), e.getClass().getName());
        }
    }
}
