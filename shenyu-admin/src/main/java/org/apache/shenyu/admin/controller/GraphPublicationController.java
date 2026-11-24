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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.GraphPublicationService;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.GraphData;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.authz.AuthorizationException;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.apache.shiro.subject.Subject;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.server.ResponseStatusException;

/**
 * Manages graph drafts and published desired state within one namespace.
 */
@RestApi("/graph/{namespaceId}")
@Transactional
public class GraphPublicationController {

    private static final String VIEW = "system:graph:view";

    private static final String EDIT = "system:graph:edit";

    private final GraphPublicationService graphs;

    private final NamespaceUserService namespaceUsers;

    public GraphPublicationController(final GraphPublicationService graphs, final NamespaceUserService namespaceUsers) {
        this.graphs = graphs;
        this.namespaceUsers = namespaceUsers;
    }

    /**
     * Create a graph draft.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @param graph editable definition
     * @return the operation result
     */
    @PostMapping("/{graphId}/draft")
    @RequiresPermissions(EDIT)
    public ShenyuAdminResult createDraft(@PathVariable("namespaceId") final String namespaceId,
                                         @PathVariable("graphId") final String graphId,
                                         @RequestBody final GraphData graph) {
        requireAccess(namespaceId, EDIT);
        return ShenyuAdminResult.success(graphs.createDraft(namespaceId, graphId, graph));
    }

    /**
     * Read the current draft and its edit version.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @return the operation result
     */
    @GetMapping("/{graphId}/draft")
    public ShenyuAdminResult getDraft(@PathVariable("namespaceId") final String namespaceId,
                                      @PathVariable("graphId") final String graphId) {
        requireAccess(namespaceId, VIEW);
        return ShenyuAdminResult.success(graphs.getDraft(namespaceId, graphId));
    }

    /**
     * Replace a draft at the expected edit version.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @param expectedDraftVersion last observed draft version
     * @param graph editable definition
     * @return the operation result
     */
    @PutMapping("/{graphId}/draft")
    @RequiresPermissions(EDIT)
    public ShenyuAdminResult updateDraft(@PathVariable("namespaceId") final String namespaceId,
                                         @PathVariable("graphId") final String graphId,
                                         @RequestParam("expectedDraftVersion") final long expectedDraftVersion,
                                         @RequestBody final GraphData graph) {
        requireAccess(namespaceId, EDIT);
        return ShenyuAdminResult.success(graphs.updateDraft(namespaceId, graphId, expectedDraftVersion, graph));
    }

    /**
     * Check a saved draft against publication rules.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @return the operation result
     */
    @PostMapping("/{graphId}/validate")
    public ShenyuAdminResult validateDraft(@PathVariable("namespaceId") final String namespaceId,
                                           @PathVariable("graphId") final String graphId) {
        requireAccess(namespaceId, VIEW);
        return ShenyuAdminResult.success(graphs.validateDraft(namespaceId, graphId));
    }

    /**
     * Validate and publish the selected draft version.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @param expectedDraftVersion last observed draft version
     * @return the operation result
     */
    @PostMapping("/{graphId}/publish")
    @RequiresPermissions(EDIT)
    public ShenyuAdminResult publish(@PathVariable("namespaceId") final String namespaceId,
                                     @PathVariable("graphId") final String graphId,
                                     @RequestParam("expectedDraftVersion") final long expectedDraftVersion) {
        requireAccess(namespaceId, EDIT);
        return ShenyuAdminResult.success(graphs.publish(namespaceId, graphId, expectedDraftVersion));
    }

    /**
     * List immutable graph revisions.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @return the operation result
     */
    @GetMapping("/{graphId}/revisions")
    public ShenyuAdminResult history(@PathVariable("namespaceId") final String namespaceId,
                                     @PathVariable("graphId") final String graphId) {
        requireAccess(namespaceId, VIEW);
        return ShenyuAdminResult.success(graphs.history(namespaceId, graphId));
    }

    /**
     * Read an immutable graph revision.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @param revision historical revision number
     * @return the operation result
     */
    @GetMapping("/{graphId}/revisions/{revision}")
    public ShenyuAdminResult getRevision(@PathVariable("namespaceId") final String namespaceId,
                                         @PathVariable("graphId") final String graphId,
                                         @PathVariable("revision") final long revision) {
        requireAccess(namespaceId, VIEW);
        return ShenyuAdminResult.success(graphs.getRevision(namespaceId, graphId, revision));
    }

    /**
     * Republish historical content under a new revision.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @param expectedDraftVersion last observed draft version
     * @param revision historical revision number
     * @return the operation result
     */
    @PostMapping("/{graphId}/revisions/{revision}/rollback")
    @RequiresPermissions(EDIT)
    public ShenyuAdminResult rollback(@PathVariable("namespaceId") final String namespaceId,
                                      @PathVariable("graphId") final String graphId,
                                      @RequestParam("expectedDraftVersion") final long expectedDraftVersion,
                                      @PathVariable("revision") final long revision) {
        requireAccess(namespaceId, EDIT);
        return ShenyuAdminResult.success(graphs.rollback(namespaceId, graphId, expectedDraftVersion, revision));
    }

    /**
     * Publish a graph deletion.
     *
     * @param namespaceId target namespace
     * @param graphId graph identity
     * @param expectedDraftVersion last observed draft version
     * @return the operation result
     */
    @DeleteMapping("/{graphId}")
    @RequiresPermissions(EDIT)
    public ShenyuAdminResult delete(@PathVariable("namespaceId") final String namespaceId,
                                    @PathVariable("graphId") final String graphId,
                                    @RequestParam("expectedDraftVersion") final long expectedDraftVersion) {
        requireAccess(namespaceId, EDIT);
        return ShenyuAdminResult.success(graphs.delete(namespaceId, graphId, expectedDraftVersion));
    }

    /**
     * Read committed desired state for namespace reconciliation.
     *
     * @param namespaceId target namespace
     * @return the operation result
     */
    @GetMapping("/snapshot")
    public ShenyuAdminResult snapshot(@PathVariable("namespaceId") final String namespaceId) {
        requireAccess(namespaceId, VIEW);
        return ShenyuAdminResult.success(graphs.snapshot(namespaceId));
    }

    @ExceptionHandler(AuthorizationException.class)
    ResponseEntity<ShenyuAdminResult> handlePermissionDenied() {
        HttpStatus status = SecurityUtils.getSubject().isAuthenticated() ? HttpStatus.FORBIDDEN : HttpStatus.UNAUTHORIZED;
        return ResponseEntity.status(status).body(ShenyuAdminResult.error(status.value(), "Graph access denied"));
    }

    @ExceptionHandler(ResponseStatusException.class)
    ResponseEntity<ShenyuAdminResult> handleGraphError(final ResponseStatusException exception) {
        int status = exception.getStatusCode().value();
        return ResponseEntity.status(status).body(ShenyuAdminResult.error(status, exception.getReason()));
    }

    @ExceptionHandler({HttpMessageNotReadableException.class, MissingServletRequestParameterException.class, MethodArgumentTypeMismatchException.class})
    ResponseEntity<ShenyuAdminResult> handleInvalidRequest() {
        return ResponseEntity.badRequest().body(ShenyuAdminResult.error(HttpStatus.BAD_REQUEST.value(), "Invalid graph request body or parameter"));
    }

    private void requireAccess(final String namespaceId, final String permission) {
        Subject subject = SecurityUtils.getSubject();
        if (!subject.isAuthenticated() || !(subject.getPrincipal() instanceof UserInfo user)) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "An authenticated dashboard user is required");
        }
        boolean permitted = subject.isPermitted(permission) || (VIEW.equals(permission) && subject.isPermitted(EDIT));
        if (!permitted || (!AdminConstants.ADMIN_NAME.equals(user.getUserName())
                && !namespaceUsers.lockMembership(namespaceId, user.getUserId()))) {
            throw new ResponseStatusException(HttpStatus.FORBIDDEN, "Graph access denied");
        }
    }
}
