// Licensed to the Apache Software Foundation (ASF) under one or more
// contributor license agreements.  See the NOTICE file distributed with
// this work for additional information regarding copyright ownership.
// The ASF licenses this file to You under the Apache License, Version 2.0
// (the "License"); you may not use this file except in compliance with
// the License.  You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

const MAX_ASSIGNEES = 10;
const USERNAME_PATTERN = /^@?[a-zA-Z0-9-]+$/;
const COMMAND_PATTERN = /^\/([a-z][a-z0-9-]*)(?:[ \t]+(.*?))?[ \t]*$/;

function noArguments(argument) {
    return argument === '' ? {} : null;
}

function userArguments(argument, required) {
    if (!argument && !required) {
        return {users: []};
    }
    if (!argument) {
        return null;
    }
    const users = argument.split(/[ \t]+/);
    if (users.some(user => !USERNAME_PATTERN.test(user))) {
        return null;
    }
    return {users: [...new Set(users.map(user => user.replace(/^@/, '').toLowerCase()))]};
}

function parseCommands(body) {
    const commands = [];
    let inCodeBlock = false;
    for (const rawLine of String(body ?? '').split(/\r?\n/)) {
        const line = rawLine.trim();
        if (line.startsWith('```')) {
            inCodeBlock = !inCodeBlock;
            continue;
        }
        if (inCodeBlock || !line.startsWith('/')) {
            continue;
        }
        const match = line.match(COMMAND_PATTERN);
        if (!match) {
            continue;
        }
        const command = COMMANDS[match[1].toLowerCase()];
        if (!command) {
            continue;
        }
        const parsed = command.parse(match[2] ?? '');
        if (parsed !== null) {
            commands.push({name: match[1].toLowerCase(), ...parsed});
        }
    }
    return commands;
}

function normalizeUsers(users, fallback) {
    return users.length ? users : [fallback.toLowerCase()];
}

function isOpenPullRequest(pullRequest) {
    return pullRequest.state === 'open';
}

function isClosedPullRequest(pullRequest) {
    return pullRequest.state === 'closed';
}

async function authorize(pullRequest, context, github) {
    if (context.payload.comment.user.login.toLowerCase() === pullRequest.user.login.toLowerCase()) {
        return true;
    }
    const {data: access} = await github.rest.repos.getCollaboratorPermissionLevel({
        ...context.repo,
        username: context.payload.comment.user.login,
    });
    return ['write', 'maintain', 'admin'].includes(access.permission);
}

async function addLabel({context, github}, label) {
    await github.rest.issues.addLabels({
        ...context.repo,
        issue_number: context.payload.issue.number,
        labels: [label],
    });
}

async function removeLabel({context, github}, label) {
    await github.rest.issues.removeLabel({
        ...context.repo,
        issue_number: context.payload.issue.number,
        name: label,
    });
}

async function assignUsers({context, github}, users) {
    const assignees = normalizeUsers(users, context.payload.comment.user.login);
    if (assignees.length > MAX_ASSIGNEES) {
        throw new Error('A pull request can have at most 10 assignees.');
    }
    const {data: updated} = await github.rest.issues.addAssignees({
        ...context.repo,
        issue_number: context.payload.issue.number,
        assignees,
    });
    const assigned = new Set(updated.assignees.map(user => user.login.toLowerCase()));
    const missing = assignees.filter(user => !assigned.has(user));
    if (missing.length) {
        throw new Error(`Could not assign: ${missing.join(', ')}.`);
    }
}

async function unassignUsers({context, github}, users) {
    await github.rest.issues.removeAssignees({
        ...context.repo,
        issue_number: context.payload.issue.number,
        assignees: normalizeUsers(users, context.payload.comment.user.login),
    });
}

async function mentionUsers({context, github}, prefix, users) {
    await github.rest.issues.createComment({
        ...context.repo,
        issue_number: context.payload.issue.number,
        body: `${prefix}: ${users.map(user => `@${user}`).join(' ')}`,
    });
}

async function submitLgtm({context, github, pullRequest}) {
    await github.rest.pulls.createReview({
        ...context.repo,
        pull_number: context.payload.issue.number,
        commit_id: pullRequest.head.sha,
        event: 'APPROVE',
        body: `Approved via /lgtm by @${context.payload.comment.user.login}.`,
    });
}

async function removeLgtm({context, github}) {
    const reviews = await github.paginate(github.rest.pulls.listReviews, {
        ...context.repo,
        pull_number: context.payload.issue.number,
        per_page: 100,
    });
    const review = [...reviews].reverse().find(candidate => candidate.state === 'APPROVED'
        && candidate.body?.startsWith('Approved via /lgtm by @'));
    if (!review) {
        return false;
    }
    await github.rest.pulls.dismissReview({
        ...context.repo,
        pull_number: context.payload.issue.number,
        review_id: review.id,
        message: `Dismissed via /unlgtm by @${context.payload.comment.user.login}.`,
    });
    return true;
}

async function closePullRequest({context, github, pullRequest}) {
    if (!await authorize(pullRequest, context, github)) {
        return false;
    }
    await github.rest.pulls.update({
        ...context.repo,
        pull_number: context.payload.issue.number,
        state: 'closed',
    });
    return true;
}

async function reopenPullRequest({context, github, pullRequest}) {
    if (!await authorize(pullRequest, context, github)) {
        return false;
    }
    await github.rest.pulls.update({
        ...context.repo,
        pull_number: context.payload.issue.number,
        state: 'open',
    });
    return true;
}

async function retestPullRequest({context, github, core, pullRequest}) {
    if (!pullRequest.head.repo) {
        return false;
    }
    const runs = await github.paginate(github.rest.actions.listWorkflowRunsForRepo, {
        ...context.repo,
        event: 'pull_request',
        head_sha: pullRequest.head.sha,
        per_page: 100,
    });
    const latest = new Map();
    for (const run of runs) {
        if (run.event !== 'pull_request' || run.head_sha !== pullRequest.head.sha
            || run.head_repository?.id !== pullRequest.head.repo.id || run.head_branch !== pullRequest.head.ref
            || (run.pull_requests?.length && !run.pull_requests.some(pull => pull.number === pullRequest.number))) {
            continue;
        }
        const previous = latest.get(run.workflow_id);
        if (!previous || run.run_number > previous.run_number) {
            latest.set(run.workflow_id, run);
        }
    }

    let requested = 0;
    const errors = [];
    const isFailed = run => run.status === 'completed' && ['failure', 'timed_out'].includes(run.conclusion);
    const needsFullRerun = async run => {
        if (!(run.path || '').endsWith('e2e-k8s.yml')) {
            return false;
        }
        const artifacts = await github.paginate(github.rest.actions.listWorkflowRunArtifacts, {
            ...context.repo,
            run_id: run.id,
            per_page: 100,
        });
        return !artifacts.some(artifact => artifact.name === 'shenyu-images' && !artifact.expired);
    };
    for (const run of latest.values()) {
        if (!isFailed(run)) {
            continue;
        }
        try {
            const {data: currentPullRequest} = await github.rest.pulls.get({
                ...context.repo,
                pull_number: context.payload.issue.number,
            });
            if (!isOpenPullRequest(currentPullRequest) || currentPullRequest.head.sha !== pullRequest.head.sha) {
                core.info('The pull request changed; stopping this retest request.');
                break;
            }
            const {data: currentRun} = await github.rest.actions.getWorkflowRun({...context.repo, run_id: run.id});
            if (!isFailed(currentRun) || currentRun.run_attempt !== run.run_attempt) {
                core.info(`Run ${run.id} has already changed; skipping it.`);
                continue;
            }
            if (await needsFullRerun(run)) {
                await github.rest.actions.reRunWorkflow({...context.repo, run_id: run.id});
                core.info(`Requested full rerun, the e2e image artifact is gone: ${run.html_url}`);
            } else {
                await github.rest.actions.reRunWorkflowFailedJobs({...context.repo, run_id: run.id});
                core.info(`Requested failed-job rerun: ${run.html_url}`);
            }
            requested++;
        } catch (error) {
            errors.push(run.id);
            core.warning(`Could not rerun ${run.html_url}: ${error.message}`);
        }
    }
    core.info(`Requested reruns for ${requested} workflow(s) at ${pullRequest.head.sha}.`);
    if (errors.length) {
        throw new Error(`Failed to request reruns for workflow run(s): ${errors.join(', ')}.`);
    }
    return true;
}

const COMMANDS = Object.freeze({
    assign: {parse: argument => userArguments(argument, false), execute: assignUsers, requiresOpen: true},
    unassign: {parse: argument => userArguments(argument, false), execute: unassignUsers, requiresOpen: true},
    cc: {parse: argument => userArguments(argument, true), execute: ({context, github}, command) =>
        mentionUsers({context, github}, 'cc', command.users), requiresOpen: true},
    uncc: {parse: argument => userArguments(argument, true), execute: ({context, github}, command) =>
        mentionUsers({context, github}, 'uncc', command.users), requiresOpen: true},
    lgtm: {parse: noArguments, execute: submitLgtm, requiresOpen: true},
    unlgtm: {parse: noArguments, execute: removeLgtm, requiresOpen: false},
    hold: {parse: noArguments, execute: ({context, github}) => addLabel({context, github}, 'hold'), requiresOpen: true},
    unhold: {parse: noArguments, execute: ({context, github}) => removeLabel({context, github}, 'hold'), requiresOpen: false},
    close: {parse: noArguments, execute: closePullRequest, requiresOpen: true},
    reopen: {parse: noArguments, execute: reopenPullRequest, requiresOpen: false},
    retest: {parse: noArguments, execute: retestPullRequest, requiresOpen: true},
});

async function run({context, github, core, allowedCommands}) {
    const {issue, comment, action} = context.payload ?? {};
    if (action !== 'created' || !issue?.pull_request || !comment) {
        return;
    }
    const commandFilter = allowedCommands && new Set(allowedCommands);
    const commands = parseCommands(comment.body).filter(command => !commandFilter || commandFilter.has(command.name));
    if (!commands.length) {
        return;
    }

    const pullRequestRequest = {...context.repo, pull_number: issue.number};
    const {data: pullRequest} = await github.rest.pulls.get(pullRequestRequest);
    const failures = [];
    for (const command of commands) {
        const definition = COMMANDS[command.name];
        if ((definition.requiresOpen && !isOpenPullRequest(pullRequest))
            || (command.name === 'reopen' && !isClosedPullRequest(pullRequest))) {
            core.info(`Skipping /${command.name} for pull request in state ${pullRequest.state}.`);
            continue;
        }
        try {
            const completed = await definition.execute({context, github, core, pullRequest}, command);
            if (completed !== false) {
                if (command.name === 'close') {
                    pullRequest.state = 'closed';
                } else if (command.name === 'reopen') {
                    pullRequest.state = 'open';
                }
                await github.rest.reactions.createForIssueComment({
                    ...context.repo,
                    comment_id: comment.id,
                    content: 'rocket',
                });
            }
        } catch (error) {
            failures.push(`/${command.name}: ${error.message}`);
            core.error(`Could not process /${command.name}: ${error.stack ?? error.message}`);
        }
    }
    if (failures.length) {
        core.setFailed(failures.join(' '));
    }
}

module.exports = {COMMANDS, parseCommands, run};
