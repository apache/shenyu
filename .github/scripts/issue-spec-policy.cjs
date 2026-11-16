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

const {createHash} = require('node:crypto');
const {execFile} = require('node:child_process');
const {readFileSync, writeFileSync} = require('node:fs');
const {join} = require('node:path');
const {promisify} = require('node:util');

const TOOL_REVISION = 'ab3e3944b052cc353ab4983c4a8a09e5e09fc2dc';
const POLICY_MODE = 'observe';
const CHECK_NAME = 'issue-spec-policy';
const BASE_BRANCH = 'master';
const PHASES = ['proposal', 'design', 'implement'];
const FIELDS = ['mode', 'reason', ...PHASES, 'specs', 'tasks', 'validation'];
const exec = promisify(execFile);

function visibleLines(body, keepComments = false) {
    let text = String(body ?? '').replace(/\r\n?/g, '\n');
    if (!keepComments) text = text.replace(/<!--[\s\S]*?(?:-->|$)/g, '');
    let fence;
    return text.split('\n').filter(line => {
        const marker = line.match(/^\s{0,3}(`{3,}|~{3,})/);
        if (marker) {
            if (!fence) fence = marker[1];
            else if (marker[1][0] === fence[0] && marker[1].length >= fence.length) fence = undefined;
            return false;
        }
        return !fence && !/^(?: {4}|\t|>)/.test(line);
    });
}

function meaningful(value) {
    return Boolean(value && !/^(?:n\/a|none|todo|tbd|\.{3}|describe\b.*|list the actual\b.*)$/i.test(value.trim()));
}

function issueNumber(value, repo) {
    const escaped = (repo.owner + '/' + repo.repo).replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
    const match = String(value ?? '').match(new RegExp('^(?:#|https://github\\.com/' + escaped + '/issues/)([1-9][0-9]*)$', 'i'));
    if (!match || !Number.isSafeInteger(Number(match[1]))) return null;
    return Number(match[1]);
}

function parseMetadata(body, repo) {
    const fields = {};
    const findings = [];
    for (const line of visibleLines(body)) {
        const match = line.match(/^IssueSpec-([A-Za-z-]+):[ \t]*(.*?)\s*$/i);
        if (!match) continue;
        const key = match[1].toLowerCase();
        if (!FIELDS.includes(key)) findings.push('Unknown IssueSpec field: ' + key + '.');
        else if (Object.hasOwn(fields, key)) findings.push('Duplicate IssueSpec field: ' + key + '.');
        else fields[key] = match[2];
    }
    if (!['full', 'lightweight'].includes(fields.mode)) findings.push('Set IssueSpec-Mode to full or lightweight.');
    for (const field of ['reason', 'validation']) {
        if (!meaningful(fields[field])) findings.push('Fill IssueSpec-' + field + ' with the actual scope or verification.');
    }
    const phases = Object.fromEntries(PHASES.map(phase => [phase, issueNumber(fields[phase], repo)]));
    const ids = {};
    if (fields.mode === 'full') {
        for (const phase of PHASES) {
            if (!phases[phase]) findings.push('IssueSpec-' + phase + ' must reference an issue in this repository.');
        }
        if (new Set(Object.values(phases)).size !== PHASES.length) findings.push('Proposal, Design and Implement must be distinct issues.');
        for (const type of ['specs', 'tasks']) {
            const prefix = type === 'specs' ? 'SPEC' : 'TASK';
            ids[type] = (fields[type] ?? '').split(',').map(value => value.trim());
            if (ids[type].length > 100 || ids[type].some(id => !new RegExp('^' + prefix + '-[0-9]{3,}$').test(id))
                    || new Set(ids[type]).size !== ids[type].length) {
                findings.push('IssueSpec-' + type + ' must list distinct ' + prefix + ' IDs, separated by commas (maximum 100).');
            }
        }
    } else if (fields.mode === 'lightweight' && [...PHASES, 'specs', 'tasks'].some(field => Object.hasOwn(fields, field))) {
        findings.push('Remove artifact-reference fields for lightweight changes, or select full.');
    }
    return {fields, phases, ids, findings};
}

function lightweightPath(path) {
    if (/^(?:\.github|\.agents|\.claude|issue-spec)\//.test(path) || /(?:^|\/)(?:AGENTS|CLAUDE)\.md$/i.test(path)) return false;
    return /(?:^|\/)docs\//.test(path) || /(?:^|\/)README[^/]*\.md$/i.test(path)
        || /^[^/]+\.md$/i.test(path) || /\/src\/test\//.test(path);
}

function lightweightDiff(files) {
    return files.length > 0 && files.every(file => lightweightPath(file.filename)
        && (!file.previous_filename || lightweightPath(file.previous_filename)));
}

function digest(value) {
    return createHash('sha256').update(JSON.stringify(value)).digest('hex');
}

function prIdentity(pr) {
    return {head: pr.head.sha, base: pr.base.sha, branch: pr.base.ref, state: pr.state,
        body: pr.body ?? '', labels: pr.labels.map(label => label.name).sort()};
}

async function listAll(method, args, maxPages = 10) {
    const items = [];
    for (let page = 1; page <= maxPages; page++) {
        const {data} = await method({...args, per_page: 100, page});
        if (!Array.isArray(data)) throw new Error('Unexpected paginated API response.');
        items.push(...data);
        if (data.length < 100) return items;
    }
    throw new Error('API result exceeds the policy pagination bound; rerun after narrowing the change.');
}

function phaseIdentity(issue, phase, repo, expectedPredecessor) {
    if (issue.pull_request) throw new Error('The ' + phase + ' reference is a pull request, not an issue.');
    const lines = visibleLines(issue.body, true);
    const markers = lines.filter(line => /^\s*<!--\s*issue-spec:issue=/.test(line));
    const marker = markers[0]?.match(/^<!--\s*issue-spec:issue=(proposal|design|implement)\s+change=([^\s>]+)\s+version=1\s*-->$/);
    if (markers.length !== 1 || !marker || marker[1] !== phase) throw new Error('The ' + phase + ' issue needs one canonical version-1 phase marker.');
    if (expectedPredecessor) {
        const label = phase === 'design' ? 'Proposal' : 'Design';
        const links = visibleLines(issue.body).map(line => line.match(new RegExp('^\\s*-\\s*' + label + '\\s+Issue\\s*:\\s*(.*?)\\s*$', 'i'))).filter(Boolean);
        if (links.length !== 1 || issueNumber(links[0][1], repo) !== expectedPredecessor) {
            throw new Error('The ' + phase + ' issue must link the selected ' + label + ' predecessor.');
        }
    }
    return marker[2];
}

async function readPhases(github, repo, phases) {
    const inputs = [];
    for (const phase of PHASES) {
        const number = phases[phase];
        const {data: issue} = await github.rest.issues.get({...repo, issue_number: number});
        const previous = phase === 'design' ? phases.proposal : phase === 'implement' ? phases.design : null;
        const change = phaseIdentity(issue, phase, repo, previous);
        const comments = await listAll(github.rest.issues.listComments, {...repo, issue_number: number});
        inputs.push({number, change, body: issue.body, state: issue.state,
            comments: comments.map(comment => ({id: comment.id, body: comment.body, updated: comment.updated_at})).sort((a, b) => a.id - b.id)});
    }
    if (new Set(inputs.map(input => input.change)).size !== 1) throw new Error('The three phase issues belong to different changes.');
    return digest(inputs);
}

async function hasMaintainerClassification(github, repo, pr) {
    const comments = await listAll(github.rest.issues.listComments, {...repo, issue_number: pr.number});
    const candidates = comments.map(comment => ({comment, decisions: visibleLines(comment.body)
        .map(line => line.match(/^IssueSpec-Classification: (lightweight|full) ([a-f0-9]{40})$/)).filter(Boolean)}))
        .filter(item => item.decisions.length === 1 && item.decisions[0][2] === pr.head.sha)
        .sort((a, b) => b.comment.id - a.comment.id);
    if (candidates.length > 50) throw new Error('Too many classification candidates; no policy verdict.');
    const permissions = new Map();
    for (const {comment, decisions} of candidates) {
        const actor = comment.user?.login;
        if (!actor) continue;
        if (!permissions.has(actor)) {
            const {data} = await github.rest.repos.getCollaboratorPermissionLevel({...repo, username: actor});
            permissions.set(actor, ['write', 'maintain', 'admin'].includes(data.permission));
        }
        if (permissions.get(actor)) return decisions[0][1] === 'lightweight';
    }
    return false;
}

async function discover({github, context, core}) {
    let numbers = [];
    if (context.payload.pull_request) numbers = [context.payload.pull_request.number];
    else if (context.eventName === 'workflow_dispatch') {
        const number = Number(context.payload.inputs?.pull_request);
        if (!Number.isSafeInteger(number) || number <= 0) throw new Error('A positive pull_request number is required.');
        numbers = [number];
    } else if (context.payload.issue?.pull_request) {
        const body = (context.payload.comment?.body ?? '') + '\n' + (context.payload.changes?.body?.from ?? '');
        if (/IssueSpec-Classification:/i.test(body)) numbers = [context.payload.issue.number];
    } else if (context.payload.issue) {
        // Ordinary repository conversations do not cause a scan of every PR.
        const body = (context.payload.issue.body ?? '') + '\n' + (context.payload.changes?.body?.from ?? '');
        if (/<!--\s*issue-spec:issue=/.test(body)) {
            const pulls = await listAll(github.rest.pulls.list, {...context.repo, state: 'open', base: BASE_BRANCH}, 20);
            numbers = pulls.filter(pr => Object.values(parseMetadata(pr.body, context.repo).phases).includes(context.payload.issue.number))
                .map(pr => pr.number);
        }
    }
    if (numbers.length > 200) throw new Error('More than 200 affected PRs; use manual dispatch for bounded review.');
    core.setOutput('pull_requests', JSON.stringify(numbers));
    core.setOutput('has_targets', numbers.length > 0 ? 'true' : 'false');
    return numbers;
}

function statePath(directory, number) {
    if (!Number.isSafeInteger(number) || number <= 0) throw new Error('Invalid pull request number.');
    return join(directory, 'issue-spec-policy-' + number + '.json');
}

async function prepare({github, context, core, directory, number, mode = POLICY_MODE}) {
    if (!['observe', 'enforce'].includes(mode)) throw new Error('Unknown policy mode.');
    const path = statePath(directory, number);
    const {data: pr} = await github.rest.pulls.get({...context.repo, pull_number: number});
    if (pr.state !== 'open' || pr.base.ref !== BASE_BRANCH) {
        core.info('No policy evaluation for a closed PR or another target branch.');
        return;
    }
    const {data: check} = await github.rest.checks.create({...context.repo, name: CHECK_NAME, head_sha: pr.head.sha,
        status: 'in_progress', external_id: 'issue-spec-pr-' + number,
        output: {title: 'Reading IssueSpec policy inputs', summary: 'Mode: ' + mode + '. This check does not approve or merge a PR.'}});
    const metadata = parseMetadata(pr.body, context.repo);
    const state = {number, checkId: check.id, mode, pr: prIdentity(pr), metadata, findings: [...metadata.findings], incomplete: false};
    try {
        if (pr.changed_files >= 3000) throw new Error('The PR reaches GitHub file-list bounds; classify it manually after splitting the change.');
        const files = await listAll(github.rest.pulls.listFiles, {...context.repo, pull_number: number}, 30);
        if (files.length !== pr.changed_files) throw new Error('The changed-file listing is incomplete.');
        const required = pr.labels.some(label => label.name === 'issuespec:required');
        state.needsClassification = metadata.fields.mode === 'lightweight' && !lightweightDiff(files) && !required;
        if (metadata.fields.mode === 'lightweight' && (required
                || (state.needsClassification && !await hasMaintainerClassification(github, context.repo, pr)))) {
            state.findings.push('Use full planning, or obtain maintainer classification for a behavior-preserving fix; issuespec:required always wins.');
        }
        if (metadata.fields.mode === 'full' && metadata.findings.length === 0) {
            state.specDigest = await readPhases(github, context.repo, metadata.phases);
        }
    } catch (error) {
        state.incomplete = true;
        state.findings.push(error.status ? 'GitHub input lookup failed (HTTP ' + error.status + '); no policy verdict.' : error.message);
    }
    writeFileSync(path, JSON.stringify(state), {mode: 0o600});
    core.setOutput('prepared', 'true');
    core.setOutput('needs_cli', state.specDigest && state.findings.length === 0 ? 'true' : 'false');
    core.setOutput('tool_revision', TOOL_REVISION);
    return state;
}

async function runTool(binary, args) {
    try {
        const {stdout} = await exec(binary, ['--profile', 'github', ...args], {timeout: 120000, maxBuffer: 2 * 1024 * 1024});
        return {code: 0, data: JSON.parse(stdout)};
    } catch (error) {
        if (Number.isInteger(error.code) && error.stdout) {
            try { return {code: error.code, data: JSON.parse(error.stdout)}; } catch { /* Report an invalid CLI response below. */ }
        }
        throw new Error('The pinned IssueSpec CLI failed, timed out, or returned invalid JSON; no policy verdict.');
    }
}

function validateArtifacts(metadata, status, artifacts) {
    const findings = [];
    if (status.ok !== true) findings.push('IssueSpec implement planning gate is not satisfied; inspect issue-spec status for blockers.');
    const selected = new Map();
    for (const artifact of artifacts) {
        if (selected.has(artifact.id)) findings.push('Duplicate active artifact: ' + artifact.id + '.');
        selected.set(artifact.id, artifact);
    }
    for (const id of metadata.ids.specs) {
        const artifact = selected.get(id);
        if (!artifact || artifact.type !== 'SPEC' || artifact.status !== 'confirmed') findings.push(id + ' is not an active confirmed SPEC.');
    }
    for (const id of metadata.ids.tasks) {
        const artifact = selected.get(id);
        if (!artifact || artifact.type !== 'TASK' || !['ready', 'in-progress', 'done'].includes(artifact.status)) {
            findings.push(id + ' is not an active ready/in-progress/done TASK.');
        }
    }
    const edges = status.relationships?.edges;
    if (!Array.isArray(edges)) findings.push('The pinned CLI did not return a relationship index.');
    else for (const spec of metadata.ids.specs) {
        if (!edges.some(edge => edge.kind === 'task-covers-spec' && metadata.ids.tasks.includes(edge.owner?.id) && edge.target?.id === spec)) {
            findings.push(spec + ' is not covered by a selected TASK.');
        }
    }
    return findings;
}

async function finish({github, context, core, directory, number, binary, tool = runTool}) {
    const state = JSON.parse(readFileSync(statePath(directory, number), 'utf8'));
    const {metadata} = state;
    try {
        if (state.specDigest && state.findings.length === 0) {
            const repo = context.repo.owner + '/' + context.repo.repo;
            const config = await tool(binary, ['workflow', 'validate', '--repo', repo, '--json']);
            if (config.code !== 0 || config.data.ok !== true) throw new Error('IssueSpec project configuration validation failed.');
            const phases = PHASES.flatMap(phase => ['--' + phase, String(metadata.phases[phase])]);
            const status = await tool(binary, ['status', '--repo', repo, ...phases, '--gate', 'implement', '--json']);
            if (status.code > 1 || typeof status.data.ok !== 'boolean') throw new Error('IssueSpec planning output is incomplete.');
            const artifacts = [];
            for (const phase of PHASES) {
                const result = await tool(binary, ['comment', 'list', '--repo', repo, '--issue', String(metadata.phases[phase]),
                    '--active-only', '--include-all-links', '--json']);
                if (result.code !== 0 || result.data.ok !== true || !Array.isArray(result.data.comments)) {
                    throw new Error('IssueSpec artifact lookup failed.');
                }
                artifacts.push(...result.data.comments);
            }
            state.findings.push(...validateArtifacts(metadata, status.data, artifacts));
        }
        const {data: current} = await github.rest.pulls.get({...context.repo, pull_number: number});
        if (digest(prIdentity(current)) !== digest(state.pr)) throw new Error('PR head, body, labels or base changed during validation; rerun.');
        if (state.needsClassification && !await hasMaintainerClassification(github, context.repo, current)) {
            throw new Error('Current-head maintainer classification is absent or revoked; use full planning or request classification.');
        }
        if (state.specDigest && await readPhases(github, context.repo, metadata.phases) !== state.specDigest) {
            throw new Error('IssueSpec inputs changed during validation; rerun.');
        }
    } catch (error) {
        state.incomplete = true;
        state.findings.push(error.status ? 'GitHub validation lookup failed (HTTP ' + error.status + '); rerun.' : error.message);
    }
    const valid = !state.incomplete && state.findings.length === 0;
    const conclusion = valid ? 'success' : state.mode === 'observe' ? 'neutral' : 'failure';
    const title = valid ? 'Planning policy satisfied' : state.mode === 'observe' ? 'Advisory: policy needs attention' : 'Planning policy not satisfied';
    const summary = [
        'Mode: ' + state.mode + '. Candidate: `' + state.pr.head + '`.',
        'IssueSpec revision: `' + TOOL_REVISION + '`.',
        state.specDigest ? 'Observed specification digest: `' + state.specDigest + '`.' : 'No full specification snapshot was validated.',
        '',
        ...(state.findings.length ? state.findings.map(finding => '- ' + finding) : ['Selected planning metadata passed.']),
        '',
        'This is not a test result, independent review, approval or merge-readiness receipt.',
        'See issue-spec/project/workflow.md for scope, examples and manual rerun instructions.',
    ].join('\n');
    await github.rest.checks.update({...context.repo, check_run_id: state.checkId, status: 'completed',
        conclusion, output: {title, summary}});
    await core.summary.addHeading('IssueSpec policy #' + number).addRaw(summary).write();
    return {conclusion, findings: state.findings, incomplete: state.incomplete};
}

module.exports = {TOOL_REVISION, POLICY_MODE, parseMetadata, issueNumber, lightweightDiff, phaseIdentity,
    validateArtifacts, discover, prepare, finish, runTool};
