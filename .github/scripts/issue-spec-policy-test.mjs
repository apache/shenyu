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

import assert from 'node:assert/strict';
import {mkdtempSync, readFileSync, rmSync} from 'node:fs';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {test} from 'node:test';

const require = createRequire(import.meta.url);
const policy = require('./issue-spec-policy.cjs');
const repo = {owner: 'apache', repo: 'shenyu'};
const root = new URL('../../', import.meta.url);
const light = [
    'IssueSpec-Mode: lightweight',
    'IssueSpec-Reason: Correct a broken documentation link.',
    'IssueSpec-Validation: Checked the target heading and relative link.',
].join('\n');
const full = [
    'IssueSpec-Mode: full',
    'IssueSpec-Reason: Add explicitly authorized resource reads.',
    'IssueSpec-Proposal: #101',
    'IssueSpec-Design: #102',
    'IssueSpec-Implement: https://github.com/apache/shenyu/issues/103',
    'IssueSpec-Specs: SPEC-101001',
    'IssueSpec-Tasks: TASK-102001',
    'IssueSpec-Validation: Regression tests passed; live upstream tests not run.',
].join('\n');

function phaseBody(phase, change = 'resource-access') {
    const previous = phase === 'design' ? '- Proposal Issue: #101\n' : phase === 'implement' ? '- Design Issue: #102\n' : '';
    return '<!-- issue-spec:issue=' + phase + ' change=' + change + ' version=1 -->\n' + previous;
}

function fixture(t, options = {}) {
    const directory = mkdtempSync(join(tmpdir(), 'shenyu-issue-policy-test-'));
    t.after(() => rmSync(directory, {recursive: true, force: true}));
    const f = {
        calls: [], outputs: {}, summaries: [], directory,
        files: options.files ?? [{filename: 'README.md'}],
        comments: {42: options.classification ? [{id: 10, user: {login: 'maintainer'}, body: 'IssueSpec-Classification: lightweight ' + 'a'.repeat(40)}] : [], 101: [], 102: [], 103: []},
        phases: Object.fromEntries(['proposal', 'design', 'implement'].map((phase, index) =>
            [101 + index, {number: 101 + index, body: phaseBody(phase), state: 'open'}])),
        permission: options.permission ?? 'write',
        status: {ok: true, relationships: {edges: [{kind: 'task-covers-spec', owner: {id: 'TASK-102001'}, target: {id: 'SPEC-101001'}}]}},
        artifacts: {
            101: [{id: 'SPEC-101001', type: 'SPEC', status: 'confirmed'}],
            102: [{id: 'TASK-102001', type: 'TASK', status: 'ready'}],
            103: [],
        },
    };
    f.pr = {
        number: 42, state: 'open', body: options.body ?? light,
        head: {sha: 'a'.repeat(40), repo: {full_name: 'contributor/shenyu'}},
        base: {sha: 'b'.repeat(40), ref: 'master'},
        labels: (options.labels ?? []).map(name => ({name})),
        changed_files: f.files.length,
    };
    const api = (name, result) => async args => {
        f.calls.push({name, args: structuredClone(args)});
        if (f.fail === name) throw Object.assign(new Error('API unavailable'), {status: 503});
        return {data: structuredClone(typeof result === 'function' ? result(args) : result)};
    };
    const page = (items, args) => items.slice((args.page - 1) * 100, args.page * 100);
    f.github = {rest: {
        pulls: {
            get: api('pull', () => f.pr),
            listFiles: api('files', args => page(f.files, args)),
            list: api('pulls', args => page(f.pulls ?? [f.pr], args)),
        },
        issues: {
            get: api('issue', args => f.phases[args.issue_number]),
            listComments: api('comments', args => page(f.comments[args.issue_number], args)),
        },
        repos: {getCollaboratorPermissionLevel: api('permission', () => ({permission: f.permission}))},
        checks: {create: api('create-check', {id: 1234}), update: api('update-check', {})},
    }};
    f.core = {
        info: () => {},
        setOutput: (key, value) => { f.outputs[key] = value; },
        summary: {
            addHeading() { return this; },
            addRaw(value) { f.summaries.push(value); return this; },
            async write() {},
        },
    };
    f.context = {repo, eventName: 'pull_request_target', payload: {pull_request: {number: 42}}};
    f.tool = async (binary, args) => {
        f.calls.push({name: 'tool', binary, args});
        if (f.toolError) throw new Error('CLI unavailable');
        if (args[0] === 'workflow') return {code: 0, data: {ok: true}};
        if (args[0] === 'status') return {code: f.status.ok ? 0 : 1, data: f.status};
        const number = Number(args[args.indexOf('--issue') + 1]);
        return {code: 0, data: {ok: true, comments: f.artifacts[number]}};
    };
    const args = () => ({github: f.github, context: f.context, core: f.core, directory, number: 42});
    f.prepare = mode => policy.prepare({...args(), mode});
    f.finish = () => policy.finish({...args(), binary: '/trusted/issue-spec', tool: f.tool});
    f.run = async mode => { await f.prepare(mode); return f.finish(); };
    return f;
}

test('metadata reads visible fields and ignores examples, quotes and comments', () => {
    const body = [
        '<!-- IssueSpec-Mode: full -->',
        '~~~text', full, '~~~',
        '> IssueSpec-Mode: full',
        '    IssueSpec-Mode: full',
        light,
    ].join('\n');
    assert.deepEqual(policy.parseMetadata(body, repo).findings, []);
    assert.equal(policy.parseMetadata(body, repo).fields.mode, 'lightweight');
    assert.deepEqual(policy.parseMetadata(full.replaceAll('\n', '\r\n'), repo).findings, []);
});

test('duplicate, unknown, unfilled and ambiguous fields never qualify', () => {
    for (const body of [
        light + '\nIssueSpec-Mode: full',
        light + '\nIssueSpec-Exception: yes',
        light.replace('Correct a broken documentation link.', 'TODO'),
        light + '\nIssueSpec-Proposal: #1',
        full.replace('TASK-102001', 'TASK-102001, TASK-102001'),
        full.replace('#102', '#101'),
        full.replace('SPEC-101001', 'SPEC-ID'),
        readFileSync(new URL('.github/PULL_REQUEST_TEMPLATE.md', root), 'utf8'),
    ]) assert.ok(policy.parseMetadata(body, repo).findings.length > 0, body);
});

test('only exact same-repository issue references are accepted', () => {
    for (const value of ['#42', 'https://github.com/apache/shenyu/issues/42']) assert.equal(policy.issueNumber(value, repo), 42);
    for (const value of ['42', '#0', '#1; touch /tmp/x', 'https://github.com/apache/other/issues/42',
        'https://github.com/apache/shenyu/pull/42', 'https://github.com/apache/shenyu/issues/42#issuecomment-1',
        'https://github.com.evil/apache/shenyu/issues/42', '#99999999999999999999']) {
        assert.equal(policy.issueNumber(value, repo), null, value);
    }
});

test('automatic lightweight classification checks all files and rename origins', () => {
    for (const path of ['README.md', 'docs/guide.md', 'shenyu-common/docs/guide.md', 'shenyu-common/src/test/java/Test.java']) {
        assert.equal(policy.lightweightDiff([{filename: path}]), true, path);
    }
    for (const path of ['AGENTS.md', 'module/AGENTS.md', 'issue-spec/project/workflow.md', '.agents/skills/example/SKILL.md',
        '.github/PULL_REQUEST_TEMPLATE.md', '.github/workflows/ci.yml', 'pom.xml', 'db/init/mysql/schema.sql',
        'shenyu-common/src/main/java/Example.java']) {
        assert.equal(policy.lightweightDiff([{filename: path}]), false, path);
        assert.equal(policy.lightweightDiff([{filename: 'docs/guide.md', previous_filename: path}]), false, path);
    }
    assert.equal(policy.lightweightDiff([]), false);
});

test('phase references require the correct kind, version, lineage and issue type', () => {
    assert.equal(policy.phaseIdentity({body: phaseBody('design')}, 'design', repo, 101), 'resource-access');
    for (const issue of [
        {body: phaseBody('proposal')},
        {body: phaseBody('design').replace('#101', '#999')},
        {body: phaseBody('design').replace('version=1', 'version=2')},
        {body: phaseBody('design') + phaseBody('design')},
        {body: '~~~\n' + phaseBody('design') + '~~~'},
        {body: phaseBody('design'), pull_request: {}},
    ]) assert.throws(() => policy.phaseIdentity(issue, 'design', repo, 101));
});

test('documentation and fork PRs receive a successful head-bound check without CLI installation', async t => {
    const f = fixture(t);
    const result = await f.run();
    assert.equal(result.conclusion, 'success');
    assert.equal(f.outputs.needs_cli, 'false');
    assert.equal(f.calls.find(call => call.name === 'create-check').args.head_sha, f.pr.head.sha);
    assert.equal(f.calls.filter(call => call.name === 'tool').length, 0);
    assert.match(f.summaries[0], /not a test result/);
});

test('missing metadata is advisory and does not masquerade as a passed policy', async t => {
    const f = fixture(t, {body: 'Fixes #8390'});
    assert.equal((await f.run()).conclusion, 'neutral');
    assert.match(f.summaries[0], /Set IssueSpec-Mode/);
});

test('enforcement is explicit and failures become blocking only in enforce mode', async t => {
    assert.equal(policy.POLICY_MODE, 'observe');
    const f = fixture(t, {body: ''});
    assert.equal((await f.run('enforce')).conclusion, 'failure');
    await assert.rejects(() => f.prepare('anything'), /Unknown policy mode/);
});

test('a contributor cannot self-exempt production changes through PR text', async t => {
    const f = fixture(t, {files: [{filename: 'shenyu-common/src/main/java/Example.java'}]});
    assert.equal((await f.run()).conclusion, 'neutral');
    assert.match(f.summaries[0], /maintainer classification/);
});

for (const permission of ['write', 'maintain', 'admin']) {
    test('lightweight production fixes can be classified by a verified ' + permission + ' actor', async t => {
        const f = fixture(t, {files: [{filename: 'pom.xml'}], classification: true, permission});
        assert.equal((await f.run()).conclusion, 'success');
        assert.equal(f.calls.find(call => call.name === 'permission').args.username, 'maintainer');
    });
}

for (const permission of ['read', 'triage', 'none']) {
    test('a ' + permission + ' actor cannot grant lightweight classification', async t => {
        const f = fixture(t, {files: [{filename: 'pom.xml'}], classification: true, permission});
        assert.equal((await f.run()).conclusion, 'neutral');
    });
}

test('latest authorized current-head classification wins; missing history grants nothing', async t => {
    const f = fixture(t, {files: [{filename: 'pom.xml'}], classification: true});
    f.comments[42].push({id: 11, user: {login: 'maintainer'}, body: 'IssueSpec-Classification: full ' + f.pr.head.sha});
    assert.equal((await f.run()).conclusion, 'neutral');
    f.comments[42] = [];
    assert.equal((await f.run()).conclusion, 'neutral');
});

test('required classification wins even for docs and a maintainer exception', async t => {
    const f = fixture(t, {labels: ['issuespec:required'], classification: true});
    assert.equal((await f.run()).conclusion, 'neutral');
    assert.equal(f.calls.filter(call => call.name === 'permission').length, 0);
});

test('classification cannot survive a new head, removal, or an untrusted author', async t => {
    for (const variant of ['head', 'deleted', 'untrusted']) {
        const f = fixture(t, {files: [{filename: 'pom.xml'}], classification: true});
        await f.prepare();
        if (variant === 'head') f.pr.head.sha = 'c'.repeat(40);
        if (variant === 'deleted') f.comments[42] = [];
        if (variant === 'untrusted') f.permission = 'read';
        assert.equal((await f.finish()).conclusion, 'neutral', variant);
    }
});

test('file pagination and rename classification do not inspect only the first page', async t => {
    const files = Array.from({length: 100}, (_, i) => ({filename: 'docs/' + i + '.md'}));
    files.push({filename: 'docs/moved.md', previous_filename: 'pom.xml'});
    const f = fixture(t, {files});
    assert.equal((await f.run()).conclusion, 'neutral');
    assert.equal(f.calls.filter(call => call.name === 'files').length, 2);
});

test('truncated file listings and permission API failure are inconclusive', async t => {
    const f = fixture(t);
    f.pr.changed_files++;
    assert.equal((await f.run()).incomplete, true);
    f.pr.changed_files = 3000;
    assert.match((await f.run()).findings.join(' '), /file-list bounds/);
    const g = fixture(t, {files: [{filename: 'pom.xml'}], classification: true});
    g.fail = 'permission';
    assert.equal((await g.run()).incomplete, true);
});

test('full planning checks active selected artifacts and task coverage with the pinned CLI', async t => {
    const f = fixture(t, {body: full, files: [{filename: 'pom.xml'}]});
    assert.equal((await f.run()).conclusion, 'success');
    assert.equal(f.outputs.tool_revision, policy.TOOL_REVISION);
    const calls = f.calls.filter(call => call.name === 'tool');
    assert.equal(calls.length, 5);
    assert.deepEqual(calls[1].args, ['status', '--repo', 'apache/shenyu', '--proposal', '101', '--design', '102',
        '--implement', '103', '--gate', 'implement', '--json']);
    assert.match(f.summaries[0], /specification digest: `[a-f0-9]{64}`/);
});

test('a green optional planning gate cannot replace missing selected SPECs or TASKs', async t => {
    const f = fixture(t, {body: full});
    f.artifacts = {101: [], 102: [], 103: []};
    f.status.relationships.edges = [];
    const result = await f.run();
    assert.equal(result.conclusion, 'neutral');
    assert.match(result.findings.join(' '), /not an active confirmed SPEC/);
    assert.match(result.findings.join(' '), /not covered by a selected TASK/);
});

for (const problem of ['draft-spec', 'blocked-task', 'uncovered-spec', 'blocking-question', 'duplicate-id']) {
    test('full planning rejects ' + problem, async t => {
        const f = fixture(t, {body: full});
        if (problem === 'draft-spec') f.artifacts[101][0].status = 'draft';
        if (problem === 'blocked-task') f.artifacts[102][0].status = 'blocked';
        if (problem === 'uncovered-spec') f.status.relationships.edges = [];
        if (problem === 'blocking-question') f.status.ok = false;
        if (problem === 'duplicate-id') f.artifacts[103].push(f.artifacts[101][0]);
        assert.equal((await f.run()).conclusion, 'neutral');
    });
}

test('unrelated change lineage is refused before running the tool', async t => {
    const f = fixture(t, {body: full});
    f.phases[102].body = phaseBody('design', 'different-change');
    const result = await f.run();
    assert.equal(result.incomplete, true);
    assert.match(result.findings.join(' '), /different changes/);
    assert.equal(f.calls.filter(call => call.name === 'tool').length, 0);
});

test('tool and remote lookup failures cannot produce a passing policy', async t => {
    const f = fixture(t, {body: full});
    f.toolError = true;
    assert.equal((await f.run()).incomplete, true);
    const g = fixture(t, {body: full});
    g.fail = 'comments';
    assert.equal((await g.run()).conclusion, 'neutral');
});

test('head and body changes during validation invalidate the observed candidate', async t => {
    for (const field of ['head', 'body', 'labels']) {
        const f = fixture(t);
        await f.prepare();
        if (field === 'head') f.pr.head.sha = 'c'.repeat(40);
        if (field === 'body') f.pr.body += '\nUpdated intent';
        if (field === 'labels') f.pr.labels.push({name: 'issuespec:required'});
        assert.equal((await f.finish()).incomplete, true, field);
    }
});

test('edited or deleted typed comments invalidate an otherwise green snapshot', async t => {
    const f = fixture(t, {body: full});
    f.comments[101] = [{id: 1, body: 'original', updated_at: '2026-11-01'}];
    await f.prepare();
    f.comments[101] = [{id: 1, body: 'changed', updated_at: '2026-11-02'}];
    assert.match((await f.finish()).findings.join(' '), /inputs changed/);
    await f.prepare();
    f.comments[101] = [];
    assert.match((await f.finish()).findings.join(' '), /inputs changed/);
});

test('closed PRs and other base branches do not create policy checks', async t => {
    const f = fixture(t);
    f.pr.state = 'closed';
    await f.prepare();
    f.pr.state = 'open';
    f.pr.base.ref = 'other';
    await f.prepare();
    assert.equal(f.calls.filter(call => call.name === 'create-check').length, 0);
});

test('check publication failure is surfaced instead of claiming success', async t => {
    const f = fixture(t);
    f.fail = 'update-check';
    await assert.rejects(() => f.run(), /API unavailable/);
});

test('discovery handles PR and manual triggers with numeric input only', async t => {
    const f = fixture(t);
    const discover = () => policy.discover(f);
    assert.deepEqual(await discover(), [42]);
    f.context.eventName = 'workflow_dispatch';
    f.context.payload = {inputs: {pull_request: '43'}};
    assert.deepEqual(await discover(), [43]);
    f.context.payload.inputs.pull_request = '43; touch /tmp/x';
    await assert.rejects(discover, /positive/);
});

test('linked phase and comment edits find PRs beyond the first list page', async t => {
    const f = fixture(t, {body: full});
    f.context.eventName = 'issue_comment';
    f.context.payload = {issue: {number: 102, body: phaseBody('design')}};
    f.pulls = Array.from({length: 100}, (_, i) => ({number: 1000 + i, body: light}));
    f.pulls.push(f.pr);
    assert.deepEqual(await policy.discover(f), [42]);
    f.context.payload = {issue: {number: 102, body: 'marker removed'}, changes: {body: {from: phaseBody('design')}}};
    assert.deepEqual(await policy.discover(f), [42]);
});

test('ordinary discussion and PR comments do not cause repository-wide scanning', async t => {
    const f = fixture(t);
    f.context.eventName = 'issue_comment';
    for (const issue of [{number: 1, body: 'ordinary issue'}, {number: 42, body: full, pull_request: {}}]) {
        f.context.payload = {issue};
        assert.deepEqual(await policy.discover(f), []);
    }
    assert.equal(f.calls.length, 0);
});

test('classification comment creation, editing and deletion rerun only the affected PR', async t => {
    const f = fixture(t);
    f.context.eventName = 'issue_comment';
    const body = 'IssueSpec-Classification: lightweight ' + f.pr.head.sha;
    for (const action of ['created', 'edited', 'deleted']) {
        f.context.payload = {action, issue: {number: 42, pull_request: {}}, comment: {body}};
        assert.deepEqual(await policy.discover(f), [42], action);
    }
    f.context.payload.comment.body = 'Classification removed';
    f.context.payload.action = 'edited';
    f.context.payload.changes = {body: {from: body}};
    assert.deepEqual(await policy.discover(f), [42]);
    assert.equal(f.calls.length, 0);
});

test('repository rollout retains required checks and tests the checker in unprivileged CI', () => {
    const workflow = readFileSync(new URL('.github/workflows/issue-spec-policy.yml', root), 'utf8');
    assert.match(workflow, /pull_request_target:/);
    assert.match(workflow, /types: \[created, edited, deleted\]/);
    assert.match(workflow, /ref: \$\{\{ github\.event\.repository\.default_branch \}\}/);
    assert.doesNotMatch(workflow, /pull_request\.head|pull_request\.base|secrets\./);
    assert.match(workflow, /persist-credentials: false/);
    assert.match(workflow, /checks: write/);
    assert.doesNotMatch(workflow, /contents: write|issues: write|pull-requests: write/);
    assert.match(workflow, /if: always\(\) && steps\.prepare\.outputs\.prepared == 'true'/);
    assert.match(readFileSync(new URL('.github/workflows/ci.yml', root), 'utf8'), /node --test \.github\/scripts\/issue-spec-policy-test\.mjs/);
    assert.doesNotMatch(readFileSync(new URL('.asf.yaml', root), 'utf8'), /- issue-spec-policy/);
});
