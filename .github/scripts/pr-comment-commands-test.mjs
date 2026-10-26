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

// Run with: node --test .github/scripts/pr-comment-commands-test.mjs
import assert from 'node:assert/strict';
import {createRequire} from 'node:module';
import {spawnSync} from 'node:child_process';
import {mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {test} from 'node:test';

const require = createRequire(import.meta.url);
const root = resolve(new URL('../..', import.meta.url).pathname);
const {parseCommands, run} = require('./pr-comment-commands.cjs');
const repo = {owner: 'apache', repo: 'shenyu'};

function fixture(body, options = {}) {
    const calls = [];
    const failures = [];
    const context = {
        repo,
        payload: {
            action: options.action ?? 'created',
            issue: {number: 42, ...(options.issue ? {} : {pull_request: {}})},
            comment: {id: 123, body, user: {login: options.user ?? 'contributor'}},
        },
    };
    const endpoint = (name, result) => async args => {
        calls.push({name, args: JSON.parse(JSON.stringify(args))});
        if (options.fail === name) {
            throw new Error('API unavailable');
        }
        return {data: typeof result === 'function' ? result(args) : result};
    };
    const github = {rest: {
        pulls: {
            get: endpoint('get', {
                number: 42,
                state: options.state ?? 'open',
                user: {login: options.author ?? 'author'},
                head: {sha: options.headSha ?? '0123456789abcdef0123456789abcdef01234567',
                    ref: 'feature', repo: {id: 99}},
            }),
            update: endpoint('update', {}),
            createReview: endpoint('lgtm', {state: 'APPROVED'}),
            dismissReview: endpoint('unlgtm', {}),
            listReviews: () => {},
        },
        issues: {
            createComment: endpoint('cc', {}),
            addLabels: endpoint('hold', []),
            removeLabel: endpoint('unhold', {}),
            addAssignees: endpoint('assign', args => ({
                assignees: (options.assigned ?? args.assignees).map(login => ({login})),
            })),
            removeAssignees: endpoint('unassign', {}),
        },
        repos: {getCollaboratorPermissionLevel: endpoint('permission', {permission: options.permission ?? 'read'})},
        reactions: {createForIssueComment: endpoint('reaction', {})},
        actions: {
            listWorkflowRunsForRepo: () => {},
            getWorkflowRun: endpoint('get-run', {}),
            reRunWorkflowFailedJobs: endpoint('retest', {}),
        },
    }};
    github.paginate = async (method, args) => {
        calls.push({name: method === github.rest.pulls.listReviews ? 'list-reviews' : 'list-runs', args});
        if (options.fail === 'paginate') {
            throw new Error('API unavailable');
        }
        return options.reviews ?? [];
    };
    const core = {
        info: () => {},
        warning: () => {},
        error: () => {},
        setFailed: message => failures.push(message),
    };
    return {
        calls,
        failures,
        context,
        run: allowedCommands => run({context, github, core, allowedCommands}),
    };
}

test('parses valid commands from prose and multiple lines', () => {
    assert.deepEqual(parseCommands([
        'Please take a look.',
        '/retest',
        '',
        '/hold',
        '/cc @Alice Bob @alice',
        '```',
        '/unhold',
        '```',
    ].join('\r\n')), [
        {name: 'retest'},
        {name: 'hold'},
        {name: 'cc', users: ['alice', 'bob']},
    ]);
});

test('ignores invalid, embedded, and argument-bearing commands', async () => {
    const f = fixture([
        'Please /hold',
        '/cc',
        '/lgtm please',
        '/hold\textra',
        '/unknown',
    ].join('\n'));
    await f.run();
    assert.deepEqual(f.calls, []);
});

test('executes multiple commands in source order and acknowledges each success', async () => {
    const f = fixture('/cc @Alice Bob\n/lgtm\n/hold\n/unhold');
    await f.run();
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'cc', 'reaction', 'lgtm', 'reaction', 'hold', 'reaction',
        'unhold', 'reaction']);
    assert.equal(f.calls[1].args.body, 'cc: @alice @bob');
    assert.equal(f.failures.length, 0);
});

test('processes the retest and hold combination from one comment', async () => {
    const f = fixture('Please check the failures.\n\n/retest\n/hold');
    await f.run();
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'list-runs', 'reaction', 'hold', 'reaction']);
    assert.equal(f.failures.length, 0);
});

test('filters commands when jobs have separate permission scopes', async () => {
    const f = fixture('/retest\n/hold');
    await f.run(['hold']);
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'hold', 'reaction']);
});

test('supports the reverse commands for labels, assignments, and approvals', async () => {
    const f = fixture('/assign @Alice\n/unassign @Alice\n/lgtm\n/unlgtm', {
        reviews: [{id: 456, state: 'APPROVED', body: 'Approved via /lgtm by @contributor.'}],
    });
    await f.run();
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'assign', 'reaction', 'unassign', 'reaction', 'lgtm',
        'reaction', 'list-reviews', 'unlgtm', 'reaction']);
    assert.equal(f.calls.at(-2).args.review_id, 456);
});

test('skips commands that do not match the pull request state', async () => {
    const closed = fixture('/hold\n/reopen', {state: 'closed', user: 'author'});
    await closed.run();
    assert.deepEqual(closed.calls.map(call => call.name), ['get', 'update', 'reaction']);

    const open = fixture('/reopen\n/hold');
    await open.run();
    assert.deepEqual(open.calls.map(call => call.name), ['get', 'hold', 'reaction']);
});

test('updates state between commands in the same comment', async () => {
    const f = fixture('/close\n/reopen', {user: 'author'});
    await f.run();
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'update', 'reaction', 'update', 'reaction']);
    assert.equal(f.calls[1].args.state, 'closed');
    assert.equal(f.calls[3].args.state, 'open');
});

test('continues processing other lines after a command failure', async () => {
    const f = fixture('/hold\n/unhold', {fail: 'hold'});
    await f.run();
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'hold', 'unhold', 'reaction']);
    assert.match(f.failures[0], /\/hold: API unavailable/);
});

test('ignores edits and ordinary issues', async () => {
    for (const options of [{action: 'edited'}, {issue: true}]) {
        const f = fixture('/hold\n/lgtm', options);
        await f.run();
        assert.deepEqual(f.calls, []);
    }
});

test('legacy issue manager handles reverse labels and delegates every PR command', t => {
    const directory = mkdtempSync(join(root, '.pr-comment-test-'));
    t.after(() => rmSync(directory, {recursive: true, force: true}));
    const eventPath = join(directory, 'event.json');
    const harness = 'curl() { printf "%s\\n" "$*"; }; export -f curl; bash -x actions/scripts/issue-manager.sh';
    for (const isPr of [true, false]) {
        writeFileSync(eventPath, JSON.stringify({
            action: 'created',
            issue: {number: 42, ...(isPr ? {pull_request: {}} : {})},
            comment: {body: '/assign @alice\n/close\n/lgtm\n/cc @bob\n/hold\n/unhold\n/unlgtm', user: {login: 'contributor'}},
        }));
        const result = spawnSync('bash', ['-c', harness], {
            cwd: root,
            encoding: 'utf8',
            env: {...process.env, GITHUB_TOKEN: 'test-token', GITHUB_EVENT_PATH: eventPath, GITHUB_REPOSITORY: 'apache/shenyu'},
        });
        assert.equal(result.status, 0, result.stderr);
        const requests = result.stderr.split('\n').filter(line => /^\++ curl /.test(line));
        assert.equal(requests.length, isPr ? 0 : 7, result.stderr);
        if (!isPr) {
            assert.ok(requests.some(request => request.includes('/labels/lgtm')));
            assert.ok(requests.some(request => request.includes('/labels/hold')));
            assert.ok(requests.some(request => request.includes('-X DELETE') && request.includes('/labels/hold')));
            assert.ok(requests.some(request => request.includes('-X DELETE') && request.includes('/labels/lgtm')));
        }
    }
});
