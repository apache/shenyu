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
import {spawnSync} from 'node:child_process';
import {mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import {dirname, join, resolve} from 'node:path';
import {test} from 'node:test';
import {fileURLToPath} from 'node:url';
import {runInNewContext} from 'node:vm';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const repo = {owner: 'apache', repo: 'shenyu'};
const scripts = Object.fromEntries(['assign', 'close', 'lgtm', 'cc', 'hold'].map(command => {
    const path = join(root, '.github/workflows', `${command}.yml`);
    const workflow = readFileSync(path, 'utf8');
    // Each workflow ends with one inline github-script block; execute that exact code.
    const [header, script] = workflow.split('          script: |\n');
    assert.ok(script, `Missing inline script in ${path}`);
    // Preserve workflow line numbers for stack traces and V8 coverage reports.
    const preamble = header.trimEnd().split('\n').map(line => `// ${line}`).join('\n');
    return [command, {path, script: `${preamble}\n(async () => {\n${script}\n})()`}];
}));

function commandBody(command) {
    return command === 'cc' ? '/cc @reviewer' : `/${command}`;
}

function fixture(command, options = {}) {
    const calls = [];
    const failures = [];
    const context = {
        repo,
        payload: {
            action: options.action ?? 'created',
            issue: {number: 42, ...(options.issue ? {} : {pull_request: {}})},
            comment: {id: 123, body: options.body ?? commandBody(command), user: {login: options.user ?? 'contributor'}},
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
                state: options.state ?? 'open',
                user: {login: options.author ?? 'author'},
                head: {sha: options.headSha ?? '0123456789abcdef0123456789abcdef01234567'},
            }),
            update: endpoint('close', {}),
            createReview: endpoint('lgtm', {state: 'APPROVED'}),
        },
        issues: {
            createComment: endpoint('cc', {}),
            addLabels: endpoint('hold', []),
            addAssignees: endpoint('assign', args => ({
                assignees: (options.assigned ?? args.assignees).map(login => ({login})),
            })),
        },
        repos: {getCollaboratorPermissionLevel: endpoint('permission', {permission: options.permission ?? 'read'})},
        reactions: {createForIssueComment: endpoint('reaction', {})},
    }};
    const core = {info: () => {}, setFailed: message => failures.push(message)};
    const {script, path} = scripts[command];
    return {
        calls,
        failures,
        context,
        run: () => runInNewContext(script, {context, github, core}, {filename: path}),
    };
}

for (const command of Object.keys(scripts)) {
    test(`${command}: ignores edits, ordinary issues, and non-command comments`, async () => {
        for (const options of [
            {action: 'edited'}, {action: 'deleted'}, {issue: true},
            {body: ''}, {body: `Please /${command}`}, {body: `> /${command}`},
            {body: `\`/${command}\``}, {body: `/${command}ment`},
            {body: `/${command}\n/lgtm`}, {body: `/${command}\nSome explanation`},
        ]) {
            const f = fixture(command, options);
            await f.run();
            assert.deepEqual(f.calls, [], JSON.stringify(options));
        }
        for (const key of ['comment', 'issue']) {
            const f = fixture(command);
            delete f.context.payload[key];
            await f.run();
            assert.deepEqual(f.calls, []);
        }
    });

    test(`${command}: accepts surrounding whitespace and acknowledges only after success`, async () => {
        const f = fixture(command, {body: ` \n${commandBody(command)}\r\n `, user: 'author'});
        await f.run();
        assert.deepEqual(f.calls.map(call => call.name), ['get', command, 'reaction']);
        assert.deepEqual(f.calls[0].args, {...repo, pull_number: 42});
        assert.deepEqual(f.calls.at(-1).args, {...repo, comment_id: 123, content: 'rocket'});
        assert.deepEqual(f.failures, []);
    });

    test(`${command}: skips closed or merged pull requests`, async () => {
        const f = fixture(command, {state: 'closed'});
        await f.run();
        assert.deepEqual(f.calls.map(call => call.name), ['get']);
    });

    test(`${command}: propagates API failures without a success reaction`, async () => {
        for (const fail of ['get', command, 'reaction']) {
            const f = fixture(command, {user: 'author', fail});
            await assert.rejects(f.run(), /API unavailable/);
            assert.equal(f.calls.at(-1).name, fail);
        }
    });
}

test('assign: defaults to the commenter', async () => {
    const f = fixture('assign');
    await f.run();
    assert.deepEqual(f.calls[1].args, {...repo, issue_number: 42, assignees: ['contributor']});
});

test('assign: supports multiple names, optional mentions, and case-insensitive deduplication', async () => {
    const f = fixture('assign', {body: '/assign @Alice\tBob @alice user-3'});
    await f.run();
    assert.deepEqual(f.calls[1].args.assignees, ['alice', 'bob', 'user-3']);
});

test('assign: rejects invalid arguments and embedded commands', async () => {
    for (const body of ['/assign @', '/assign @a, @b', '/assign @a /close', '/assign\n@a', '/assign $(id)']) {
        const f = fixture('assign', {body});
        await f.run();
        assert.deepEqual(f.calls, [], body);
    }
});

test('assign: allows ten distinct users and rejects eleven', async () => {
    for (const count of [10, 11]) {
        const f = fixture('assign', {body: `/assign ${Array.from({length: count}, (_, i) => `@user-${i}`).join(' ')}`});
        await f.run();
        assert.equal(f.calls.length, count === 10 ? 3 : 1);
        assert.equal(f.failures.length, count === 10 ? 0 : 1);
    }
});

test('assign: detects silently ignored users without acknowledging success', async () => {
    const f = fixture('assign', {body: '/assign @Alice @Bob', assigned: ['ALICE']});
    await f.run();
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'assign']);
    assert.deepEqual(f.failures, ['Could not assign: bob.']);
});

test('assign: repeated assignments remain successful', async () => {
    const f = fixture('assign', {assigned: ['CONTRIBUTOR']});
    await f.run();
    await f.run();
    assert.deepEqual(f.failures, []);
    assert.equal(f.calls.filter(call => call.name === 'reaction').length, 2);
});

test('close: lets the author close without requesting collaborator permissions', async () => {
    const f = fixture('close', {user: 'AUTHOR'});
    await f.run();
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'close', 'reaction']);
    assert.deepEqual(f.calls[1].args, {...repo, pull_number: 42, state: 'closed'});
});

test('close: allows write, maintain, and admin permissions', async () => {
    for (const permission of ['write', 'maintain', 'admin']) {
        const f = fixture('close', {permission});
        await f.run();
        assert.deepEqual(f.calls.map(call => call.name), ['get', 'permission', 'close', 'reaction']);
        assert.deepEqual(f.calls[1].args, {...repo, username: 'contributor'});
    }
});

test('close: rejects read, triage, none, and unknown permissions', async () => {
    for (const permission of ['read', 'triage', 'none', 'unknown']) {
        const f = fixture('close', {permission});
        await f.run();
        assert.deepEqual(f.calls.map(call => call.name), ['get', 'permission']);
    }
});

test('close: fails closed if permission lookup fails', async () => {
    const f = fixture('close', {fail: 'permission'});
    await assert.rejects(f.run(), /API unavailable/);
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'permission']);
});

test('close, lgtm, and hold: reject arguments', async () => {
    for (const command of ['close', 'lgtm', 'hold']) {
        const f = fixture(command, {body: `/${command} please`});
        await f.run();
        assert.deepEqual(f.calls, []);
    }
});

test('lgtm: submits an approving review for the current head and records the commenter', async () => {
    const f = fixture('lgtm');
    await f.run();
    assert.deepEqual(f.calls[1].args, {
        ...repo,
        pull_number: 42,
        commit_id: '0123456789abcdef0123456789abcdef01234567',
        event: 'APPROVE',
        body: 'Approved via /lgtm by @contributor.',
    });
    assert.deepEqual(f.calls.map(call => call.name), ['get', 'lgtm', 'reaction']);
});

test('lgtm: binds a new request to the fetched head commit', async () => {
    const headSha = 'abcdef0123456789abcdef0123456789abcdef01';
    const f = fixture('lgtm', {headSha});
    await f.run();
    assert.equal(f.calls[1].args.commit_id, headSha);
});

test('cc: posts mentions on the pull request', async () => {
    const f = fixture('cc');
    await f.run();
    assert.deepEqual(f.calls[1].args, {...repo, issue_number: 42, body: 'cc: @reviewer'});
});

test('cc: supports multiple names, optional mentions, and case-insensitive deduplication', async () => {
    const f = fixture('cc', {body: '/cc @Alice\tBob @alice user-3'});
    await f.run();
    assert.equal(f.calls[1].args.body, 'cc: @alice @bob @user-3');
});

test('cc: rejects missing users, invalid arguments, and embedded commands', async () => {
    for (const body of [
        '/cc', '/cc @', '/cc @a, @b', '/cc @a /close', '/cc\n@a', '/cc $(id)',
        '/cc @org/team', '/cc @a\n/hold', 'Please /cc @a', '> /cc @a', '`/cc @a`',
    ]) {
        const f = fixture('cc', {body});
        await f.run();
        assert.deepEqual(f.calls, [], body);
    }
});

test('hold: adds the hold label without replacing other labels', async () => {
    const f = fixture('hold');
    await f.run();
    assert.deepEqual(f.calls[1].args, {...repo, issue_number: 42, labels: ['hold']});
});

test('hold: repeated requests remain successful', async () => {
    const f = fixture('hold');
    await f.run();
    await f.run();
    assert.deepEqual(f.failures, []);
    assert.equal(f.calls.filter(call => call.name === 'hold').length, 2);
    assert.equal(f.calls.filter(call => call.name === 'reaction').length, 2);
});

test('legacy manager: delegates PR commands, preserves issue commands and other PR commands', t => {
    const directory = mkdtempSync(join(root, '.pr-comment-test-'));
    t.after(() => rmSync(directory, {recursive: true, force: true}));
    const eventPath = join(directory, 'event.json');
    // Override curl in a child shell so the legacy integration test cannot call GitHub.
    const harness = 'curl() { printf "%s\\n" "$*"; }; export -f curl; bash -x actions/scripts/issue-manager.sh';
    for (const isPr of [true, false]) {
        for (const action of ['created', 'edited']) {
            writeFileSync(eventPath, JSON.stringify({
                action,
                issue: {number: 42, ...(isPr ? {pull_request: {}} : {})},
                comment: {body: '/assign @alice\n/close\n/lgtm\n/cc @bob\n/hold\n/unhold', user: {login: 'contributor'}},
            }));
            const result = spawnSync('bash', ['-c', harness], {
                cwd: root,
                encoding: 'utf8',
                env: {...process.env, GITHUB_TOKEN: 'test-token', GITHUB_EVENT_PATH: eventPath, GITHUB_REPOSITORY: 'apache/shenyu'},
            });
            assert.equal(result.status, 0, result.stderr);
            const requests = result.stderr.split('\n').filter(line => /^\++ curl /.test(line));
            assert.equal(requests.length, isPr ? 1 : 6, result.stderr);
            assert.ok(requests.at(-1).includes('-X DELETE'));
            assert.ok(requests.at(-1).includes('/labels/hold'));
            if (!isPr) {
                assert.ok(requests[0].includes('/assignees'));
                assert.ok(requests[1].includes('closed'));
                assert.ok(requests[2].includes('lgtm'));
                assert.ok(requests[3].includes('/comments'));
                assert.match(requests[3], /cc:\s+@bob/);
                assert.ok(requests[4].includes('hold'));
            }
        }
    }
});
