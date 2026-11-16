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
import {createRequire} from 'node:module';
import {test} from 'node:test';

const require = createRequire(import.meta.url);
const {run: runCommands} = require('./pr-comment-commands.cjs');
const requiredArtifacts = ['shenyu-images', 'shenyu-local-maven-repository'];
const validArtifacts = requiredArtifacts.map(name => ({name, expired: false}));

async function retest({path = '.github/workflows/e2e-k8s.yml', artifacts = validArtifacts, artifactError = false} = {}) {
    const calls = [];
    const failures = [];
    const repo = {owner: 'apache', repo: 'shenyu'};
    const pr = {number: 42, state: 'open', head: {sha: 'head', ref: 'fix', repo: {id: 7}}};
    const run = {
        id: 123, workflow_id: 1, run_number: 1, run_attempt: 1,
        event: 'pull_request', head_sha: 'head', head_branch: 'fix', head_repository: {id: 7},
        pull_requests: [{number: 42}], status: 'completed', conclusion: 'failure', path,
        html_url: 'https://github.com/apache/shenyu/actions/runs/123',
    };
    const github = {
        paginate: async (method, args) => method(args),
        rest: {
            pulls: {get: async () => ({data: pr})},
            reactions: {createForIssueComment: async () => ({})},
            actions: {
                listWorkflowRunsForRepo: async () => [run],
                getWorkflowRun: async () => ({data: run}),
                listWorkflowRunArtifacts: async args => {
                    calls.push({method: 'artifacts', args});
                    if (artifactError) {
                        throw new Error('artifact API unavailable');
                    }
                    return artifacts;
                },
                reRunWorkflow: async args => calls.push({method: 'all', args}),
                reRunWorkflowFailedJobs: async args => calls.push({method: 'failed', args}),
            },
        },
    };
    await runCommands({
        github,
        context: {repo, payload: {action: 'created', issue: {number: 42, pull_request: {}}, comment: {id: 9, body: '/retest'}}},
        core: {info() {}, warning() {}, error() {}, setFailed: message => failures.push(message)},
        allowedCommands: ['retest'],
    });
    return {calls, failures};
}

test('E2E preserves successful jobs while both artifacts are valid', async () => {
    const result = await retest();
    assert.deepEqual(result.calls.map(call => call.method), ['artifacts', 'failed']);
    assert.deepEqual(JSON.parse(JSON.stringify(result.calls[1].args)), {owner: 'apache', repo: 'shenyu', run_id: 123});
    assert.deepEqual(result.failures, []);
});

for (const name of requiredArtifacts) {
    test(`E2E rebuilds the workflow when ${name} has expired`, async () => {
        const result = await retest({artifacts: validArtifacts.map(artifact => ({...artifact, expired: artifact.name === name}))});
        assert.deepEqual(result.calls.map(call => call.method), ['artifacts', 'all']);
        assert.deepEqual(result.failures, []);
    });

    test(`E2E rebuilds the workflow when ${name} is missing`, async () => {
        const result = await retest({artifacts: validArtifacts.filter(artifact => artifact.name !== name)});
        assert.deepEqual(result.calls.map(call => call.method), ['artifacts', 'all']);
        assert.deepEqual(result.failures, []);
    });
}

test('other workflows retain failed-job reruns without inspecting artifacts', async () => {
    const result = await retest({path: '.github/workflows/ci.yml'});
    assert.deepEqual(result.calls.map(call => call.method), ['failed']);
    assert.deepEqual(result.failures, []);
});

test('an artifact API failure is reported without requesting a rerun', async () => {
    const result = await retest({artifactError: true});
    assert.deepEqual(result.calls.map(call => call.method), ['artifacts']);
    assert.equal(result.failures.length, 1);
    assert.match(result.failures[0], /123/);
});
