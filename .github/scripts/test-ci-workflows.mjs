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
import { readFileSync, readdirSync } from 'node:fs';
import { createRequire } from 'node:module';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import vm from 'node:vm';
import test from 'node:test';
import { execFileSync } from 'node:child_process';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const bundle = resolve(root, 'actions/paths-filter/dist/index.js');
// Load the checked-in action's real matcher and YAML parser without executing its API entry point.
const source = readFileSync(bundle, 'utf8').replace(
  'var __webpack_exports__ = __nccwpck_require__(3109);',
  'var __webpack_exports__ = { Filter: __nccwpck_require__(3707).Filter, yaml: __nccwpck_require__(1917) };',
);
const context = { module: { exports: {} }, require: createRequire(bundle), __dirname: dirname(bundle), process, Buffer, console };
vm.runInNewContext(source, context, { filename: bundle });
const { Filter, yaml } = context.module.exports;
const workflows = new Map(readdirSync(resolve(root, '.github/workflows')).filter(name => name.endsWith('.yml'))
  .map(name => [name, yaml.load(readFileSync(resolve(root, '.github/workflows', name), 'utf8'))]));

for (const [name, workflow] of workflows) {
  for (const [jobName, job] of Object.entries(workflow.jobs)) {
    for (const step of job.steps ?? []) {
      if (step.uses !== './actions/paths-filter') continue;
      test(`${name}/${jobName}: documentation is excluded and production/CI changes are included`, () => {
        const filter = new Filter(step.with.filters, { predicateQuantifier: step.with['predicate-quantifier'] });
        const matches = filename => Object.values(filter.match([{ filename, status: 'modified' }])).some(files => files.length);
        for (const path of ['README.md', 'README_CN.md', 'docs/guide.md', 'shenyu-common/README.md', 'shenyu-common/docs/guide.txt']) {
          assert.equal(matches(path), false, path);
        }
        for (const path of ['mvnw', '.mvn/wrapper/maven-wrapper.properties', 'pom.xml', 'shenyu-common/src/main/java/Example.java', `.github/workflows/${name}`, 'actions/setup-java-with-retry/action.yml']) {
          assert.equal(matches(path), true, path);
        }
        assert.equal(matches('shenyu-common/src/test/resources/fixture.txt'), true);
      });
    }
  }
  if (workflow.concurrency) {
    test(`${name}: only repeated PRs share a concurrency group`, () => {
      assert.equal(workflow.concurrency['cancel-in-progress'], "${{ github.event_name == 'pull_request' }}");
      assert.ok(workflow.concurrency.group.includes('github.event.pull_request.number || github.run_id'));
    });
  }
}

test('Java 17 and Java 21 shards cover identical module sets without duplicates', () => {
  const jobs = workflows.get('ci.yml').jobs;
  const moduleSets = [];
  for (const id of ['test_group', 'compat_java21']) {
    const modules = jobs[id].strategy.matrix.include.flatMap(group => execFileSync('bash', ['.github/scripts/expand-ci-module-roots.sh'], {
      cwd: root, env: { ...process.env, GROUP_ROOTS: group.modules }, encoding: 'utf8',
    }).trim().split(','));
    assert.equal(modules.length, new Set(modules).size, 'Modules must belong to exactly one shard');
    moduleSets.push(new Set(modules));
  }
  assert.deepEqual(moduleSets[0], moduleSets[1]);
  const all = execFileSync('git', ['ls-files', 'shenyu-*/**/src/test/*'], { cwd: root, encoding: 'utf8' }).split('\n');
  for (const file of all) {
    const marker = file.indexOf('/src/test/');
    if (marker < 0 || /^(shenyu-examples|shenyu-e2e|shenyu-integrated-test|shenyu-bootstrap-http)\//.test(file)) continue;
    assert.ok(moduleSets[0].has(file.slice(0, marker)), `Uncovered test module: ${file}`);
  }
});

test('the stable build gate includes every independent correctness check', () => {
  const gate = workflows.get('ci.yml').jobs.requirement;
  for (const name of ['agent_gateway_migrations', 'registry_dependencies', 'rat_check', 'check-license-header', 'test_group', 'compat_java21']) {
    assert.ok(gate.needs.includes(name), name);
  }
});


test('a slow starter subtree runs once in its own shard', () => {
  const groups = workflows.get('ci.yml').jobs.test_group.strategy.matrix.include;
  const agent = groups.find(group => group.name === 'starters-agent');
  const rest = groups.find(group => group.name === 'starters');
  const modules = execFileSync('bash', ['.github/scripts/expand-ci-module-roots.sh'], {
    cwd: root, env: { ...process.env, GROUP_ROOTS: rest.modules }, encoding: 'utf8',
  }).trim().split(',');
  assert.ok(!modules.includes(agent.modules));
  assert.ok(modules.includes('shenyu-spring-boot-starter'));
  assert.throws(() => execFileSync('bash', ['.github/scripts/expand-ci-module-roots.sh'], {
    cwd: root, env: { ...process.env, GROUP_ROOTS: 'shenyu-common,!does-not-exist' }, stdio: 'pipe',
  }));
});


test('E2E protocol shards retain every sync channel and leave other scenarios intact', () => {
  const step = workflows.get('e2e-k8s.yml').jobs.changes.steps.find(step => step.id === 'shards');
  const original = { include: [
    { script: 'e2e-grpc-sync-compose', case: 'grpc', example_projects: ':grpc' },
    { script: 'e2e-apache-dubbo-sync-compose', case: 'dubbo', example_projects: ':dubbo' },
    { script: 'e2e-logging-rocketmq-compose', case: 'logging', example_projects: ':http' },
    { script: 'e2e-cluster-jdbc-compose', case: 'cluster', example_projects: ':http' },
  ] };
  const output = execFileSync('bash', ['-c', step.run], {
    cwd: root, env: { ...process.env, E2E_MATRIX: JSON.stringify(original), GITHUB_OUTPUT: '/dev/stdout' }, encoding: 'utf8',
  }).trim();
  const shards = JSON.parse(output.slice('matrix='.length)).include;
  assert.equal(shards.length, 10);
  for (const entry of original.include) {
    const selected = shards.filter(shard => shard.script === entry.script);
    assert.deepEqual(selected.map(shard => shard.sync), entry.case === 'cluster' ? [''] : ['websocket', 'http', 'zookeeper']);
    for (const shard of selected) {
      assert.equal(shard.case, entry.case);
      assert.equal(shard.example_projects, entry.example_projects);
    }
  }
});

test('manual E2E runs keep all sync channels and invalid selections fail', () => {
  const command = 'source .github/scripts/select-e2e-sync.sh; printf "%s\\n" "${SYNC_ARRAY[@]}"';
  for (const channel of ['', 'websocket', 'http', 'zookeeper']) {
    const actual = execFileSync('bash', ['-c', command], {
      cwd: root, env: { ...process.env, SHENYU_E2E_SYNC: channel }, encoding: 'utf8',
    }).trim().split('\n');
    assert.deepEqual(actual, channel ? [channel] : ['websocket', 'http', 'zookeeper']);
  }
  assert.throws(() => execFileSync('bash', ['-c', command], {
    cwd: root, env: { ...process.env, SHENYU_E2E_SYNC: 'unsupported' }, stdio: 'pipe',
  }));
});
