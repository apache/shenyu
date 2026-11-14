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
import { existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { createServer } from 'node:http';
import { createRequire } from 'node:module';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import vm from 'node:vm';
import test from 'node:test';
import { execFile, execFileSync, spawnSync } from 'node:child_process';
import { promisify } from 'node:util';

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


test('the plugin contract consumer resolves packaged artifacts in a separate Maven invocation', () => {
  const jobs = workflows.get('ci.yml').jobs;
  const job = jobs.plugin_contract;
  assert.ok(job, 'The external consumer must run in CI');
  assert.deepEqual(Array.from(job.needs), ['changes']);
  assert.ok(job.if.includes("needs.changes.outputs.code == 'true'"));
  const commands = job.steps.filter(step => step.run).map(step => step.run);
  const producer = commands.findIndex(command => command.includes('-pl shenyu-web -am install'));
  const consumer = commands.findIndex(command => command.includes('-f shenyu-examples/shenyu-examples-plugin-contract/pom.xml'));
  assert.ok(producer >= 0 && consumer > producer, 'Install the producer before testing the separate consumer');
  assert.ok(commands[producer].includes('-DskipTests'));
  assert.ok(!commands[producer].includes('-Dmaven.test.skip'));
  assert.ok(commands[consumer].includes('test org.apache.rat:apache-rat-plugin:0.15:check'));
  assert.ok(commands[consumer].includes('-DfailIfNoTests=true'));
  assert.ok(!commands[consumer].includes('-am'));
  assert.ok(!/-D(?:skipTests|maven\.test\.skip|checkstyle\.skip|rat\.skip)(?:[=\s]|$)/.test(commands[consumer]));
  assert.ok(jobs.requirement.needs.includes('plugin_contract'));
  assert.ok(jobs.requirement.steps.some(step => step.run?.includes('needs.plugin_contract.result')));
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
  const temporary = mkdtempSync(resolve(root, '.github/scripts/.matrix-test-'));
  let output;
  try {
    const outputFile = resolve(temporary, 'output');
    execFileSync('bash', ['-c', step.run], {
      cwd: root, env: { ...process.env, E2E_MATRIX: JSON.stringify(original), GITHUB_OUTPUT: outputFile },
    });
    output = readFileSync(outputFile, 'utf8').trim();
  } finally {
    rmSync(temporary, { recursive: true, force: true });
  }
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

function runWebSocketCompose(failure = '', channel = 'websocket', failLogs = false) {
  const temporary = mkdtempSync(resolve(root, '.github/scripts/.websocket-test-'));
  const trace = resolve(temporary, 'trace');
  const environment = resolve(temporary, 'environment.sh');
  try {
    writeFileSync(trace, '');
    writeFileSync(environment, `
record() {
  printf '%s\\n' "$1" >> "$RUN_TRACE"
  if [[ "$1" == "$FAIL_STAGE" ]]; then return 23; fi
}
docker() {
  if [[ "$1" == network ]]; then record network; return; fi
  if [[ "$1" == ps ]]; then record ps; return; fi
  local stage=sync
  if [[ "$3" == *shenyu-examples-websocket-compose.yml ]]; then stage=example; fi
  record "$stage:$4"
  if [[ "$4" == logs && "$FAIL_LOGS" == true ]]; then return 24; fi
}
bash() {
  case "$1" in
    */storage_init_mysql.sh) record storage ;;
    .github/scripts/maven-with-retry.sh) record maven ;;
    *) return 99 ;;
  esac
}
sh() {
  case "$2" in
    http://localhost:31095/actuator/health) record admin-health ;;
    http://localhost:31195/actuator/health) record gateway-health ;;
    http://localhost:31191/actuator/health) record example-health ;;
    *) return 99 ;;
  esac
}
sleep() { :; }
`);
    const result = spawnSync('bash', [
      'shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-websocket/compose/script/e2e-websocket-sync-compose.sh',
    ], { cwd: root, encoding: 'utf8', timeout: 5000, env: {
      ...process.env, BASH_ENV: environment, RUN_TRACE: trace, FAIL_STAGE: failure, SHENYU_E2E_SYNC: channel, FAIL_LOGS: String(failLogs),
    } });
    assert.ifError(result.error);
    return { ...result, calls: readFileSync(trace, 'utf8').trim().split('\n') };
  } finally {
    rmSync(temporary, { recursive: true, force: true });
  }
}

const webSocketStartup = ['storage', 'network', 'sync:up', 'admin-health', 'gateway-health', 'example:up', 'example-health'];
for (const [index, stage] of webSocketStartup.entries()) {
  test(`WebSocket Compose stops before Maven when ${stage} fails`, () => {
    const result = runWebSocketCompose(stage);
    assert.equal(result.status, 23, result.stderr);
    assert.deepEqual(result.calls, webSocketStartup.slice(0, index + 1));
  });
}

for (const channel of ['', 'websocket', 'http', 'zookeeper']) {
  test(`WebSocket Compose retains successful setup, tests and cleanup for ${channel || 'all channels'}`, () => {
    const result = runWebSocketCompose('', channel);
    assert.equal(result.status, 0, result.stderr);
    const iteration = [...webSocketStartup.slice(2), 'ps', 'maven', 'sync:down', 'example:down'];
    const iterations = channel ? iteration : [iteration, iteration, iteration].flat();
    assert.deepEqual(result.calls, [...webSocketStartup.slice(0, 2), ...iterations]);
  });
}

for (const failLogs of [false, true]) {
  test(`WebSocket Compose retains Maven failure and all diagnostics when logs ${failLogs ? 'fail' : 'succeed'}`, () => {
    const result = runWebSocketCompose('maven', '', failLogs);
    assert.equal(result.status, 1, result.stderr);
    assert.deepEqual(result.calls, [...webSocketStartup, 'ps', 'maven', 'sync:logs', 'sync:logs', 'example:logs']);
    assert.ok(result.stdout.includes('websocket-sync-e2e-test failed'));
  });
}


test('cluster health polling retains its previous startup grace and returns as soon as ready', () => {
  const command = 'curl() { echo "$STATUS"; }; sleep() { :; }; source shenyu-e2e/shenyu-e2e-case/k8s/script/healthcheck.sh http://fixture 45';
  const ready = execFileSync('bash', ['-c', command], { cwd: root, env: { ...process.env, STATUS: '200' }, encoding: 'utf8' });
  assert.ok(ready.includes('Success'));
  assert.ok(!ready.includes('2 curl'));
  assert.throws(() => execFileSync('bash', ['-c', command], { cwd: root, env: { ...process.env, STATUS: '503' }, stdio: 'pipe' }), error => {
    assert.ok(error.stdout.toString().includes('45 curl'));
    assert.ok(error.stdout.toString().includes('Failed'));
    return error.status === 1;
  });
});


for (const name of ['minimal-http.yml', 'wasm-fixtures.yml', 'docker-image-smoke.yml']) {
  test(`${name}: optional workflows also exclude documentation inside source directories`, () => {
    const patterns = workflows.get(name).on.pull_request.paths;
    const positive = patterns.filter(pattern => !pattern.startsWith('!'));
    const negative = patterns.filter(pattern => pattern.startsWith('!'));
    const filter = new Filter(yaml.dump({ paths: [`{${positive.join(',')}}`, ...negative] }), { predicateQuantifier: 'every' });
    for (const filename of ['shenyu-admin/README.md', 'shenyu-plugin/shenyu-plugin-wasm-api/README.md', 'docs/container-runtime.md', 'script/performance/minimal-http/README.md']) {
      assert.equal(filter.match([{ filename, status: 'modified' }]).paths.length, 0, filename);
    }
    assert.equal(filter.match([{ filename: `.github/workflows/${name}`, status: 'modified' }]).paths.length, 1);
  });
}

test('SQL scope excludes Admin and database documentation, keeping mixed code changes', () => {
  const run = workflows.get('native-sql-matrix.yml').jobs.scope.steps.find(step => step.id === 'scope').run;
  const code = run.split("python3 - <<'PY'\n")[1].replace(/\nPY\s*$/, '');
  const bootstrap = 'import os, subprocess, sys; subprocess.run = lambda *a, **k: None; subprocess.check_output = lambda *a, **k: os.environ["CHANGED_PATHS"]; exec(sys.argv[1])';
  const temporary = mkdtempSync(resolve(root, '.github/scripts/.matrix-test-'));
  try {
    const fixtures = [
      ['shenyu-admin/README.md', false], ['db/init/docs/guide.md', false],
      ['db/init/mysql/schema.sql', true], ['shenyu-admin/src/main/java/Plugin.java', true],
      ['shenyu-admin/README.md\ndb/upgrade/mysql/schema.sql', true],
    ];
    for (const [index, [paths, expected]] of fixtures.entries()) {
      const output = resolve(temporary, `output-${index}`);
      execFileSync('python3', ['-c', bootstrap, code], { cwd: root, env: {
        ...process.env, EVENT_NAME: 'pull_request', BASE_SHA: '1'.repeat(40), GITHUB_OUTPUT: output, CHANGED_PATHS: paths,
      } });
      assert.equal(readFileSync(output, 'utf8').trim(), `required=${expected}`, paths);
    }
  } finally {
    rmSync(temporary, { recursive: true, force: true });
  }
});


test('aggregate jobs still report failures but do not keep cancelled PR runs alive', () => {
  for (const [workflow, job] of [['ci.yml', 'requirement'], ['e2e-k8s.yml', 'requirement'], ['native-sql-matrix.yml', 'sql-matrix']]) {
    const condition = workflows.get(workflow).jobs[job].if;
    assert.ok(condition.includes('!cancelled()'), workflow);
    assert.ok(!condition.includes('always()'), workflow);
  }
});

test('CI validates the Maven wrapper before every independent Maven job', () => {
  for (const [name, job] of Object.entries(workflows.get('ci.yml').jobs)) {
    const steps = job.steps ?? [];
    const firstMaven = steps.findIndex(step => /\.\/mvnw\b/.test(step.run ?? ''));
    if (firstMaven < 0) continue;
    const preflight = steps.findIndex(step => step.run === 'bash .github/scripts/ensure-maven-wrapper.sh');
    assert.ok(preflight >= 0 && preflight < firstMaven, name);
  }
  const sourceBuild = workflows.get('ci.yml').jobs.source_release_check.steps.find(step => step.run?.includes('source_archive=')).run;
  const extractedRoot = sourceBuild.indexOf('cd "${source_root}/apache-shenyu-${version}-src"');
  const extractedPreflight = sourceBuild.indexOf('bash "${GITHUB_WORKSPACE}/.github/scripts/ensure-maven-wrapper.sh"');
  const extractedMaven = sourceBuild.indexOf('./mvnw');
  assert.ok(extractedRoot >= 0 && extractedPreflight > extractedRoot && extractedMaven > extractedPreflight,
    'Validate the extracted source distribution wrapper before using it');
});

test('integration builds validate the Maven wrapper before their first Maven invocation', () => {
  for (const name of ['integrated-test-k8s-ingress.yml', 'integrated-test.yml']) {
    const steps = workflows.get(name).jobs.build.steps;
    const preflight = steps.findIndex(step => step.run === 'bash .github/scripts/ensure-maven-wrapper.sh');
    const firstMaven = steps.findIndex(step => /\.\/mvnw\b/.test(step.run ?? ''));
    assert.ok(preflight >= 0 && firstMaven > preflight, name);
    assert.equal(steps[preflight].shell, 'bash');
  }
});

test('Maven wrapper preflight handles cached, downloaded and failed artifacts', { timeout: 45000 }, async t => {
  const preflight = readFileSync(resolve(root, '.github/scripts/ensure-maven-wrapper.sh'), 'utf8');
  const temporary = mkdtempSync(resolve(root, '.github/scripts/.wrapper-test-'));
  t.after(() => rmSync(temporary, { recursive: true, force: true }));

  // Only the archive/launcher-entry contract is under test; no Maven download is needed.
  const classes = resolve(temporary, 'classes');
  const launcher = 'org/apache/maven/wrapper/MavenWrapperMain.class';
  mkdirSync(dirname(resolve(classes, launcher)), { recursive: true });
  writeFileSync(resolve(classes, launcher), 'launcher entry fixture');
  const validJar = resolve(temporary, 'valid.jar');
  const otherJar = resolve(temporary, 'other.jar');
  execFileSync('jar', ['cf', validJar, '-C', classes, '.']);
  writeFileSync(resolve(classes, 'Other.class'), 'unrelated entry fixture');
  execFileSync('jar', ['cf', otherJar, '-C', classes, 'Other.class']);
  const valid = readFileSync(validJar);
  const corrupt = Buffer.from('incomplete cached download');
  const fixtures = [
    { name: 'valid cache avoids downloading', initial: valid, requests: 0, succeeds: true },
    { name: 'missing wrapper is downloaded', requests: 1, succeeds: true },
    { name: 'corrupt cache is replaced', initial: corrupt, requests: 1, succeeds: true },
    { name: 'two transient 503s are retried', failures: 2, requests: 3, succeeds: true },
    { name: 'invalid download preserves the old file', initial: corrupt, payload: Buffer.from('not a jar'), requests: 1 },
    { name: 'archive without launcher is rejected', payload: readFileSync(otherJar), requests: 1 },
    { name: 'persistent 503s stop after bounded retries', failures: 6, initial: corrupt, requests: 6 },
    { name: 'missing wrapper URL fails without a request', missingUrl: true, requests: 0 },
  ];
  const run = promisify(execFile);
  for (const [index, fixture] of fixtures.entries()) {
    await t.test(fixture.name, async subtest => {
      const working = resolve(temporary, `case-${index}`);
      const wrapper = resolve(working, '.mvn/wrapper');
      const target = resolve(wrapper, 'maven-wrapper.jar');
      mkdirSync(wrapper, { recursive: true });
      if (fixture.initial) writeFileSync(target, fixture.initial);
      let requests = 0;
      const server = createServer((_request, response) => {
        requests += 1;
        response.writeHead(requests <= (fixture.failures ?? 0) ? 503 : 200);
        response.end(requests <= (fixture.failures ?? 0) ? 'unavailable' : fixture.payload ?? valid);
      });
      await new Promise((resolve, reject) => {
        server.once('error', reject);
        server.listen(0, '127.0.0.1', resolve);
      });
      subtest.after(() => new Promise((resolve, reject) => server.close(error => error ? reject(error) : resolve())));
      writeFileSync(resolve(wrapper, 'maven-wrapper.properties'), fixture.missingUrl
        ? 'distributionUrl=unused\n'
        : `wrapperUrl=http://127.0.0.1:${server.address().port}/wrapper.jar\n`);
      const execution = run('bash', ['--noprofile', '--norc', '-e', '-o', 'pipefail', '-c', preflight], {
        cwd: working,
        env: { ...process.env, NO_PROXY: '127.0.0.1', no_proxy: '127.0.0.1' },
        timeout: 20000,
      });
      if (fixture.succeeds) {
        await execution;
        assert.deepEqual(readFileSync(target), valid);
      } else {
        await assert.rejects(execution, error => Number.isInteger(error.code) && error.code !== 0);
        if (fixture.initial) assert.deepEqual(readFileSync(target), fixture.initial);
        else assert.equal(existsSync(target), false);
      }
      assert.equal(requests, fixture.requests);
      assert.ok(readdirSync(wrapper).every(name => !name.startsWith('maven-wrapper.jar.')), 'Temporary downloads must be removed');
    });
  }
});
