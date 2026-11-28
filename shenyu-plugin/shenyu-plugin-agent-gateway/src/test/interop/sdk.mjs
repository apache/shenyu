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

import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import { readFile, writeFile } from 'node:fs/promises';
import { createServer } from 'node:http';
import { once } from 'node:events';
import { createInterface } from 'node:readline';
import { Client, StreamableHTTPClientTransport } from '@modelcontextprotocol/client';
import { createMcpHandler, McpServer } from '@modelcontextprotocol/server';

const version = '2026-07-28';
const modes = { json: 'application/json', sse: 'text/event-stream' };
const effects = { json: 0, sse: 0, legacy: 0 };
const wire = { json: [], sse: [], legacy: [] };
const ingress = [];
const sessions = new Set();
const fixtureToken = process.env.SHENYU_INTEROP_SERVICE_TOKEN;

function toolResult(binding) {
    effects[binding]++;
    return { content: [{ type: 'text', text: String(effects[binding]) }] };
}

function nativeHandler(mode) {
    return createMcpHandler(() => {
        const server = new McpServer({ name: 'interop', version: '1' });
        server.registerTool('count', { description: 'Increment the fixture counter' }, () => toolResult(mode));
        return server;
    }, { legacy: 'reject', responseMode: mode, keepAliveMs: 0 });
}

const handlers = { json: nativeHandler('json'), sse: nativeHandler('sse') };

function legacyResponse(req, body) {
    const session = req.headers['mcp-session-id'];
    assert.equal(req.headers['mcp-protocol-version'], '2025-06-18');
    if (req.method === 'DELETE') {
        assert.ok(sessions.delete(session));
        return new Response(null, { status: 204 });
    }
    assert.equal(req.method, 'POST');
    const respond = (result, headers) => Response.json({ jsonrpc: '2.0', id: body.id, result }, { headers });
    if (body.method === 'initialize') {
        assert.equal(session, undefined);
        const id = randomUUID();
        sessions.add(id);
        return respond({ protocolVersion: '2025-06-18', capabilities: { tools: {} }, serverInfo: { name: 'legacy', version: '1' } },
            { 'Mcp-Session-Id': id });
    }
    assert.ok(sessions.has(session));
    switch (body.method) {
        case 'notifications/initialized': return new Response(null, { status: 202 });
        case 'tools/list': return respond({ tools: [{ name: 'count', description: 'Increment the fixture counter', inputSchema: { type: 'object' } }] });
        case 'tools/call': return respond(toolResult('legacy'));
        default: throw new Error('Unexpected legacy request');
    }
}

async function serve(req, res) {
    const binding = req.url.slice(1);
    assert.ok(Object.hasOwn(wire, binding));
    const chunks = [];
    for await (const chunk of req) chunks.push(chunk);
    const raw = Buffer.concat(chunks);
    const message = raw.length ? JSON.parse(raw) : {};
    const method = message.method ?? req.method;
    wire[binding].push(method);
    assert.ok(req.headers.authorization === 'Bearer ' + fixtureToken, 'service credential mismatch');
    let response;
    if (binding === 'legacy') {
        response = legacyResponse(req, message);
    } else {
        assert.equal(req.method, 'POST');
        assert.equal(req.headers['mcp-session-id'], undefined);
        assert.equal(req.headers['last-event-id'], undefined);
        assert.ok(['server/discover', 'tools/list', 'tools/call'].includes(method));
        assert.equal(req.headers['mcp-protocol-version'], version);
        assert.equal(req.headers['mcp-method'], method);
        assert.equal(req.headers['mcp-name'], method === 'tools/call' ? 'count' : undefined);
        assert.equal(message.params._meta['io.modelcontextprotocol/protocolVersion'], version);
        assert.deepEqual(message.params._meta['io.modelcontextprotocol/clientCapabilities'], {});
        const request = new Request('http://127.0.0.1' + req.url, { method: req.method, headers: req.headers, body: raw });
        response = await handlers[binding].fetch(request);
        assert.ok(response.ok);
        assert.equal(response.headers.get('content-type').split(';')[0], modes[binding]);
    }
    res.writeHead(response.status, Object.fromEntries(response.headers));
    res.end(Buffer.from(await response.arrayBuffer()));
}

function retarget(url, request, body) {
    return new Request(url, { method: request.method, headers: request.headers, body, signal: request.signal, redirect: 'error' });
}

// Route one SDK client's calls between independent gateway instances, with no shared session.
async function exercise(ports) {
    const url = i => 'http://127.0.0.1:' + ports[i] + '/mcp';
    let gateway = 0;
    const transport = new StreamableHTTPClientTransport(new URL(url(0)), {
        fetch: async (input, init) => {
            const request = new Request(input, init);
            const body = await request.arrayBuffer();
            const message = JSON.parse(new TextDecoder().decode(body));
            const response = await fetch(retarget(url(gateway), request, body));
            assert.ok(response.ok);
            assert.equal(response.headers.get('content-type').split(';')[0], gateway === 1 ? modes.sse : modes.json);
            ingress.push({ gateway: gateway + 1, method: message.method, tool: message.params.name });
            return response;
        }
    });
    const client = new Client({ name: 'shenyu-interop', version: '1' }, { versionNegotiation: { mode: { pin: version } } });
    try {
        await client.connect(transport);
        const names = (await client.listTools()).tools.map(tool => tool.name).sort();
        assert.deepEqual(names, ['json.count', 'legacy.count', 'sse.count']);
        // Two intentional calls of json.count reach the same upstream through different gateways.
        const calls = [['json.count', 0, '1'], ['json.count', 1, '2'], ['sse.count', 1, '1'], ['legacy.count', 0, '1']];
        for (const [name, target, expected] of calls) {
            gateway = target;
            const result = await client.callTool({ name, arguments: {} });
            assert.deepEqual(result.content, [{ type: 'text', text: expected }]);
        }
    } finally {
        await client.close();
    }
    // A second implementation issues a self-describing first request to the second gateway.
    const response = await fetch(url(1), {
        method: 'POST',
        headers: { 'Content-Type': 'application/json', Accept: 'application/json, text/event-stream',
            'MCP-Protocol-Version': version, 'Mcp-Method': 'tools/list' },
        body: JSON.stringify({ jsonrpc: '2.0', id: 'independent', method: 'tools/list', params: { _meta: {
            'io.modelcontextprotocol/protocolVersion': version, 'io.modelcontextprotocol/clientCapabilities': {}
        } } })
    });
    assert.equal(response.status, 200);
    assert.equal(response.headers.get('content-type').split(';')[0], modes.sse);
    const frame = /^event: message\ndata: ([^\n]+)\n\n$/.exec(await response.text());
    assert.ok(frame);
    const reply = JSON.parse(frame[1]);
    assert.equal(reply.id, 'independent');
    assert.deepEqual(reply.result.tools.map(tool => tool.name).sort(), ['json.count', 'legacy.count', 'sse.count']);
}

async function report() {
    assert.deepEqual(effects, { json: 2, sse: 1, legacy: 1 });
    assert.equal(sessions.size, 0);
    for (const mode of Object.keys(modes)) {
        const expected = ['server/discover', 'tools/list', 'server/discover', 'tools/list', 'tools/call'];
        if (mode === 'json') expected.push('tools/call');
        assert.deepEqual(wire[mode], expected);
    }
    assert.deepEqual(wire.legacy, ['initialize', 'notifications/initialized', 'tools/list',
        'initialize', 'notifications/initialized', 'tools/list', 'tools/call', 'DELETE', 'DELETE']);
    const lock = await readFile(new URL('package-lock.json', import.meta.url));
    const packages = JSON.parse(lock).packages;
    const versions = {};
    for (const name of ['client', 'server', 'core']) {
        const key = 'node_modules/@modelcontextprotocol/' + name;
        const installed = JSON.parse(await readFile(new URL(key + '/package.json', import.meta.url), 'utf8'));
        assert.equal(installed.version, '2.4.0');
        assert.equal(packages[key].version, installed.version);
        versions[name] = installed.version;
    }
    const revision = execFileSync('git', ['rev-parse', 'HEAD'], { encoding: 'utf8' }).trim();
    const dirty = execFileSync('git', ['--no-optional-locks', 'status', '--porcelain'], { encoding: 'utf8' }).length > 0;
    const evidence = { schemaVersion: 1, issue: 8196, status: 'passed', recordedAt: new Date().toISOString(),
        protocol: version, shenyuRevision: revision, workingTreeDirty: dirty, sdkSource: '951fe5603a7ec0b2dcb6a0b95d92cd86f93ad9e5', sdk: versions, node: process.version,
        lockfileSha256: createHash('sha256').update(lock).digest('hex'), ingress, upstreamMethods: wire,
        contentTypes: modes, sideEffects: effects, independentClient: 'raw tools/list over SSE', gatewayInstances: 2 };
    await writeFile(process.argv[2], JSON.stringify(evidence, null, 2) + '\n');
}

const lines = createInterface({ input: process.stdin });
const commands = lines[Symbol.asyncIterator]();
const server = createServer((req, res) => serve(req, res).catch(error => {
    console.error(error.message);
    res.writeHead(500).end();
}));
try {
    server.listen(0, '127.0.0.1');
    await once(server, 'listening');
    console.log(server.address().port);
    const ports = JSON.parse((await commands.next()).value);
    await exercise(ports);
    console.log('calls-complete');
    assert.equal((await commands.next()).value, 'catalogs-closed');
    await report();
} finally {
    lines.close();
    await Promise.all(Object.values(handlers).map(handler => handler.close()));
    server.closeAllConnections();
    server.close();
}
