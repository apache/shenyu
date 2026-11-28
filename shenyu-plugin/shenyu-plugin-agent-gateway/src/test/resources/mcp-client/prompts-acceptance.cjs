/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

const assert = require('node:assert/strict');
const { createRequire } = require('node:module');
const { readFileSync } = require('node:fs');
const { resolve } = require('node:path');

const [root, endpoint, stage] = process.argv.slice(2);
const externalRequire = createRequire(resolve(root, 'package.json'));
const { Client, StreamableHTTPClientTransport } = externalRequire('@modelcontextprotocol/client');
const version = JSON.parse(readFileSync(resolve(root, 'node_modules/@modelcontextprotocol/client/package.json'), 'utf8')).version;
assert.equal(version, '2.4.0', 'Acceptance requires the documented, unmodified SDK version');
const exchanges = [];

function connection(credential) {
    const transport = new StreamableHTTPClientTransport(new URL(endpoint), {
        requestInit: { headers: { Authorization: `Bearer ${credential}`, Cookie: 'fixture-private-cookie' } },
        fetch: async (input, init) => {
            const request = new Request(input, init);
            const body = request.method === 'POST' ? await request.clone().json() : undefined;
            const response = await fetch(request);
            if (body) {
                assert.equal(body.params?._meta?.['io.modelcontextprotocol/protocolVersion'], '2026-07-28');
                assert.ok(body.params._meta['io.modelcontextprotocol/clientCapabilities']);
                assert.match(request.headers.get('accept'), /application\/json/);
                assert.match(request.headers.get('accept'), /text\/event-stream/);
            }
            exchanges.push({ method: body?.method ?? request.method, status: response.status });
            return response;
        }
    });
    const client = new Client({ name: 'shenyu-prompt-acceptance', version: '1' }, {
        versionNegotiation: { mode: { pin: '2026-07-28' } }
    });
    return { client, transport };
}

async function main() {
    const { client, transport } = connection('owner');
    try {
        await client.connect(transport);
        assert.equal(client.getProtocolEra(), 'modern');
        const listed = await client.listPrompts();
        const expected = stage === 'full' ? ['first.next', 'first.summary', 'local', 'second.summary'] : ['local'];
        assert.deepEqual(listed.prompts.map(prompt => prompt.name).sort(), expected);
        for (const name of expected) {
            const result = await client.getPrompt({ name, arguments: { topic: 'gateway' } });
            assert.equal(result.messages[0].role, 'user');
            assert.equal(result.messages[0].content.type, 'text');
            assert.equal(result.messages[0].content.text, `${name}:gateway`);
        }
        if (stage === 'restricted') {
            await assert.rejects(client.getPrompt({ name: 'first.summary', arguments: { topic: 'gateway' } }));
            assert.ok(exchanges.some(exchange => exchange.method === 'prompts/get' && exchange.status === 403));
        }
        await assert.rejects(client.getPrompt({ name: 'local', arguments: {} }));
        assert.ok(exchanges.some(exchange => exchange.method === 'prompts/get' && exchange.status === 400));
        assert.deepEqual((await client.listTools()).tools, []);
        assert.ok(exchanges.some(exchange => exchange.method === 'server/discover' && exchange.status === 200));
        assert.ok(exchanges.every(exchange => !['initialize', 'notifications/initialized'].includes(exchange.method)));
        console.log(JSON.stringify({ sdk: version, protocolEra: client.getProtocolEra(), stage, prompts: expected, exchanges }));
    } finally {
        await client.close();
    }
    const denied = connection('untrusted');
    try {
        await assert.rejects(denied.client.connect(denied.transport));
        assert.ok(exchanges.some(exchange => exchange.method === 'server/discover' && exchange.status === 401));
    } finally {
        await denied.client.close();
    }
}

main().catch(error => {
    console.error(error);
    process.exitCode = 1;
});
