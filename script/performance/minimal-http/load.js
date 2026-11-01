// Licensed to the Apache Software Foundation (ASF) under one or more
// contributor license agreements. See the NOTICE file distributed with
// this work for additional information regarding copyright ownership.
// The ASF licenses this file to You under the Apache License, Version 2.0
// (the "License"); you may not use this file except in compliance with
// the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import http from 'k6/http';
import { Counter, Rate } from 'k6/metrics';

const successful = new Counter('successful_requests');
const errors = new Rate('response_errors');

export const options = {
  scenarios: {
    proxy: {
      executor: 'constant-arrival-rate',
      rate: Number(__ENV.RATE),
      timeUnit: '1s',
      duration: `${__ENV.DURATION}s`,
      preAllocatedVUs: Number(__ENV.VUS || 64),
      maxVUs: Number(__ENV.VUS || 64),
      gracefulStop: '5s',
    },
  },
  summaryTrendStats: ['avg', 'min', 'med', 'p(50)', 'p(95)', 'p(99)', 'max'],
  thresholds: {
    response_errors: ['rate<0.001'],
    dropped_iterations: ['count==0'],
    http_req_duration: [`p(99)<${__ENV.P99_MS || 100}`],
  },
};

export default function () {
  const response = http.get(__ENV.URL, { timeout: '3s' });
  const ok = response.status === 200 && response.body === __ENV.EXPECTED_BODY;
  errors.add(!ok);
  successful.add(ok ? 1 : 0);
}

export function handleSummary(data) {
  return { [__ENV.SUMMARY]: JSON.stringify(data, null, 2) };
}
