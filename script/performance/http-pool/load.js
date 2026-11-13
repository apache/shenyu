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

import http from 'k6/http';
import { Counter, Trend } from 'k6/metrics';

const successes = new Counter('successful_requests');
const failures = new Counter('failed_requests');
const successfulDuration = new Trend('successful_duration', true);

export const options = {
  scenarios: {
    requests: {
      executor: 'constant-arrival-rate',
      rate: Number(__ENV.RATE || 160),
      timeUnit: '1s',
      duration: __ENV.DURATION || '8s',
      preAllocatedVUs: 64,
      maxVUs: 256,
      gracefulStop: '5s',
    },
  },
  summaryTrendStats: ['avg', 'min', 'max', 'p(50)', 'p(95)', 'p(99)'],
  discardResponseBodies: true,
};

export default function () {
  const response = http.get(__ENV.URL, { timeout: '4s' });
  if (response.status === 200) {
    successes.add(1);
    successfulDuration.add(response.timings.duration);
  } else {
    failures.add(1);
  }
}
