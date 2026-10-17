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

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;

/**
 * Closed-loop HTTP load driver for the bootstrap-vs-minimal benchmark (issue #7649).
 *
 * <p>Single-file source program, JDK 17+, no dependencies:
 * {@code java LoadDriver.java <url> <workers> <warmupSec> <measureSec> [marker]}
 *
 * <p>Each worker thread issues synchronous requests in a loop (keep-alive on).
 * A response counts as successful only when it is HTTP 200 AND its body contains
 * the marker (default {@code bench-backend-ok}): ShenYu answers route/upstream
 * failures with HTTP 200 plus a JSON error code, so status alone cannot tell a
 * proxied response from a gateway-level rejection. Latencies are recorded only
 * during the measure window and only for successful responses, into a
 * 50-microsecond bucket histogram; errors are counted separately. On exit one
 * CSV line is printed: {@code requests,errors,rps,p50us,p90us,p99us,maxus}
 * (percentiles are -1 when no response succeeded).
 *
 * <p>Closed loop is deliberate: it measures the maximum sustained throughput a
 * variant supports at a given concurrency, plus the latency at that saturation
 * point. It does NOT simulate a fixed external request rate.
 */
public final class LoadDriver {

    private static final long BUCKET_US = 50;

    private static final int BUCKETS = 100_000; // up to 5 s per request

    private LoadDriver() {
    }

    /**
     * Runs the load.
     *
     * @param args url, worker threads, warmup seconds, measure seconds, optional marker
     * @throws InterruptedException if interrupted while joining workers
     */
    public static void main(final String[] args) throws InterruptedException {
        final String url = args[0];
        final int workers = Integer.parseInt(args[1]);
        final String marker = args.length > 4 ? args[4] : "bench-backend-ok";
        final long warmupNanos = Long.parseLong(args[2]) * 1_000_000_000L;
        final long measureNanos = Long.parseLong(args[3]) * 1_000_000_000L;

        final HttpClient client = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(5))
                .version(HttpClient.Version.HTTP_1_1)
                .build();
        final HttpRequest request = HttpRequest.newBuilder(URI.create(url))
                .timeout(Duration.ofSeconds(15))
                .GET()
                .build();

        final AtomicLongArray histogram = new AtomicLongArray(BUCKETS);
        final AtomicLong requests = new AtomicLong();
        final AtomicLong errors = new AtomicLong();
        final long measureStart = System.nanoTime() + warmupNanos;
        final long measureEnd = measureStart + measureNanos;

        final List<Thread> threads = new ArrayList<>();
        for (int i = 0; i < workers; i++) {
            final Thread t = new Thread(() -> {
                while (System.nanoTime() < measureEnd) {
                    final long t0 = System.nanoTime();
                    final boolean measure = t0 >= measureStart;
                    try {
                        final HttpResponse<byte[]> resp = client.send(request, HttpResponse.BodyHandlers.ofByteArray());
                        final long elapsedUs = (System.nanoTime() - t0) / 1_000L;
                        if (measure) {
                            requests.incrementAndGet();
                            if (resp.statusCode() == 200 && new String(resp.body(), StandardCharsets.UTF_8).contains(marker)) {
                                histogram.incrementAndGet((int) Math.min(elapsedUs / BUCKET_US, BUCKETS - 1));
                            } else {
                                errors.incrementAndGet();
                            }
                        }
                    } catch (Exception e) {
                        if (measure) {
                            requests.incrementAndGet();
                            errors.incrementAndGet();
                        }
                    }
                }
            });
            t.setDaemon(true);
            t.start();
            threads.add(t);
        }
        for (final Thread t : threads) {
            t.join();
        }

        final long total = requests.get();
        final long succeeded = total - errors.get();
        final double rps = total * 1_000_000_000.0 / measureNanos;
        final long p50 = succeeded > 0 ? percentile(histogram, succeeded, 0.50) : -1;
        final long p90 = succeeded > 0 ? percentile(histogram, succeeded, 0.90) : -1;
        final long p99 = succeeded > 0 ? percentile(histogram, succeeded, 0.99) : -1;
        final long max = succeeded > 0 ? maxBucket(histogram) : -1;
        System.out.printf("%d,%d,%.1f,%d,%d,%d,%d%n", total, errors.get(), rps, p50, p90, p99, max);
    }

    private static long percentile(final AtomicLongArray histogram, final long total, final double quantile) {
        final long threshold = (long) Math.ceil(total * quantile);
        long cumulative = 0;
        for (int i = 0; i < BUCKETS; i++) {
            cumulative += histogram.get(i);
            if (cumulative >= threshold) {
                return i * BUCKET_US + BUCKET_US / 2;
            }
        }
        return BUCKETS * BUCKET_US;
    }

    private static long maxBucket(final AtomicLongArray histogram) {
        for (int i = BUCKETS - 1; i >= 0; i--) {
            if (histogram.get(i) > 0) {
                return i == BUCKETS - 1 ? -1 : i * BUCKET_US + BUCKET_US / 2;
            }
        }
        return 0;
    }
}
