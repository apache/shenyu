#!/usr/bin/env python3
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Compare packaged HTTP/full assemblies with identical config, JVM and traffic.

Readiness includes completed WebSocket initialization and a successful proxy request.
First launch is explicitly NOT a controlled OS-cold-cache measurement.
"""

import argparse
import json
import math
import os
import platform
import re
import shutil
import socket
import statistics
import subprocess
import time
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import urlopen

import psutil

from fixture import SyncFixture, Upstream
from inventory import inventory

HERE = Path(__file__).resolve().parent


def request(url):
    try:
        with urlopen(url, timeout=2) as response:
            return response.status, response.read()
    except HTTPError as error:
        return error.code, error.read()


def wait_for(url, predicate, process, timeout=120):
    deadline, last = time.monotonic() + timeout, None
    while time.monotonic() < deadline:
        if process.poll() is not None:
            raise RuntimeError(f"Gateway exited with {process.returncode}; inspect its gateway.log")
        try:
            last = request(url)
            if predicate(*last):
                return last
        except (URLError, TimeoutError, ConnectionError, OSError):
            pass
        time.sleep(0.05)
    raise TimeoutError(f"Timed out waiting for {url}: {last}")


def free_port():
    with socket.socket() as listener:
        listener.bind(("127.0.0.1", 0))
        return listener.getsockname()[1]


def error_code(body, code):
    try:
        return json.loads(body).get("code") == code
    except (ValueError, AttributeError):
        return False


def launch(args, directory, name, fixture, config, output):
    output.mkdir(parents=True)
    port = free_port()
    main = "org.apache.shenyu.bootstrap.http.ShenyuHttpApplication" if name == "minimal" else "org.apache.shenyu.bootstrap.ShenyuBootstrapApplication"
    command = [args.java, "-Xms512m", "-Xmx1024m", "-XX:MaxDirectMemorySize=256m", "-XX:ActiveProcessorCount=2",
               f"-Dlogging.config={config.parent / 'logback.xml'}", "-cp", str(directory / "lib" / "*"), main,
               f"--spring.config.location=file:{config.as_posix()}", f"--server.port={port}", "--server.address=127.0.0.1",
               f"--shenyu.sync.websocket.urls=ws://127.0.0.1:{fixture.port}/websocket",
               "--shenyu.netty.http.worker-count=2", "--shenyu.httpclient.keep-alive=true",
               "--shenyu.heartbeat.enabled=false", "--spring.cloud.discovery.enabled=false", "--eureka.client.enabled=false",
               "--management.health.redis.enabled=false", "--management.health.elasticsearch.enabled=false",
               "--logging.level.org.apache.shenyu=WARN"]
    if getattr(fixture, "allow_origin", None):
        command.append(f"--shenyu.sync.websocket.allow-origin={fixture.allow_origin}")
    if getattr(fixture, "sync_token", None):
        command.append(f"--shenyu.sync.websocket.token={fixture.sync_token}")
    (output / "command.json").write_text(json.dumps(command, indent=2), encoding="utf-8")
    log = (output / "gateway.log").open("w", encoding="utf-8")
    started = time.monotonic()
    process = subprocess.Popen(command, cwd=directory, stdout=log, stderr=subprocess.STDOUT)
    try:
        if args.cpu_affinity:
            psutil.Process(process.pid).cpu_affinity(args.cpu_affinity)
        base = f"http://127.0.0.1:{port}"
        wait_for(base + "/actuator/health/liveness", lambda status, _: status == 200, process)
        if not fixture.snapshot_allowed.is_set():
            assert request(base + "/actuator/health/readiness")[0] == 503, "Readiness accepted an incomplete snapshot"
            fixture.snapshot_allowed.set()
        wait_for(base + "/actuator/health/readiness", lambda status, _: status == 200, process)
        wait_for(base + "/bench/ok", lambda status, body: status == 200 and body.startswith(b"upstream-a:"), process)
        return process, log, base, time.monotonic() - started
    except BaseException:
        stop(process, log)
        raise


def stop(process, log):
    process.terminate()
    try:
        process.wait(timeout=15)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait(timeout=5)
    log.close()


def smoke(base, process, fixture, upstream_a, upstream_b):
    results = ["initial snapshot and HTTP success"]
    fixture.update("RULE", fixture.data["RULE"], "DELETE")
    wait_for(base + "/bench/ok", lambda _, body: error_code(body, -102), process, 10)
    results.append("rule deletion removes route")
    fixture.reset(upstream_a.port)
    wait_for(base + "/bench/ok", lambda status, body: status == 200 and body == upstream_a.body, process, 10)
    results.append("rule creation restores route")
    fixture.update("DISCOVER_UPSTREAM", fixture.upstreams(upstream_b.port))
    wait_for(base + "/bench/ok", lambda status, body: status == 200 and body == upstream_b.body, process, 10)
    results.append("upstream update reaches a different backend")
    wait_for(base + "/bench/fail", lambda status, _: status == 503, process, 10)
    results.append("upstream HTTP failure is preserved")
    fixture.update("DISCOVER_UPSTREAM", fixture.upstreams(free_port()))
    wait_for(base + "/bench/ok", lambda status, _: status >= 500, process, 10)
    results.append("unreachable upstream fails")
    fixture.reset(upstream_a.port)
    wait_for(base + "/bench/ok", lambda status, body: status == 200 and body == upstream_a.body, process, 10)
    fixture.update("SELECTOR", fixture.data["SELECTOR"], "DELETE")
    wait_for(base + "/bench/ok", lambda _, body: error_code(body, -107), process, 10)
    results.append("selector deletion removes route")
    fixture.reset(upstream_a.port)
    wait_for(base + "/bench/ok", lambda status, body: status == 200 and body == upstream_a.body, process, 10)
    results.append("recovery after configuration updates")
    return results


def resources(process, seconds):
    target = psutil.Process(process.pid)
    before = sum(target.cpu_times()[:2])
    started = time.monotonic()
    samples = []
    while time.monotonic() - started < seconds:
        samples.append({"elapsed_s": time.monotonic() - started, "rss_bytes": target.memory_info().rss})
        time.sleep(0.2)
    elapsed = time.monotonic() - started
    return {"rss_bytes": target.memory_info().rss, "peak_sampled_rss_bytes": max(s["rss_bytes"] for s in samples),
            "cpu_percent_one_core": 100 * (sum(target.cpu_times()[:2]) - before) / elapsed, "samples": samples}


def loaded_classes(args, process, output):
    jcmd = str(Path(args.java).with_name("jcmd.exe" if os.name == "nt" else "jcmd"))
    if not Path(jcmd).is_file():
        jcmd = shutil.which("jcmd")
    if not jcmd:
        raise RuntimeError("A JDK with jcmd is required for loaded-class evidence")
    result = subprocess.run([jcmd, str(process.pid), "PerfCounter.print"], capture_output=True, text=True, check=True)
    (output / "perf-counters.txt").write_text(result.stdout, encoding="utf-8")
    def counter(name):
        return int(re.search(rf"^{re.escape(name)}=(\d+)$", result.stdout, re.MULTILINE)[1])
    return counter("java.cls.loadedClasses") - counter("java.cls.unloadedClasses"), jcmd


def load(args, url, body, rate, duration, output, gateway=None):
    output.mkdir(parents=True)
    env = dict(os.environ, URL=url, EXPECTED_BODY=body.decode(), RATE=str(rate), DURATION=str(duration),
               SUMMARY=str(output / "summary.json"), P99_MS=str(args.p99_ms), K6_NO_USAGE_REPORT="true")
    command = [args.k6, "run", "--quiet", "--out", f"json={output / 'samples.json'}", str(HERE / "load.js")]
    with (output / "k6.log").open("w", encoding="utf-8") as log:
        process = subprocess.Popen(command, stdout=log, stderr=subprocess.STDOUT, env=env)
        target = psutil.Process(gateway.pid) if gateway else None
        cpu_before = sum(target.cpu_times()[:2]) if target else 0
        started, samples = time.monotonic(), []
        try:
            while process.poll() is None:
                if target:
                    samples.append(target.memory_info().rss)
                if time.monotonic() - started > duration + 45:
                    raise TimeoutError("Load generator did not finish")
                time.sleep(0.2)
        finally:
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)
        elapsed = time.monotonic() - started
        cpu = 100 * (sum(target.cpu_times()[:2]) - cpu_before) / elapsed if target else None
    # 99 is a completed run with failed thresholds, useful when bracketing load.
    # Other exits (including external aborts) must not look like gateway saturation.
    if process.returncode not in (0, 99):
        raise RuntimeError(f"k6 exited with {process.returncode}; inspect {output / 'k6.log'}")
    summary = json.loads((output / "summary.json").read_text(encoding="utf-8"))
    metrics = summary["metrics"]
    def nonnegative(value, name):
        if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value) or value < 0:
            raise ValueError(f"Invalid k6 summary value for {name}: {value}")
        return value
    def count(name):
        value = nonnegative(metrics.get(name, {}).get("values", {}).get("count"), name)
        if value != int(value):
            raise ValueError(f"Invalid k6 summary count for {name}: {value}")
        return value
    requests, successful = count("http_reqs"), count("successful_requests")
    if requests == 0 or successful > requests:
        raise ValueError("Invalid k6 summary: expected completed requests and successes <= requests")
    measured_ms = nonnegative(summary.get("state", {}).get("testRunDurationMs"), "testRunDurationMs")
    if measured_ms == 0:
        raise ValueError("Invalid k6 summary: testRunDurationMs must be positive")
    # k6 counts requests completed during gracefulStop. Use its actual run duration,
    # not only the offered-load window, to avoid inflating throughput at high load.
    measured_seconds = measured_ms / 1000
    return {"offered_rps": rate, "offered_duration_s": duration, "measurement_duration_s": measured_seconds,
            "successful_rps": successful / measured_seconds, "requests": requests,
            "errors": requests - successful, "dropped_iterations": count("dropped_iterations") if "dropped_iterations" in metrics else 0,
            "latency_ms": metrics["http_req_duration"]["values"], "thresholds_passed": process.returncode == 0,
            "cpu_percent_one_core": cpu, "peak_sampled_rss_bytes": max(samples) if samples else None}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--minimal", required=True, type=lambda s: Path(s).resolve())
    parser.add_argument("--full", type=lambda s: Path(s).resolve())
    parser.add_argument("--output", required=True, type=lambda s: Path(s).resolve())
    parser.add_argument("--java", default=shutil.which("java"))
    parser.add_argument("--k6", default=shutil.which("k6"))
    parser.add_argument("--smoke-only", action="store_true")
    parser.add_argument("--runs", type=int, default=3)
    parser.add_argument("--rates", nargs="+", type=int, default=[200, 500, 1000])
    parser.add_argument("--duration", type=int, default=30)
    parser.add_argument("--warmup", type=int, default=30)
    parser.add_argument("--idle", type=int, default=10)
    parser.add_argument("--p99-ms", type=float, default=100)
    parser.add_argument("--cpu-affinity", nargs="+", type=int)
    parser.add_argument("--jfr", action="store_true")
    args = parser.parse_args()
    if min(args.runs, args.duration, args.warmup, args.idle, *args.rates) <= 0:
        parser.error("Run counts, durations and rates must be positive")
    if not args.smoke_only and (not args.full or not args.k6):
        parser.error("Comparison requires --full and a k6 executable")
    args.output.mkdir(parents=True, exist_ok=False)
    report = {"environment": {"platform": platform.platform(), "cpu": platform.processor(), "logical_cpus": os.cpu_count(),
                              "java": subprocess.run([args.java, "-version"], capture_output=True, text=True, check=True).stderr,
                              "git_revision": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
                              "working_tree": subprocess.check_output(["git", "status", "--short"], text=True),
                              "parameters": vars(args) | {"minimal": str(args.minimal), "full": str(args.full), "output": str(args.output)},
                              "readiness": "initial WebSocket sync plus HTTP proxy success", "cpu_unit": "100% = one logical CPU",
                              "placement": "generator, Python upstream and gateway on the same host; direct-upstream controls required",
                              "cold_cache": "unmeasured; first launch and subsequent warm filesystem launches only"}, "inventories": {}, "runs": []}
    assemblies = [("minimal", args.minimal)] + ([("full", args.full)] if args.full else [])
    for name, directory in assemblies:
        report["inventories"][name] = inventory(directory, name == "minimal")
    upstream_a, upstream_b = Upstream("upstream-a"), Upstream("upstream-b")
    fixture = SyncFixture(upstream_a.port, hold_snapshot=args.smoke_only)
    config = args.minimal / "conf" / "application.yml"
    try:
        if not args.smoke_only:
            report["environment"]["k6"] = subprocess.check_output([args.k6, "version"], text=True).strip()
            report["direct_controls"] = [load(args, f"http://127.0.0.1:{upstream_a.port}/bench/ok", upstream_a.body,
                                              rate, args.duration, args.output / f"direct-{rate}") for rate in args.rates]
        for repeat in range(1 if args.smoke_only else args.runs):
            # Alternate ordering to reduce systematic warm-filesystem and time drift bias.
            for name, directory in assemblies if repeat % 2 == 0 else reversed(assemblies):
                fixture.reset(upstream_a.port)
                output = args.output / f"{name}-{repeat + 1}"
                process, log, base, ready = launch(args, directory, name, fixture, config, output)
                try:
                    result = {"assembly": name, "repeat": repeat + 1, "readiness_s": ready,
                              "launch_condition": "first-observed" if repeat == 0 else "warm-filesystem",
                              "smoke": smoke(base, process, fixture, upstream_a, upstream_b),
                              "idle": resources(process, args.idle), "loads": []}
                    result["loaded_classes"], jcmd = loaded_classes(args, process, output)
                    if args.jfr:
                        subprocess.run([jcmd, str(process.pid), "JFR.start", "name=assembly", "settings=profile"], check=True, capture_output=True)
                    if not args.smoke_only:
                        load(args, base + "/bench/ok", upstream_a.body, args.rates[0], args.warmup, output / "warmup", process)
                        for rate in args.rates:
                            result["loads"].append(load(args, base + "/bench/ok", upstream_a.body, rate, args.duration, output / f"load-{rate}", process))
                    if args.jfr:
                        subprocess.run([jcmd, str(process.pid), "JFR.dump", "name=assembly", f'filename="{output / "gateway.jfr"}"'], check=True, capture_output=True)
                    report["runs"].append(result)
                    (args.output / "report.json").write_text(json.dumps(report, indent=2, default=str) + "\n", encoding="utf-8")
                    print(f"{name} run {repeat + 1}: ready={ready:.3f}s classes={result['loaded_classes']} smoke=passed", flush=True)
                finally:
                    stop(process, log)
        for name, _ in assemblies:
            ready = [row["readiness_s"] for row in report["runs"] if row["assembly"] == name]
            print(f"{name}: readiness median={statistics.median(ready):.3f}s range={min(ready):.3f}..{max(ready):.3f}s")
    finally:
        fixture.close()
        upstream_a.close()
        upstream_b.close()


if __name__ == "__main__":
    main()
