# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Snapshot compiled fixture classes, then run an external fixed-arrival k6 comparison."""

import argparse
import ctypes
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
import threading
import time
import urllib.request
import xml.etree.ElementTree as ET
import zipfile

ROOT = Path(__file__).resolve().parents[3]
MODULE = ROOT / "shenyu-spring-boot-starter/shenyu-spring-boot-starter-plugin/shenyu-spring-boot-starter-plugin-httpclient"
PLUGIN = ROOT / "shenyu-plugin/shenyu-plugin-httpclient"
MAIN = "org.apache.shenyu.springboot.starter.plugin.httpclient.HttpClientPoolBenchmark"
FLAGS = ["-Xms128m", "-Xmx256m", "-XX:MaxDirectMemorySize=64m", "-XX:ActiveProcessorCount=2"]
CASES = [
    ("direct-slow-control", "FIXED", True, False, "/slow", 160, True),
    ("fixed-fast-close", "FIXED", False, False, "/fast", 100, False),
    ("fixed-fast-reuse", "FIXED", True, False, "/fast", 100, False),
    ("fixed-steady", "FIXED", True, False, "/slow", 40, False),
    ("fixed-overload", "FIXED", True, False, "/slow", 160, False),
    ("elastic-overload", "ELASTIC", True, False, "/slow", 160, False),
    ("http-stream-overload", "FIXED", True, False, "/stream", 16, False),
    ("ai-client-stream-control", "FIXED", True, True, "/stream", 16, False),
]


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def extended(path):
    return Path("\\\\?\\" + str(path.resolve())) if os.name == "nt" else path


def snapshot(destination):
    destination.mkdir(parents=True, exist_ok=True)
    report = next(extended(MODULE / "target/surefire-reports").glob("TEST-*.xml"))
    properties = {node.get("name"): node.get("value") for node in ET.parse(report).getroot().findall("./properties/property")}
    roots = [MODULE / "target/test-classes", MODULE / "target/classes", PLUGIN / "target/classes"]
    with zipfile.ZipFile(destination / "fixture.jar", "w", zipfile.ZIP_DEFLATED) as archive:
        for root in roots:
            for file in extended(root).rglob("*"):
                if file.is_file():
                    archive.write(file, file.relative_to(extended(root)).as_posix())
    excluded = {str(root.resolve()).casefold() for root in roots}
    classpath = [str(destination.resolve() / "fixture.jar")]
    classpath += [entry for entry in properties["java.class.path"].split(os.pathsep) if str(Path(entry).resolve()).casefold() not in excluded]
    (destination / "classpath.txt").write_text(os.pathsep.join(classpath), encoding="utf-8")
    write_json(destination / "build.json", {
        "commit": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        "trackedProductionDiffSha256": hashlib.sha256(subprocess.check_output(["git", "diff", "--", "**/src/main/**"], cwd=ROOT)).hexdigest(),
        "fixtureSha256": hashlib.sha256((destination / "fixture.jar").read_bytes()).hexdigest(),
        "java": properties.get("java.runtime.version"), "os": platform.platform(),
        "hostLogicalCpus": os.cpu_count(), "jvmFlags": FLAGS,
    })
    print("Snapshot ready:", destination, flush=True)


def get(url):
    with urllib.request.urlopen(url, timeout=10) as response:
        return response.read().decode()


def run_k6(k6, url, rate, duration, prefix, raw):
    command = [k6, "run", "--quiet", "--summary-export", str(prefix) + ".summary.json",
               "-e", "URL=" + url, "-e", "RATE=" + str(rate), "-e", "DURATION=" + str(duration) + "s"]
    if raw:
        command += ["--out", "json=" + str(prefix) + ".points.json"]
    command += [str(Path(__file__).with_name("load.js"))]
    with open(str(prefix) + ".log", "w", encoding="utf-8") as output:
        subprocess.run(command, stdout=output, stderr=subprocess.STDOUT, check=True, timeout=duration + 40)
    return json.loads(Path(str(prefix) + ".summary.json").read_text(encoding="utf-8"))


def handles(pid, stop, maximum):
    if os.name != "nt":
        return
    kernel = ctypes.windll.kernel32
    kernel.OpenProcess.restype = ctypes.c_void_p
    kernel.GetProcessHandleCount.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_ulong)]
    kernel.CloseHandle.argtypes = [ctypes.c_void_p]
    process = kernel.OpenProcess(0x0400, False, pid)
    if not process:
        return
    try:
        while not stop.wait(0.05):
            count = ctypes.c_ulong()
            if kernel.GetProcessHandleCount(process, ctypes.byref(count)):
                maximum[0] = max(maximum[0], count.value)
    finally:
        kernel.CloseHandle(process)


def run(args):
    destination = args.output.resolve()
    destination.mkdir(parents=True, exist_ok=True)
    # Invalidate an earlier success before any executable/build validation can fail.
    write_json(destination / "results.json", {"runs": []})
    build = json.loads((args.build / "build.json").read_text(encoding="utf-8"))
    if hashlib.sha256((args.build / "fixture.jar").read_bytes()).hexdigest() != build["fixtureSha256"]:
        raise RuntimeError("Compiled fixture snapshot does not match its recorded SHA-256")
    runtime = subprocess.run([args.java, "-version"], text=True, capture_output=True, check=True)
    results = {"build": build, "javaVersion": (runtime.stdout + runtime.stderr).strip(),
               "javaExecutable": shutil.which(args.java) or args.java,
               "k6Version": subprocess.check_output([args.k6, "version"], text=True).strip(),
               "warmupSeconds": args.warmup, "measurementSeconds": args.duration, "repetitions": args.repeat, "runs": []}
    for repeat in range(args.repeat):
        for name, pool_type, keep_alive, ai, path, rate, direct in CASES:
            prefix = destination / (str(repeat + 1) + "-" + name)
            flags = FLAGS + ["-Dbench.type=" + pool_type, "-Dbench.keepAlive=" + str(keep_alive).lower(),
                             "-Dbench.aiControl=" + str(ai).lower()]
            if args.pending is not None:
                flags += ["-Dbench.pending=" + str(args.pending)]
            if name == "fixed-overload" and repeat == 0:
                flags += ["-XX:StartFlightRecording=filename=" + str(prefix) + ".jfr,settings=profile,dumponexit=true"]
            classpath = (args.build / "classpath.txt").read_text(encoding="utf-8")
            argfile = Path(str(prefix) + ".args")
            argfile.write_text("\n".join(json.dumps(value) for value in [*flags, "-cp", classpath, MAIN]), encoding="utf-8")
            fixture_log = open(str(prefix) + ".fixture.log", "w", encoding="utf-8")
            process = subprocess.Popen([args.java, "@" + str(argfile)], stdout=fixture_log, stderr=subprocess.STDOUT)
            try:
                ready = None
                for _ in range(200):
                    text = Path(str(prefix) + ".fixture.log").read_text(encoding="utf-8", errors="replace")
                    match = re.search(r"POOL_BENCH_READY (\{[^\n]+\})", text)
                    if match:
                        ready = json.loads(match.group(1))
                        break
                    if process.poll() is not None:
                        raise RuntimeError("Fixture failed: " + text[-2000:])
                    time.sleep(0.1)
                if ready is None:
                    raise RuntimeError("Fixture did not become ready")
                url = ready["upstream" if direct else "proxy"] + path
                run_k6(args.k6, url, rate, args.warmup, Path(str(prefix) + ".warmup"), False)
                get(ready["proxy"] + "/reset")
                stop, maximum = threading.Event(), [-1]
                sampler = threading.Thread(target=handles, args=(ready["pid"], stop, maximum), daemon=True)
                sampler.start()
                try:
                    summary = run_k6(args.k6, url, rate, args.duration, prefix, True)
                    pool = json.loads(get(ready["proxy"] + "/stats"))
                finally:
                    stop.set()
                    sampler.join(timeout=2)
                pool["peakWindowsProcessHandles"] = maximum[0]
                latencies = []
                for line in Path(str(prefix) + ".points.json").read_text(encoding="utf-8").splitlines():
                    point = json.loads(line)
                    if point.get("type") == "Point" and point.get("metric") == "successful_duration":
                        latencies.append(point["data"]["value"])
                result = {"case": name, "repeat": repeat + 1, "rate": rate, "poolType": pool_type,
                          "keepAlive": keep_alive, "aiClientControl": ai, "directUpstream": direct,
                          "pendingAcquireMaxCount": args.pending, "ready": ready, "summary": summary,
                          "resources": pool, "successfulLatencyMillis": latencies}
                results["runs"].append(result)
                write_json(destination / "results.json", results)
                successes = summary["metrics"].get("successful_requests", {}).get("rate", 0)
                print(str(repeat + 1) + "/" + name + ": " + str(round(successes, 2)) + " successful/s", flush=True)
            finally:
                if ready:
                    try:
                        get(ready["proxy"] + "/stop")
                    except (OSError, TimeoutError):
                        pass
                else:
                    process.terminate()
                try:
                    process.wait(timeout=8)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=5)
                fixture_log.close()
    print("Results:", destination / "results.json", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="mode", required=True)
    snap = sub.add_parser("snapshot")
    snap.add_argument("destination", type=Path)
    execute = sub.add_parser("run")
    execute.add_argument("--build", type=Path, required=True)
    execute.add_argument("--output", type=Path, required=True)
    execute.add_argument("--k6", default="k6")
    execute.add_argument("--java", default="java")
    execute.add_argument("--pending", type=int)
    execute.add_argument("--warmup", type=int, default=5)
    execute.add_argument("--duration", type=int, default=8)
    execute.add_argument("--repeat", type=int, default=3)
    args = parser.parse_args()
    if args.mode == "snapshot":
        snapshot(args.destination)
    else:
        run(args)


if __name__ == "__main__":
    main()
