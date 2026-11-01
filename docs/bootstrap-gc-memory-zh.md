<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Bootstrap GC 与内存诊断

[English](bootstrap-gc-memory.md) · [容器部署指南](container-runtime.md)

本指南用于在调整 Bootstrap 堆大小、收集器或插件配置前收集证据，不表示已经发现生产环境 GC 缺陷，也不修改运行时默认值。每份结果都应记录应用版本、配置和测量时间范围。相关任务：[#8160](https://github.com/apache/shenyu/issues/8160)、[性能跟踪 #7650](https://github.com/apache/shenyu/issues/7650)。

## 确认实际运行环境

针对**网关进程**记录以下信息，将 Admin、上游和压测工具的数据分开：

| 方面 | 应记录的信息 |
|---|---|
| JVM | JDK 版本与发行商、实际收集器、生效参数、堆上限、直接内存配置、线程数 |
| 部署 | 镜像摘要或归档版本、系统与架构、CPU 配额和限流时间、内存限制、进程 RSS、cgroup 内存用量 |
| 流量 | 请求速率、并发数、上游数量、请求与响应大小、流式请求持续时间和并发数、超时、重试、取消 |
| 网关 | 已启用插件、选择器与规则数量、匹配缓存配置、日志队列容量与积压、连接池配置、配置发布时间 |

[容器入口脚本](../shenyu-dist/shenyu-bootstrap-dist/docker/entrypoint.sh)默认使用 `-Xms2g -Xmx2g -Xmn1g`；[归档包启动脚本](../shenyu-dist/shenyu-bootstrap-dist/src/main/resources/bin/start.sh)默认使用 `-Xms4g -Xmx4g -Xmn1g`。两者还设置 `-Xss512k`、`-XX:+DisableExplicitGC` 和 `-XX:LargePageSizeInBytes=128m`，均未默认启用 GC 日志、NMT 或显式直接内存上限。

两个脚本中的 `BOOT_JVM` 都会**替换整组默认 JVM 参数**，不是追加。其他启动方式可能不同，应检查实际进程。

Docker 的 `docker stats --no-stream <container>` 反映容器用量，应另外采集容器内的进程 RSS。Linux cgroup v2 可通过 `/proc/<pid>/cgroup` 定位所属 cgroup，采集其 `memory.current`、`memory.max`、`memory.events`、`cpu.max` 和 `cpu.stat`；v1 文件名不同。容器用量还可能包含文件缓存和其他进程，不等于 JVM 堆或 RSS。对比延迟异常窗口内 `cpu.stat` 的 `nr_throttled`、`throttled_usec` 增量，有助于定位 CPU 配额压力。

## 采集最小诊断材料

使用目标 JDK 的工具，在相同主机、相同 PID 命名空间内，以相同有效用户和用户组执行。明确指定网关 PID，不向所有 JVM 广播诊断命令。当前容器入口脚本让 Java 成为 PID 1；归档包部署需要查明实际 Java PID。

```sh
pid=1 # 容器示例；归档包部署请替换为实际 PID
jcmd "$pid" VM.version
jcmd "$pid" VM.command_line
jcmd "$pid" VM.flags
jcmd "$pid" GC.heap_info
jcmd "$pid" JFR.start name=shenyu-gc settings=profile duration=120s filename=/tmp/shenyu-gc.jfr
```

保存带时间信息的命令输出。`VM.flags` 和 `GC.heap_info` 用于确认生效参数与收集器，不能只凭镜像的 JDK 标签推断收集器。命令行可能包含凭据，分享前应移除。初步观测不需要为了取数强制执行 Full GC。

JFR 在 120 秒后停止，将文件写到**目标进程文件系统**中的 `/tmp/shenyu-gc.jfr`，不是调用者工作目录。完成后执行 `jfr summary /tmp/shenyu-gc.jfr` 检查，并在容器重启前导出文件。`profile` 录制有开销，应在对比实验中记录其启用情况，分享前检查录制内容是否包含敏感数据。

[当前发行镜像](container-runtime.md#java-runtime)包含完整 JDK。派生或精简镜像缺少 `jcmd`、`jfr` 时，应使用获准的匹配 JDK 工具环境，并确保可以访问目标 PID 命名空间、具备 attach 权限，或在诊断镜像中复现。缺少工具或禁止 attach 不代表 GC 正常。Alpine 镜像不通过归档包的 Bash 启动脚本启动，应使用其 POSIX 入口脚本。

### GC 日志和可写空间

将以下选项追加到部署中**已确认的完整 JVM 参数组**：

```text
-Xlog:gc*,safepoint:file=/tmp/shenyu-gc-%p.log:time,uptime,level,tags:filecount=5,filesize=20M
```

如果已经显式设置覆盖值，可在 shell 中使用：

```sh
: "${BOOT_JVM:?set the complete existing JVM option string first}"
export BOOT_JVM="$BOOT_JVM -Xlog:gc*,safepoint:file=/tmp/shenyu-gc-%p.log:time,uptime,level,tags:filecount=5,filesize=20M"
```

如果原先未设置 `BOOT_JVM`，先复制实际启动脚本的整组默认参数；只设置日志参数会丢弃原有堆配置。将完整环境变量应用到网关部署后重启。YAML 中可使用包含全部选项的折叠标量，不应假设 Kubernetes 会按 shell 语法展开 `${BOOT_JVM}`。

即使根文件系统只读，目标 UID 也必须能写入日志目录。该轮转配置最多保留五个归档文件和一个活动文件，约 120 MiB，另需为 JFR 与临时文件预留空间。容器指南中较小的 `/tmp` 冒烟示例不适合作为诊断空间预算；tmpfs 本身也占用容器内存。限制录制时长和存储占用，在 Pod 被替换前导出材料。

### 原生内存

NMT 需要在启动时设置 `-XX:NativeMemoryTracking=summary`，不能在运行后才开启。受控重启后采集：

```sh
jcmd "$pid" VM.native_memory summary
jcmd "$pid" VM.native_memory baseline
# 经过可比的稳定负载区间后再次执行：
jcmd "$pid" VM.native_memory summary.diff
```

区分已提交内存和预留地址空间。NMT 跟踪 HotSpot 记账的原生分配，不涵盖全部第三方原生内存，也不等于 RSS。应同时观察 RSS、cgroup、线程数，以及部署中可用的 Netty 分配器 used/active 直接内存指标。JVM buffer-pool 指标未必覆盖所有分配路径；池容量保持高位本身不证明泄漏。NMT 有额外开销，对比双方应使用相同配置。

## 区分指标来源并关联时间

[Bootstrap 示例配置](../shenyu-bootstrap/src/main/resources/application.yml)中 `shenyu.metrics.enabled` 为 `false`。`props.jvm_enabled: true` 只有在指标服务实际启动后才注册 JVM 收集器。确保指标插件与 starter 可用后，可使用仅监听回环地址的示例：

```yaml
shenyu:
  metrics:
    enabled: true
    name: prometheus
    host: 127.0.0.1
    port: 8090
    props:
      jvm_enabled: true
```

确认启动日志出现 `Prometheus metrics HTTP server ... start success`，然后在**网关所在网络命名空间内**访问 `http://127.0.0.1:8090/metrics`，例如执行 `wget -q -O - http://127.0.0.1:8090/metrics`。需要监听其他地址时，应通过部署的监控访问控制开放。

这是指标插件的独立 Prometheus 监听端口，不是 `/actuator/prometheus`。Actuator/Micrometer 有各自的启用条件、配置和指标名，采集时需保留来源与标签。

插件的 HotSpot 收集器可导出 `jvm_memory_used_bytes`、`jvm_buffer_pool_used_bytes`、`jvm_threads_current`、`jvm_gc_collection_seconds_count` 和 `jvm_gc_collection_seconds_sum`。最后两项是收集次数和累计收集耗时，不是暂停分位数或全部 GC CPU 开销。暂停分布应从 GC 日志或 JFR 分析，CPU 成本应观察 GC 线程。JFR 抽样分配权重适合定位分配热点，不是精确 bytes/request；做精确计数实验时，应说明线程覆盖范围和测量边界。

将指标与吞吐量、端到端 p95/p99、错误率、活跃流数、日志队列积压与丢弃、配置发布时间对齐。日志队列用量不包含已出队待发送批次，队列低水位也不证明后端发送成功。相关性用于缩小排查范围，不直接证明哪个对象在保留内存。[诊断与监控文档跟踪任务](https://github.com/apache/shenyu-website/issues/1174)单独负责入口说明；网关可选诊断包不会自动生成 JFR 或堆转储，Admin 也不是完整指标存储系统。

## 根据症状决定下一步

| 可比窗口中的现象 | 下一步排查 |
|---|---|
| Young GC 频繁，但可比的 GC 后堆用量稳定 | 分配带宽、暂停和 CPU 影响；频繁回收不等于泄漏 |
| 可比的 GC 后或老年代占用基线持续升高 | 保留对象、缓存、日志积压、长生命周期请求；Young GC 后堆用量不是精确 live set |
| 堆稳定，RSS 或直接内存上升 | Netty buffer 所有权、连接生命周期、原生分配和池容量；区分活跃分配与预留容量 |
| 配置发布时 GC 或分配突增 | 快照大小、JSON 转换、缓存重建；对比静默期和发布期 |
| 延迟突增但没有相应 GC 暂停 | 事件循环阻塞、CPU 限流、上游延迟和排队；暂停时间也不能覆盖所有 GC CPU 争用 |

### 一次只验证一个 JVM 改动

先确认 G1 是否实际启用。对于 G1，`-Xmn` 固定年轻代大小会约束自适应暂停控制。保持收集器、堆大小和其他变量一致，用同一代表性负载比较保留与移除该选项后的结果，不因删除参数就承诺延迟改善。识别问题后，切换收集器仍需单独实验。

容器预算应涵盖堆、直接/原生缓冲区、Metaspace、Code Cache、线程栈、GC 数据结构和运行余量。`-Xmx` 不是进程总内存限制，按百分比设置堆也不会自动给其他部分留够空间。不存在适用于所有插件和流式负载的堆百分比或直接内存上限，应在峰值报文、并发、重试和慢消费者负载下验证。直接内存上限过小可能在堆尚有余量时触发分配失败。

`DisableExplicitGC` 阻止应用通过 `System.gc()` 触发回收。修改前应确认实际分配与清理机制：关闭显式回收可能影响依赖它的清理过程，允许显式回收又可能引入暂停。它不能替代池化 buffer 的正确释放，也不能修复仍被引用的对象。

## 可复现实验矩阵

复用[真实 Divide HTTP 网关基准](benchmarks/divide-gateway/README.md)及已有输出格式。当前脚本为 PowerShell，其受控 HTTP 链不包含完整 Bootstrap/Admin 同步、认证或可选日志与 AI 插件。根据场景扩展相应 fixture，不能把 HTTP 基准结果当成下表所有场景的覆盖。

| 场景 | 变化维度与记录项 |
|---|---|
| 普通 HTTP 转发 | 上游数、请求与响应大小、请求速率、并发数 |
| 报文回放与 CURRENT/FAILOVER 重试 | 重试次数、回放体积、超时、取消、最终状态和完整响应体正确性 |
| AI 流式响应 | chunks/s、事件大小、持续时间、并发流、慢消费者、取消 |
| 日志后端变慢 | collector/mode、队列容量、积压、丢弃、发送耗时、后端恢复后的排空 |
| 配置突发更新 | 选择器/规则/快照大小、发布频率、同步完成时间、匹配缓存开关 |

对每组实验执行以下流程：

1. 固定 JDK、收集器、CPU/内存限制、上游行为、报文和诊断配置，记录版本与生效参数。网关、上游、压测器使用独立进程，分开核算分配，确认上游或压测器不是瓶颈。
2. 预热到吞吐、JIT 与占用趋势稳定，说明预热和采样时间。交替执行两个版本并重复多次，报告波动范围，不能只选择最好的一次。
3. 记录网关分配率和每个完成请求/每个 chunk 的字节数及计数方法、GC 后占用趋势、暂停分布、可获得的 GC CPU、吞吐、错误率、端到端 p95/p99。包含完整响应体完成时间与压测器调度延迟，分母中明确处理被取消和失败的请求。
4. 增加稳定负载长测，覆盖多个 GC、连接、重试和配置更新周期；比较峰值与负载排空后的状态。短时成功不能排除缓慢保留。在实验前约定接受条件和 JVM 参数回滚标准。

## 历史证据及边界

[#8160 中的初步本地观测](https://github.com/apache/shenyu/issues/8160)使用 JDK 21、macOS arm64、G1、256 MiB 堆、1 KiB 响应及约 2,000 requests/s 的生产 HTTP 插件链。记录了 36,000 个无错误请求、每请求约 25–27 KB 分配、七次约 3.0–4.2 ms 的 G1 evacuation 暂停、无 Full GC，以及这些回收后立即观测到的约 30.6–32.5 MiB 堆占用。网关、上游、压测器为独立 JVM，并启用了 JFR 与 NMT。这些是 issue 作者记录的历史观测，不是本次文档改动新跑出的结果。

该短测没有启动完整 Spring Boot Bootstrap/Admin，不包含认证、AI、日志、重试或数据同步。Young GC 后堆占用不等于精确 live set。这些数字与已有 2.6.0/JDK 8 历史基准都不能作为当前生产容量、收集器对比或无泄漏证明。[已确认的取消/保留问题 #8157](https://github.com/apache/shenyu/issues/8157)属于独立实现问题。

## 命令验证及参考资料

本指南的命令语法、120 秒 JFR 输出、GC 日志轮转选项以及 NMT summary/baseline/diff 已在 macOS x86_64 的 OpenJDK 17.0.20 上，使用独立分配负载 JVM 执行验证。这验证的是诊断命令，不代表 Bootstrap 性能、容器 attach 权限或完整部署验证；部署特有的权限和路径应提前确认。

- [JDK 17 诊断命令](https://docs.oracle.com/en/java/javase/17/docs/specs/man/jcmd.html)
- [JDK 17 G1 调优](https://docs.oracle.com/en/java/javase/17/gctuning/garbage-first-garbage-collector-tuning.html)
- [JDK 17 Native Memory Tracking](https://docs.oracle.com/en/java/javase/17/vm/native-memory-tracking.html)
- [JDK 17 Java 参数与统一日志](https://docs.oracle.com/en/java/javase/17/docs/specs/man/java.html)
