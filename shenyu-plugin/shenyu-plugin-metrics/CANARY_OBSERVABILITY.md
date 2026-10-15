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

# Divide 灰度分流观测

本实现复用现有 Metrics 插件及 Prometheus 输出端点。启用 Metrics 插件并配置现有 Prometheus reporter 后，新指标自动注册；无须配置额外服务。
Divide 不依赖 Metrics 模块。即使未启用 Metrics，首次选路仍会保存观测记录，原有路由、重试和错误响应行为保持不变。

## 请求内的数据流

1. 沿用 GlobalPlugin 调用的 DefaultShenyuContextBuilder 已有逻辑，在 ShenyuContext 的 `startDateTime` 中保存请求上下文创建时间；不新增全局计时属性。
2. MetricsPlugin 安装可选的选路回调，沿用 `chain.execute(...).doOnSuccess(...).doOnError(...)` 执行链，并通过 `doFinally` 结算灰度请求结果。
3. Divide 调用一次 `decide()` 后，将 `CanaryContext` 放在 `SHENYU_CANARY_CONTEXT` 中；执行候选池过滤、初始回退和负载均衡后，补充实际分区和拒绝/回退原因。
4. Divide 通知可选回调，立即记录决策耗时和成功回退。长连接不必等到结束才能看到这些指标。
5. 下游链正常完成或报错后，沿用原来的 responseCommitted：响应已提交则立即记录延迟，否则注册 `beforeCommit` 回调等待提交。
6. MetricsPlugin 包围的下游链完成、出错或取消时，记录一次外部请求结果。

`CanaryContext` 位于 plugin-api，包含 selector/rule ID、期望分区、实际分区、成功回退原因、拒绝原因和决策纳秒耗时。
埋点沿用 Sentinel、RateLimiter、Resilience4j 的 exchange Consumer 回调方式：MetricsPlugin 注册 `METRICS_CANARY`，Divide 在选路结束或拒绝时调用。没有独立的 Recorder 或 Observer；指标上报、请求计时和去重统一在 MetricsPlugin 内完成。
CanaryContext 只由 Divide 在首次选路期间填充，MetricsPlugin 读取，重试不修改它。分流决策计时紧邻 Divide 的 decide 调用。
它不保存 Header/Cookie、sticky key、用户 ID、请求 ID、路径或上游地址。
原有 `SHENYU_CANARY_PARTITION` 和 `SHENYU_CANARY_LABELS` 继续供 HTTP 重试使用；观测上下文不会改变其含义。

## 指标契约

| 名称 | 类型 | 标签 |
| --- | --- | --- |
| `shenyu_canary_requests_total` | Counter | `selector, rule, partition, outcome` |
| `shenyu_canary_fallback_total` | Counter | `selector, rule, reason` |
| `shenyu_canary_decision_duration_seconds` | Histogram | `selector, rule` |
| `shenyu_execute_latency_millis`（复用现有指标） | Histogram | `partition` |

三个新增指标的名称、标签及顺序统一定义在 `CanaryMetric`，注册和上报使用同一份定义。
`CanaryMetric` 只提供指标定义及按定义排列的标签值，不执行注册或上报。`MetricsReporter.register` 统一组织注册，具体创建指标由 `MetricsRegister` 实现；MetricsPlugin 直接通过 MetricsReporter 记录计数和耗时。
请求延迟复用 `MetricsPlugin.responseCommitted/recordTime → MetricsReporter.recordTime`，保留 `shenyu_execute_latency_millis` 的名称和毫秒单位，增加 `partition=stable/canary/none` 标签。
该延迟指标仅区分分区，不含 selector/rule，因此只能比较跨规则聚合的分区延迟。请求量和错误率仍可按规则比较。

### 分区与结果

- 已选出节点：`partition` 使用实际分区，Stable 回退后的请求归 Stable。
- 未选出节点：`partition` 使用最初期望分区。例如 Canary 和 Stable 都为空时，拒绝仍归最初期望的 Canary。
- 没有灰度观测上下文：现有请求延迟使用 `partition=none`，不产生三个新增灰度指标。
- `success`：下游链正常完成，最终 HTTP 状态为 2xx/3xx；没有显式状态时按默认成功响应处理。
- `error`：转发链异常，或正常完成但 HTTP 状态不属于 2xx/3xx。4xx 纳入错误，因此该指标不是纯服务端故障率。
- `reject`：Divide 显式标记的选路拒绝。拒绝原因优先于普通状态/错误分类，不能仅凭 HTTP 503 推断。
- `cancelled`：下游链取消，优先于其他分类；响应头已经提交也仍可取消。

每次外部请求最多记录一个 outcome。重试后的成功只计一个 success，重试耗尽只计一个 error。
这里统计的是外部请求归属，不是后端收到的实际网络请求次数；未选出节点的异常请求也可能按期望分区归属。
长连接的 requests 计数要等到终止时才出现；进程退出或崩溃时尚未结束的请求不会结算。

### 回退与拒绝原因

只有首次选路从 Canary 成功选中 Stable 节点，才增加 fallback，reason 固定为 `canary_pool_empty`。
Canary 和 Stable 都空、Stable 负载均衡未选中节点，以及同分区重试，都不会增加 fallback。
成功回退后再发生转发失败或取消，不撤销已发生的回退事件。

观测上下文中的拒绝原因包括 `canary_pool_empty`、`stable_pool_empty`、`no_upstream_selected`。
它们用于受 DEBUG 级别控制的选路日志；第一版请求计数不增加 reason 标签。

### 耗时

- 决策耗时以 `System.nanoTime()` 的差值计量，仅包围首次 `decide()`，不含候选池过滤、负载均衡或网络转发。
- 请求延迟沿用 `DateUtils.acquireMillisBetween(startDateTime, LocalDateTime.now())`，起点为 ShenyuContext 创建时间。这里是每个请求的上下文初始化，不是网关进程启动时间。
- 结束点沿用既有逻辑：下游链正常完成或报错时，响应已经提交则在此刻记录；尚未提交则在随后执行的 `beforeCommit` 回调中记录。因此不能统一称为“响应头提交延迟”，它可能包含响应体处理时间，也不保证字节已发送到客户端。
- 请求延迟包含此前的重试等待。沿用的墙上时钟计算可能受系统时钟调整影响。
- 请求上下文未设置 startDateTime 时，沿用 MetricsPlugin 进入时刻作为起点。
- 取消不触发 doOnSuccess/doOnError：无论响应头是否已提交，此路径只结算 cancelled 请求计数，不新增延迟样本。
- 响应已提交但下游链尚未结束时，不提前记录延迟或 success；之后完成或报错时再记录延迟和最终结果。
- 外层错误处理器在转发异常后提交错误响应，可触发 doOnError 安装的延迟回调。异常后始终没有响应提交，则没有延迟样本。请求结果描述 MetricsPlugin 下游链，不包含外层错误处理器写错误响应时的二次失败。
- 延迟直方图包含已提交的拒绝及错误响应，不能按最终 outcome 过滤，不是成功请求专用延迟。

本实现保留原 execute 的写法和延迟记录时点，只在执行链末尾增加 doFinally 结算灰度请求结果。默认 DefaultShenyuPluginChain 本身已通过 Mono.defer 延迟执行插件；MetricsPlugin 不再额外包一层。自定义链若在返回 Mono 前直接抛异常，仍沿用原有的直接抛出行为，不经过这里的响应式统计回调。请求延迟保留一次性记录标记，防止同一 exchange 重入后重复上报。

仅为决策耗时新增 `MetricsReporter.observe(..., double)` 和相应 SPI，纳秒除以 `1e9` 后以小数秒上报。
请求延迟继续使用旧 `recordTime(..., long)`。自定义 MetricsRegister 可以继续编译和运行，但须覆写 `observe` 才能输出决策直方图；默认跳过，避免截断为零。
支持显式桶边界，Prometheus 使用以下初始设置：

- 决策秒：`0.000001, 0.000005, 0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1`。
- 请求毫秒：`1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 30000`。

决策桶定义在 `CanaryMetric`；现有请求延迟桶在 `MetricsReporter.register` 中按毫秒设置。可按实际分布调整；部署时同名直方图须保持桶配置一致。

## 统计范围与兼容性

- 灰度配置存在但 enabled=false、percentage=0 或条件未命中：仍执行分区决策，归 Stable。
- 无灰度配置、指定域名绕过灰度：不产生灰度指标。
- 请求大小校验失败、整个 selector 的可用上游列表为空：仍沿用决策之前的返回路径，不为观测额外调用 decide，因此不产生灰度指标。
- decide 自身抛出异常时尚无分区结果，不产生灰度指标；原有全局异常计数继续适用。
- 普通请求计数沿用原有口径，本改动没有给此前以成功 HTTP 状态返回的业务拒绝重新分类。现有请求延迟增加 partition 标签，保留原来的记录时点。
- 现有延迟指标的标签和桶边界发生变化，需要同步调整看板聚合，不能宣称所有旧查询结果不变。完成进程升级后再比较新口径；滚动升级期间不要将新旧桶配置混合计算分位数。
- MetricsReporter 沿用既有异常传播行为，不统一捕获上报异常；未配置 reporter 时仍跳过上报。Divide 的首次选路通知保留本地回调异常处理；注册阶段仍暴露配置错误。
- 每组配置会产生多个标签组合和直方图时序。标签只使用配置 ID 和固定枚举，仍需关注大量规则及规则 ID 频繁更替造成的时序增长。

## 查询与看板示例

以下请求量、错误率和回退查询跨实例聚合并保留规则维度，延迟查询仅保留分区。实际部署时添加 job/环境过滤，避免不同网关配置的 ID 混在一起。
请求计数按终止时间记录，回退和决策按首次选路时间记录，不保证短窗口内逐项相等。

各分区、结果的请求速率：

```promql
sum by (selector, rule, partition, outcome) (
  rate(shenyu_canary_requests_total[5m])
)
```

已完成且非拒绝、非取消的请求中 Canary 的归属占比：

```promql
(
  sum by (selector, rule) (
    rate(shenyu_canary_requests_total{partition="canary", outcome=~"success|error"}[5m])
  )
  or on (selector, rule)
  0 * sum by (selector, rule) (
    rate(shenyu_canary_requests_total{outcome=~"success|error"}[5m])
  )
)
/
sum by (selector, rule) (
  rate(shenyu_canary_requests_total{outcome=~"success|error"}[5m])
)
```

该比例描述请求归属，不等于底层发送次数，也不能直接校验配置百分比；条件匹配、sticky key 缺失和空池回退都会影响它。
未创建过的 Canary 时序通过已有请求时序补零；总流量为零时显示无数据，不应当作 0% 的健康结论。

各分区的请求错误率（不含拒绝和取消）：

```promql
(
  sum by (selector, rule, partition) (
    rate(shenyu_canary_requests_total{outcome="error"}[5m])
  )
  or on (selector, rule, partition)
  0 * sum by (selector, rule, partition) (
    rate(shenyu_canary_requests_total{outcome=~"success|error"}[5m])
  )
)
/
sum by (selector, rule, partition) (
  rate(shenyu_canary_requests_total{outcome=~"success|error"}[5m])
)
```

Stable/Canary 分区请求延迟 P95（毫秒，跨规则聚合）：

```promql
histogram_quantile(0.95,
  sum by (partition, le) (
    rate(shenyu_execute_latency_millis_bucket{partition=~"stable|canary"}[5m])
  )
)
```

现有看板如需继续查看所有请求的平均延迟，应聚合新增的分区序列（毫秒，包含 none）：

```promql
sum(rate(shenyu_execute_latency_millis_sum[5m]))
/
sum(rate(shenyu_execute_latency_millis_count[5m]))
```

十分钟内的成功回退次数：

```promql
sum by (selector, rule, reason) (
  increase(shenyu_canary_fallback_total[10m])
)
```

建议看板并列展示请求速率及样本量、错误率、拒绝率、取消率、请求延迟和回退次数。
缺失时序与真实零值应区分；低样本窗口不据此判断 Canary 优劣。第一版不设置未经流量基线校准的自动回滚阈值。
