## [v2.7.0]- 2024-12-23

### ✨ New Features

1. Upgrade dockerfile java runtime version 8 to 17
2. Upgrade SpringBoot to 3.x
3. Support ShenYu Admin Cluster
4. Upgrade checkstyle plugin to 3.4.0
5. Datasource support OceanBase
6. Supports batch modification of selector/rule status
7. Supports batch modification of PathAuth status 
8. Upgrade apache dubbo version 
9. Support `Contribute with Gitpod`
10. Support Configs Export And Import 
11. Add shenyu client heartbeat 
12. Support Namespace 
13. Support k8s dynamically scale 
14. Invalidate Previous Tokens on New Login by Implementing Client ID Validation
15. Support for gray release in divide-plugin
16. Support Kubernetes registry
17. Add shenyu-plugin-wasm

### ⚡Enhancement

1. Add rocketmq logging e2e test
2. Enhance metrics-ratelimiter collect
3. Enhance metrics collection for Sentinel, Resilience4j, and Hystrix
4. Arrange sofa common tools dependencies
5. Remove expired comments
6. Add missing license
7. Set up callback for send message on Kafka
8. Use the loadbalance configuration from metadata for Dubbo
9. Add non null validation for upstream which obtained from select
10. Set timeout which from rule handle to dubbo rpc context
11. Publish event when enable selector and rule
12. Remove closed session from the NAMESPACE_SESSION_MAP
13. Add test case for ShenyuClientURIExecutorSubscriber
14. Add test case for `ShenyuClientIllegalArgumentException`
15. Add test case for `ShenyuClientRegisterEventPublisher`
16. Add test case for `ShenyuClientMetadataExecutorSubscriber`
17. Add test case for `AbstractWasmPluginDataHandler`
18. Add test case for `ShenyuClientRegisterRepositoryFactoryTest`
19. Add test case for `AbstractWasmDiscoveryHandler`
20. Upgrade sofa rpc version support
21. Add header key of Sign plugin to CrossFilter config
22. Encrypt the password
23. Add AbstractShenyuWasmPluginTest
24. RewritePlugin/ContextPathPlugin supports across application and plugin
25. Remove duplicate path check
26. Remove Alibaba Dubbo Support
27. Support docker env set http path
28. Add some code refactor improve
29. Support get token from cookie\header\param
30. Make the default value of ShenyuDubboService annotation equal to that of DubboService annotation
31. Add db script into admin package
32. Get rid of the dead code and add some improvements
33. MotanServiceEventListenerTest case optimization
34. Delete duplicate maven in shenyu-registry-eureka.xml
35. Jwt dependency updated
36. Print plugin execute time
37. Discovery Local support upstream health check in Admin
38. Close rule cache
39. Less concurrency
40. Optimize logic to avoid "orElse" execution，Update VersionTwoExtractor.java

### ♻️Refactor

1. Admin distributed lock by spring-integration-jdbc
2. Refactor beanUtils
3. Remove macos ci
4. Update logging plugin DataBuffer deprecated method
5. Modify e2e k8s to docker compose
6. Migrate Admin swagger from springfox to springdoc
7. Refactor springcloud plugin
8. Refactor some code
9. Delete SO_SNDBUF & SO_RCVBUF
10. Refactor shenyu-sync-data-http : replace log %s -> {}.
11. Optimizing the node type listener
12. Refactor plugin lifecycle
13. Adjust code order and remove invalid input parameters

### 🐛Bug Fix

1. Fix duplicate header for request plugin
2. Fix proxy.selector and discovery not delete when delete divide selector
3. Fix LoggingPlugin error log catch
4. Fix logging plugin sample bug
5. Fix memory overflow
6. Fix rewrite integrated test
7. Fix AbstractWasmPluginDataHandlerTest
8. Fix missing PRIMARY KEY in sql-script/h2/schema.sql
9. Fix Data dictionary page data sorting exception
10. FIx doc error
11. Resolve dashboard routing mismatch post context-path update
12. Fix etcd sync config problem
13. Fix consul sync problem
14. Fix the bug of being unable to query without registration
15. Fix Plugin Edit Page Issue by Correcting Plugin ID Query and Updating Data Type
16. Fix class AdminConstants has word spelling error
17. Fix shenyu-examples-springmvc start failed
18. Fix dashboard menu children sort not working problem
19. Fix ShenyuApacheDubboXmlProviderApplication config
20. Fix data sync dataId for proxy selector and discovery is not unique
21. Filter disable dict option
22. Fix SpringCloudParser MetaData null data
23. Fix client register validation
24. Config dubbo serialize-check-status=DISABLE
25. Fix example TestApacheDubboXmlApplication start failed
26. Fix the nacos data sync model missing the contextPath configuration
27. Fix SPI create non singleton objects in multi-threaded scenarios
28. Fix BadSqlGrammarException
29. Fix some word typo error
30. Fix ListUtil->merge exception
31. Fix metaData disable not filtered
32. Fix divide logging request method
33. Fix e2e chunk header error
34. Fix cookie error and sql check
35. Fixed NPE issue
36. Fix Invalid path error
37. Fix hot load issue
38. Fix e2e test case can not run wget command
39. Fix fallback issue
40. Fix some ci bugs
41. Resolve the sql error in rule-sqlmap.xml
42. Fix readYmlBuildRepository NPE
43. Fix nacos cannot be registered in the Shenyu-examples-SpringCloud project
44. Fix springCloud ruleData path setting didn't used
45. Fix shenyu-plugin-logging-elasticsearch : modify setIndexName of ElasticSearchLogConfig
46. Fix Not first offline from the gateway when stopping service
47. Fix k8s liveness probe can not run wget command error
48. Fix AbstractNodeDataSyncService load discoverUpstream on startup


## 2.6.1

### New Features

1. Add dubbo annotation analysis for shenyu ingress controller
2. Support plugin lifecycle
3. Support shenyu-sdk-client by openFeign
4. Adding a Motan plugin and Spring Cloud to shenyu ingress-controller
5. Support alert notice
6. Shenyu client add discovery register center
7. Add shenyu  context-path plugin ingress controller
8. Add shenyu grpc plugin ingress controller
9. Add shenyu sofa ingress controller
10. Add nacos, etcd, eureka as discovery service
11. Add new plugin: basic-auth
12. Add shenyu logging-rabbitmq plugin
13. Binding selector by shenyu-discovery

### API Changes

1. Refactor shenyu sync data data structure

   ```
   plugin.list ["plugin.sign", "plugin.dubbo", "plugin.pluginName"]
   -> plugin.sign
   -> plugin.dubbo
   -> plugin.pluginName
   
   examples data:
   selector.key1.list ["selector.key1.value1", "selector.key1.value2", "selector.key1.value3"]
   -> selector.key1.value1
   -> selector.key1.value2
   -> selector.key1.value3
   
   selector.key2.list ["selector.key2.value1", "selector.key2.value2", "selector.key2.value3"]
   -> selector.key2.value4
   -> selector.key2.value5
   -> selector.key2.value6
   ```

2. Support NettyClient as default httpclient

3. Refactor admin-listener to support admin data sync

4. Remove brpc supports including brpc plugin, brpc example, brpc integrated test

5. Remove Apollo dependency to support Java 17(add jar by yourself)

6. Remove shenyu middleware register client

### Enhancement

1. Add test cases for shenyu model event
2. Add selector event test cases
3. Add motan e2e test cases
4. Support the motan protocol
5. Add Grpc e2e test cases
6. Bump apache-rat-plugin to 0.15
7. Support String isBlank in match condition
8. Clickhouse support ttl field
9. Support HttpUtils log level
10. Add unit test for Ingress Reconciler
11. Support checksum when packing distribution
12. Implement zero-copy in TCP plugin
13. shenyu-client-springmvc supports default appName and contextPath
14. Add sdk-feign example and integrated test case
15. ElasticSearch logging support for custom index
16. Enhance grpc plugin to support shenyu loadbalancer
17. Support http2 upstream server
18. Enhance Dubbo plugin support shenyu loadbalancer
19. Add ingress-controller integration springCloud test case
20. Add WebSocket plugin proxy ping configuration
21. Add ingress-controller integration websocket test
22. RewritePlugin supports percentage
23. Admin use discovery config init discovery-service
24. Divide plugin adapt shenyu discovery
25. Alert report support config admin cluster multi servers
26. WebSocket plugin adapt shenyu discovery
27. Register service instance into discovery
28. Admin adapter discovery local mode
29. Add test case for ShenYu sdk core
30. Add unit test for shenyu-discovery
31. Add opengauss e2e test case
32. Add upload plugin jar size limit
33. Add unit test for shenyu-client-spring-websocket
34. Upgrade Shiro version to a secure version(1.18.0)
35. Update license and upgrade springboot(2.7.17)
36. Send alarm message when gateway global error happen
37. Add EurekaDiscoveryService unit test

### Refactor

1. Refactor 2.6.1 version(pom.xml)
2. Simplify Map operations by computeIfAbsent
3. Refactor polaris test cases
4. Migrate Maven Wrapper from io.takari to official release
5. Use compiled Pattern in WebClientMessageWriter
6. Refactor HttpUtils request method
7. Update github action version
8. Refactor Sync data center abstract template method
9. Update MenuProject, MenuModule, MenuDocItem to VO
10. Unified dubbo version
11. Refactor Httpclient's package
12. Refactor github ci action cache
13. Refactor motan pojo as rpc method parameter
14. Upgrade Kafka client version to 3.4.0
15. Migrate Admin swagger from springfox to springdoc
16. Upgrade Dubbo version to 3.2.5 and refactor some methods
17. Refactor AbstractShenyuSdkClient getOrDefault method
18. Refactor http client properties
19. Refactor webclient plugin implement
20. Upgrade com.google.guava:guava to 32.0.0-jre
21. support k8s as e2e test case environment
22. Refactor @Restapi as rest api request mapping
23. String concatenation recommended using StringBuilder
24. Set the netty allocator to unpooled
25. Refactor startup banner
26. Removing duplicate code and extracting the same code for common use
27. Standardized null detection coding
28. Refactor log plugin selector handler
29. Refactor plugin classloader
30. Refactor Logging plugin to support sampleRate at plugin level
31. Refactor context-path register to avoid repeat context-path(use select for update)

### Bug Fix

1. Avoid the permanent overhead of creating TimeoutException
2. Fix example module main class path
3. Fix plugin page sorting bug
4. Update Makefile SNAPSHOT version
5. Fix typo in RELEASE-NOTES.md
6. Fix the error package name of shenyu-example
7. Fix password rules, add special characters '#' and '.'
8. Fix health check for zookeeper:3.8.0 in e2e
9. Fix unstable ci check
10. Add e2e WaitForHelper exception log
11. Fix springcloud plugin can't get scheme
12. Fix javadoc build errors
13. Fix the wrong request type in HttpUtils
14. Fix  userId can not update success when update auth
15. Fix thread leak in TCP plugin
16. Format "Quick start" part in shenyu-integrated-test/README
17. Fix SQL script error
18. Fix uri plugin path error and change path to rawpath
19. Fix WebSocket plugin to support rewrite plugin
20. Fix indexName not working for es-logging
21. Fix the error of context-path plugin
22. Fix shenyu-admin cpu surge
23. Fix alert localDateTime format problem
24. shenyu-client persist ApiDoc error retry
25. Fix applicationContextAware initialization too late
26. Fix duplicate response header
27. Set the maximum time to wait for the k8s cluster to start up
28. Fix type for status field for clickhouse log plugin
29. Fix response plugin memory leak
30. Fix dataType contrast error
31. Fix http data sync error
32. Fix spelling error
33. Fix shenyu-dubbo register status
34. Fix buildDiscoveryUpstreamPath causing multiple `/`
35. Fix bug when registering with Eureka through EurekaInstanceRegisterRepository#persistInstance
36. Fix AbstractLogPluginDataHandler hashcode error
37. Fix Ratelimit plugin key error in redis cluster mode
38. Fix multi shenyu client register repeat context path
39. Fix shenyu can't load ext plugin after close the plugin
40. Fix upload plugin jar bug in shenyu admin
41. Fix plugin can not load resource path file
42. Fix Admin script to show dictionary code
43. Fix authorization conflict in sign plugin
44. Fix sign plugin context path match error



## 2.6.0

### New Features

1. Support shenyu-admin expose prometheus metrics
2. Add shenyu Level-1 cache and Level-2 cache
3. Save extend plugin jar to shenyu admin
4. Support shenyu upload plugin hot load in gateway
5. Support apollo sync data and register data
6. Initializes client information collection
7. Support spring-boot-client auto config in shenyu client
8. Add TCP plugin
9. Super admin forces password change
10. Spring-mvc(boot) client support collect api-meta
11. Add zookeeper discovery sync
12. Initializes Shenyu ingress controller
13. Add discovery upstream and proxy selector proxy
14. Expose shenyu actuator endpoint
15. Add naocs discovery sync
16. Add apollo discovery sync
17. Add HttpLongPolling discovery sync
18. Add consul discovery sync
19. Add huawei cloud lts logging plugin
20. Support openGauss database for shenyu admin
21. Support polaris config sync and register center

### API Changes

### Enhancement

1. Add tags for shenyu api doc client
2. Add brpc integrated test
3. Brpc support shared thread pool
4. Add mapType to cryptor request and response plugin
5. Crypto plugin supports multi field names
6. Add p2c loadbalancer
7. Modify plugin jar to Base64-string in plugin data
8. Add shortest response load balancer
9. Add hash load balancer test case
10. Add DetailSerivice test case
11. Tolerant path slash for shenyu
12. Add shenyu-common enums test case
13. Add shenyu-common dto test case
14. Add shenyu-admin model test case
15. Add shenyu match cache test case
16. Support k8s probes
17. Add shenyu-admin service test case
18. Add document json data in api doc
19. The SPEL in the mock plugin is secure by default
20. Add test cases for ShenyuClientApiDocExecutorSubscriber
21. Add test case for shenyu-client-sofa module
22. Add tag relation for shenyu api doc
23. Support shenyu admin, bootstrap service stop script bat in windows
24. Add test case for ShenyuSdkClientFactory
25. Add websocket synchronization method for e2e-springcloud
26. Support divide plugin active offline
27. Add springcloud service instance cache
28. Change password support i18n
29. Add websocket sync for shenyu discovery
30. Update springboot to 2.7.13
31. Add other syn method e2e-spring-cloud
32. Add api doc client generated annotation attribute
33. Update zookeeper client register repository active offline
34. Update apollo client register repository active offline
35. Storage adjustment for swagger type API documents, change from local cache to database
36. Support nacos client offline
37. Add e2e alibaba dubbo test case
38. Add e2e apache dubbo test case
39. Add shenyu-spring-sdk test cases
40. Add e2e sofa test cases
41. Add apollo config sync test case
42. Add database connection pool
43. Add idea icon for shenyu project

### Refactor

1. Optimize shenyu-admin
2. Refactor least active balance algorithm
3. Optimized version-one for sign-plugin
4. Optimize upstream check service
5. Resolve shenyu project global version
6. Refactor ShenyuConsulConfigWatch code
7. Refactor shenyu trie codes
8. Check uri condition of rule when saving
9. Optimize shenyu-client code for shenyu-client-websocket
10. Add license for admin dependency micrometer
11. Update maven-assembly-plugin to 3.5.0
12. Optimize global plugin sorting
13. Use BearerToken replace StatelessToken in shenyu-admin
14. Refactor shenyu-logging module
15. Add validation for api doc
16. Optimize Trie code and improve wildcard * supporting
17. Refactor the custom plugin support hot load
18. Refactor ShenyuWebHandler putPlugin methods
19. Refactor Shenyu webfilter
20. Reactor oauth2 plugin
21. Refactor shenyu selector data continued field
22. Refactor shenyu selector and rule cache
23. Removed unused generics in shenyu client
24. Refactor shenyu-plugin-sentinel plugin
25. Refactor cache and add endpoint to expose cache
26. Refactor checkUserPassword, not print known error log when startup
27. Add some parameters for log
28. Refactor shenyu global exception handler
29. Add shenyu upload plugin integrated test case
30. Optimize some syntactic sugar
31. Change discovery_upstream discovery_handler_id
32. Refactor shenyu global exception handlers
33. Refactor shenyu plugin module
34. Refactor AlibabaDubboConfigCache
35. Remove hutool from dependency
36. Refactor ShenyuClientShutdownHook
37. Extractor add BaseAnnotationApiBeansExtractor
38. Support multi-client registration
39. Refactor shenyu-e2e support shenyu check style
40. Refactor shenyu client register base
41. Add domain test for shenyu divide plugin
42. Update other rpc_ext for the same service
43. Optimize consul connect operation
44. Refactor shenyu e2e springcloud yaml change
45. Add integrated test for k8s ingress controller
46. Split the document field of the apidoc detail interface,and add fields such as requestHeaders and responseParameters
47. Add swagger sample project to test the relevant functionality of the API documentation
48. Optimize the display of form fields in json format
49. Refactor shenyu log observability
50. Add bootstrap start log
51. Refactor api document for swagger
52. Upgrade grpc version to 1.53.0
53. Refactor api meta data process function
54. polish master code and pom

### Bug Fix

1. Smart h2 path
2. Fix crypto-response plugin
3. Fix jdk8 Map computeIfAbsent performance bug
4. Fix zombieRemovalTimes code
5. Fix the upgraded sql file
6. Remove detectOfflineLinks tag
7. Ignore flattened-pom
8. Fix LOG invoke method
9. Fix NPE for shenyu-example-springcloud with nacos
10. Fix shenyu-admin names for arguement of type
11. Fix loadbalance spi resource
12. Fix sql script error
13. Fix to 24-hour format and timeZone for jackson
14. Fix JwtUtils error
15. Fix dubbo invoker cache bug
16. Fix missing HOST delete operation
17. Fix SpringMvcClientEventListener test case
18. Fix pass update PENDING_SYNC for zombie
19. Fix Memory leak
20. Fix rule query failed because there are too many rules
21. Fix missing actuator dependency and port error in examples http
22. Fix UpstreamCheckUtils http and https
23. Fix FileFilter make memory leak
24. Fix zookeeper sync error
25. Fix MemorySafeWindowTinyLFUMap memory leak error
26. Fix lack separator of path of ApiDoc
27. Fix NPE for shenyu trie
28. Fix plugin skip error
29. Fix oracle sql error
30. Fix shenyu icon can't load in shenyu admin
31. Fix hystrix fallback bug
32. Fix warm up time for divide and springcloud
33. Fix springcloud service chooser
34. Fix shenyu-spring-boot-starter-plugin-mock add spring.factories
35. Fix shenyu-client-mvc and shenyu-client-springcloud lose ip
36. Fix empty rule data and selector data in cache
37. Fix api document module update api detail error
38. Fix get topic from config in KafkaLogCollectClient
39. Fix logging console thread safety
40. Fix brpc integration testing response size
41. Fix selector update gray remove cache for plugn-dubbo-common
42. Fix shenyu admin menu name bug
43. Fix shenyu admin cannot configure consul port
44. Fix shenyu client metadata and uri cannot sync to admin with apollo
45. Fix PathVariable annotation url cannot match
46. Fix could not update uri in PathPattern mode
47. Fix client shutdown method call twice
48. Fix shenyu mishandle consul configuration
49. Remove unused configuration from Request, modifyResponse plugin
50. Fix http registration metadata
51. Fix websocket lost the user-defined clost status
52. Fix consul register lose the prop of meta-path when special symbol
53. Fix etcd sync error
54. Fix admin sync error
55. Fix shenyu motan plugin execute error

## 2.5.1

### New Features
1. Add brpc example
2. Add spring boot starter plugin brpc&admin
3. Add brpc-plugin
4. Add shenyu-client-api-doc
5. Add sdk duplicate class check
6. Support diff nacos namespace
7. Add array method of expression in mock plugin
8. Support generation of mock data on request
9. Support user specify http request domain
10. Add MockRequestRecord
11. Development shenyu-register-instance-eureka
12. Support API document Api doc detail mapper
13. Add api doc ddl
14. Add TagMapper and TagRelationMapper
15. Add api and api_rule_relation mapper
16. Not config rule
17. Refactor message readers
18. Add sentinel rule handle parameter
19. Add shenyu-e2e test engine
20. Make an Apache Shenyu SSO authentication plugin based on casdoor
21. Add logging-tencent-cls plugin
22. Support clickhouse-logging-pugin
23. Add logging-pulsar plugin
24. Add new plugin: key-auth
25. Fix sign plugin DataBufferLimitException error
26. Fix context-path error

### API Changes

### Enhancement
1. Add simpler client annotations for motan
2. Add simpler client annotations for websocket
3. Add configuration in starter for motan plugin
4. Add convenience annotation for shenyu-client-springcloud and shenyu-client-springmvc

### Refactor
1. Refactor some code for mock request of api doc
2. Refactor logging-clickhouse
3. Polish maven dependencies of dubbo
4. Refactor sign plugin
5. Update ShenyuExtConfiguration
6. Remove unnecessary singleton
7. Fix generating mock data in multithreading
8. Refactor sdk test and processArgument
9. Refactor DefaultSignService
10. Fix shenyu-admin rule
11. Optimized ShaUtil
12. Fix cache too large
13. Fix ConcurrentModificationException
14. Fix sync data in etcd
15. Refactor shenyu sdk client
16. Optimize request timeout response
17. Refactor log module
18. Refactor shenyu-client-springcloud
19. Refactor MotanServiceEventListener
20. Refactor shenyu-admin sync data listener
21. Refactor shenyu-client-tars
22. Refactor client sdks alibaba dubbo
23. Refactor springmvc client
24. Refactor admin mapper config
25. Refactor shenyu-plugin-logging
26. Optimize random algorithm
27. Refactor random loadbalancer
28. Refactor logging-kafka

### Bug Fix
1. Remove redundant cookie setting
2. Fix appAuth delete
3. fix Cryptor-Request Plugin
4. To avoid load the same ext plugins repeatedly
5. Fix the TagRelationQuery
6. Fix upgrade sql
7. Fix Nacos register NPE
8. Fix sandbox json parsing
9. Prevent the first time from failing to load
10. Fix plugin update bug by modifying config field setter
11. Fix postgresql sql
12. Fix the postgresql error during ShenYu-Admin startup
13. Fix sentinel can't fuse
14. Fix TencentClsLogCollectClient
15. Fix change password error
16. Fix selector page
17. Fix request plugin can't replaceCookie
18. Fix RateLimiterPlugin concurrent handler error

## 2.5.0

### New Features
1. Add logging-aliyun-sls plugin 
2. Add mock plugin 
3. Add logging-es plugin 
4. Add logging-rocketmq plugin 
5. Add logging-kafka plugin 
6. Add custom message writer in response plugin 
7. Add record log in admin 
8. Add apache dubbo http 
9. Add nacos register 
10. add the logic of annotation on the splicing class for sofa client 
11. Add the logic of annotation on the splicing class for motan client 
12. Add netty http server parameters 
13. Add the logic of annotation on the splicing class for apache dubbo client 
14. Add alert module 
15. Add support configurable timeout for MotanPlugin 
16. Add api document
17. Add user permissions Exclude admin
18. Add springBoot upgrade to 2.6.8
19. Add support register instance to consul
20. Add admin Support oracle database

### API Changes

### Enhancement

1. Enhancement cache plugin
2. Enhancement divide plugin

### Refactor
1. Refactor spring cloud loadbalancer
2. Refactor thread pool
3. Refactor max memory config logic
4. Refactor cors logic
5. Refactor selector match
6. Refactor fixed and elastic connection provider pool
7. Refactor uri register
8. Refactor zk client delete logic
9. Refactor IpUtils
10. Refactor result wrap
11. Refactor app auth
12. Refactor http client
13. Refactor proxy and webclient remove host
14. Refactor shared thread pool

### Bug Fix
1. Fix null pointer exception in divide plugin
2. Fix body maxInMemorySize
3. Fix admin delay update handle in selector
4. Fix register-client loop error
5. Fix the error of combination plugin
6. Fix sofa and websocket client lossless registration
7. Fix grpc client lossless registration
8. Fix springcloud client lossless registration
9. Fix spring cloud dubbo example
10. Fix NPE repair of admin module caused by spring MVC example synchronization
11. Fix curator version incompatible in bootstrap
12. Fix hidden logic bug
13. Fix failure to load local plugins
14. Fix pg script error
15. Fix hystrix-plugin tests failure
16. Fix client registration by consul only register 1 metadata
17. Fix websocket datasync can chose allow origin to avoid CSRF

## 2.4.3

### New Features

1. Add http register client retry.
2. Support octet-stream context-type.
3. Support redirecting to URIs outside of bootstrap and refactoring code.
4. Add local API authorization.
5. Support config dubbo consumer pool.
6. Support DividePlugin failover retry.
7. Support websocket client configuration.
8. Support config netty thread pool in HttpClient.
9. Support MemoryLimitedLinkedBlockingQueue.
10. Support alibaba dubbo plugin shared thread pool.
11. Support grpc plugin shared thread pool.
12. Add Metrics Plugin.
13. Add Cache Plugin.
14. Add logging rocketmq plugin.

### API Changes

### Enhancement

1. Test combination of Param mapping, Rewrite plugin,
2. Test combination of Param mapping and Redirect plugin.
3. Test combination of RateLimiter and Rewrite plugin.
4. Test combination of RateLimiter and Redirect plugin.
5. Test combination of Request and Redirect plugin.
6. Test combination of Request and Rewrite plugin.
7. Test combination of JWT and RateLimiter plugin.
8. Test combination of JWT and Redirect plugin.
9. Test combination of JWT and Rewrite plugin.
10. Add integrated test of Resilience4j plugin.
11. Add integrated test of Hystrix plugin.
12. Update junit4 to junit5.
13. Add shenyu-examples-springmvc-tomcat.
14. Optimize the password encryption.
15. Optimize and verify shenyu-admin module interface parameters.
16. Optimize the configurable Shenyu agent log collection.
17. Optimize code about data init when sync data.
18. Add unit test for LoggingRocketMQPlugin

### Refactor

1. Used Wheel-Timer instead of ScheduledExecutorService class.
2. Remove DisruptorProvider#onData(final Consumer<DataEvent> function)
3. Synchronized instance rather than class in MetadataExecutorSubscriber.
4. Refactor admin buildHandle about register uri.
5. Spring cloud client auto set port.
6. Refactor jwt support multi-level tokens.
7. Remove monitor plugin.
8. Change logback theme.
9. remove shenyu-agent.

### Bug Fix

1. Fix init CommonUpstreamUtils NPE.
2. Make a judgment on the failure of Nacos registration.
3. Fix NPE when login with non-existent user.
4. Fix double log.
5. Fix misspelled token.
6. Fix retryCount not work bug.
7. Fix token parse error.
8. Fix the trouble of big data in Websocket.
9. Fix NettyHttpClientPlugin did not retry when failed.
10. Fix CVE-2021-41303.
11. Fix judgment of the contains condition of all plugins does not work.
12. Fix http headers lose bug.
13. Fix Bug The Rewrite Plugin should support {PathVariable} request.
14. Fix Bug about data sync with Nacos.
15. Fix Nacos namespace config.
16. Fix NPE or websocket proxy fails when the context-path plug-in is opened.
17. Fix http registers the client plug-in port occupancy detection.

## 2.4.2

### New Features

1. Add Mqtt plugin
2. Add Shenyu-Agent module support observability
3. Add opentelemetry plugin on Shenyu-Agent module
4. Add jaeger plugin on Shenyu-Agent module
5. Add zipkin plugin on Shenyu-Agent module
6. Support Shenyu instance register by zookeeper
7. Support Shenyu response data custom format
8. Support https for upstream check
9. Add RpcContextPlugin to transmit header to rpc context
10. Support cluster model for dubbo plugin
11. Support Shenyu instance register by ETCD

### API Changes

1.Add configuration properties for HTTP synchronization data
2.Remove'/shenyu-client/**','/configs/**','/plugin'interface from the whitelist interface of Shenyu admin

### Enhancement

1. Optimize global error handler for flexible processing
2. Optimized the database access in the loop
3. Optimize result media type and reset response header
4. Enhancement crossfilter filter the same headers
5. Optimize shenyu context module data
6. Optimize dubbo plugin
7. Optimize admin db operation
8. Refactor Response and Cryptor plugin
9. Optimize Admin Resource Permission loader
10. Add authentication on shenyu admin when register by http
11. Optimize netty config.
12. Optimize SQL files for resource,permission
13. Add ExcludeOperatorJudge for selector and rule
14. Add docker-compose on Shenyu-dist
15. Enhance the ability of jwt plugin

### Refactor

1. Remove SpEL and Groovy plugins
2. Optimization prompt of ExtensionLoader
3. Add http client strategy property
4. Refactor shenyu client

### Bug Fix

1. Fix sentinel Plugin-exception number is not effective
2. Fix HttpClientProperties.javaresponseTimeout can not config in yaml
3. Fix Connection reset by peer Exception on webclient
4. Fix register metadata and uri order
5. Fix Admin when press the Add button
6. Fix Spi config
7. Support Dubbo Plugin Single Parameter Primitive Type
8. Fix using etcd cluster to sync data init failed
9. Fix Shiro get white list is null bug
10. Fix zookeeper sync error handling event bug
11. Fix modify-response-plugin and cryptor-response-plugin are used in combination, and no information is returned
12. Fix the bug of missing some field in cryptor rule handler using h2


## 2.4.1

### New Features

1. Support PostgreSQL for admin
1. Support dynamic loading plugin
1. Support local modification data mode
1. Add Websocket plugin
1. Add CryptorRequest plugin
1. Add CryptorResponse plugin
1. Support Grayscale Release for SpringCloud
1. Support Grayscale Release for Apache Dubbo
1. Implement the async dubbo invoking for alibaba-dubbo
1. Support external cross filter config
1. Support sign plugin custom dynamic sign provider

### API Changes

1. Refactor shenyu config in yaml

### Enhancement

1. Optimize code about dubbo async call
2. Add loadbalancer common module
3. Optimize sql init
4. Refactor Admin PageHelper to query list
5. Optimize GlobalErrorHandler
6. Optimize the return value of the'skip' method interface of'ShenyuPlugin' to boolean
7. Optimize register rules
8. Modify dubbo and sofa param resolve service
9. Refactor sign plugin api
10. Remove websocket filter

### Refactor

1. Remove lombok dependency
2. Remove mapstruct dependency
3. Support JDK8 ~ JDK15
4. Add missing plugin_handle sql for plugin motan

### Bug Fix

1. Fix JsonSyntaxException in jwt plugin
2. Fix sql miss for resilience4j plugin handler
3. Fix disruptor problem of hold event data in consume event
4. Fix deadlock bug of HealthCheckTask
5. Fix client retry the connection add log and increase sleep time
6. Fix the default_group of nacos
7. Fix maven ignore and docker entrypoint
8. Fix admin Return password question
9. Fix LDAP query built from user-controlled source
10. Fix the IP address retrieval error
11. Fix Gson toJson is null
12. Fix the index out of range bug for context path.
13. Fix monitor init metrics label bug
14. Fix GlobalErrorHandler error object to map bug by JacksonUtils.toMap
15. Fix modify response plugin order bug
16. Fix the bug of register
17. Fix sofa plugin register metadata and parameters resolve
18. Fix motan ,dubbo, sofa plugin metadata init bug


## 2.4.0

### New Features

1. Support reading init_script file which is not under resource/directory
2. Display the plugin menus in categories
3. Admin add execute Multi-path sql script
4. IpUtils add a parameter to select the network ip
5. Add parameter-mapping plugin
6. Support Consul as shenyu-register-center
7. Support Etcd as shenyu-sync-data-center
8. Add sentinel customized fallbackhandler
9. Add response plugin
10. Add JWT plugin
11. Add Request plugin
12. Add Motan plugin
13. Add Logging plugin
14. Add Modify-response plugin
15. Add Oauth2 plugin
16. Add Menu Resource Permissions
17. Add Data Permissions

### API Changes

1. Change the project name from Soul to ShenYu
2. Change the group id from org.dromara to org.apache.shenyu

### Enhancement

1. H2 support insert ignore into in Mysql model
2. Improvements For the Apache Dubbo plugin
3. Optimization of GRPC plugin

### Refactor

1. Refactor code about "async invoke" is not supported in Dubbo lower than 2.7.3
2. Replace the term Operator by Predicate
3. Refine judge conditions operator
4. Refactor PredicateJudge module using SPI
5. Refactor code about client register

### Bug Fix

1. Fix the JwtUtil.getUserId method bug
2. Fix the shenyu-spring-boot-starter bug
3. The encoded urlPath will be re-encoded in WebClientPlugin
4. Replace The Risky Cryptographic Algorithm "AES/ECB/NoPadding"
5. ReadTimeoutHandler on a channel which in a PooledConnectionProvider would cause an unexpected ReadTimeoutException
6. Got ClassNotFoundException while start my Gateway in 2.4.8 spring boot


2.3.0（2021-04-02）
------------------
### soul-admin

* Add 'open' field  to allow app path authentication or not in sign plugin. #1168
* Optimize divide plugin to use common plugin template in soul-dashboard. #1163
* Add  default values and rule checks in plugin handler. #1112
* Add resource management to allow user to add plugin, adjust menu and button resource and so on  in  soul-dashboard and soul-admin.  #1034
* Add menu and data permission in soul-admin. #917
* Add H2 store for soul-admin #918

### soul-bootstrap

* Add tars plugin #863
* Add sentinel plugin #331
* Add sofa plugin #384
* Add Resilience4j plugin for soul-plugin. #434
* Add Context path mapping plugin for soul-plugin. #894
* Add Grpc plugin supports grpc protocol. #1081
* support form submission for dubbo plugin.#339
* feat(plugin handle): #307
* Add dist package module #320
* Add test cases for soul-admin #500
* Add register center for consul #1148
* Add register center for etcd #1161
* Add register center for nacos #1182
* Add register center for zookeeper #1141 #1139
