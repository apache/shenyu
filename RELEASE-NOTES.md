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
19. Add support regsiter instance to consul
20. Add admin Support oracle database

### API Changes

### Enhancement

1. Enhancement cache pluign
2. Enhancement divide plugin

### Refactor
1. Refactor spring cloud loadbalancer
2. Refactor thread pool
4. Refactor max memory config logic
5. Refactor cors logic
6. Refactor selector match
7. Refactor fixed and elastic connection provider pool
8. Refactor uri register
9. Refactor zk client delete logic
10. Refactor IpUtils
11. Refactor result wrap
13. Refactor app auth
14. Refactor http client
15. Refactor proxy and webclient remove host
16. Refactor shared thread pool

### Bug Fix
1. Fix divide has nullpointerexception
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
12. Fix hiden logic bug
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

1. Optimze code about dubbo async call
1. Add loadbalancer common module
1. Optimize sql init
1. Refactor Admin PageHelper to query list
1. Optimize GlobalErrorHandler
1. Optimize the return value of the'skip' method interface of'ShenyuPlugin' to boolean
1. Optimize register rules
1. Modify dubbo and sofa param resolve service
1. Refactor sign plugin api
1. Remove websocket filter

### Refactor

1. Remove lombok dependency
1. Remove mapstruct dependency
1. Support JDK8 ~ JDK15
1. Add missing plugin_handle sql for plugin motan

### Bug Fix

1. Fix JsonSyntaxException in jwt plugin
1. Fix sql miss for resilience4j plugin handler
1. Fix disruptor problem of hold event data in consume event
1. Fix deadlock bug of HealthCheckTask
1. Fix client retry the connection add log and increase sleep time
1. Fix the default_group of nacos
1. Fix maven ignore and docker entrypoint
1. Fix admin Return password question
1. Fix LDAP query built from user-controlled source
1. Fix the IP address retrieval error
1. Fix Gson toJson is null
1. Fix the index out of range bug for context path.
1. Fix monitor init metrics label bug
1. Fix GlobalErrorHandler error object to map bug by JacksonUtils.toMap
1. Fix modify response plugin order bug
1. Fix the bug of register
1. Fix sofa plugin register metadata and parameters resolve
1. Fix motan ,dubbo, sofa plugin metadata init bug


## 2.4.0

### New Features

1. Support reading init_script file which is not under resource/directory
1. Display the plugin menus in categories
1. Admin add execute Multi-path sql script
1. IpUtils add a parameter to select the network ip
1. Add parameter-mapping plugin
1. Support Consul as shenyu-register-center
1. Support Etcd as shenyu-sync-data-center
1. Add sentinel customized fallbackhandler
1. Add response plugin
1. Add JWT plugin
1. Add Request plugin
1. Add Motan plugin
1. Add Logging plugin
1. Add Modify-response plugin
1. Add Oauth2 plugin
1. Add Menu Resource Permissions
1. Add Data Permissions

### API Changes

1. Change the project name from Soul to ShenYu
1. Change the group id from org.dromara to org.apache.shenyu

### Enhancement

1. H2 support insert ingore into in Mysql model
1. Improvements For the Apache Dubbo plugin
1. Optimization of GRPC plugin

### Refactor

1. Refactor code about "async invoke" is not supported in Dubbo lower than 2.7.3
1. Replace the term Operator by Predicate
1. Refine judge conditions operator
1. Refactor PredicateJudge module using SPI
1. Refactor code about client register

### Bug Fix

1. Fix the JwtUtil.getUserId method bug
1. Fix the  shenyu-spring-boot-starter bug
1. The encoded urlPath will be re-encoded in WebClientPlugin
1. Replace The Risky Cryptographic Algorithm "AES/ECB/NoPadding"
1. ReadTimeoutHandler on a channel which in a PooledConnectionProvider would cause an unexpected ReadTimeoutException
1. Got ClassNotFoundException while start my Gateway in 2.4.8 spring boot


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
