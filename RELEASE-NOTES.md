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



