-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements.  See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership.  The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License.  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

CREATE DATABASE IF NOT EXISTS ApolloConfigDB DEFAULT CHARACTER SET = utf8mb4;

Use ApolloConfigDB;

--
-- Table structure for table `AccessKey`
--

DROP TABLE IF EXISTS `AccessKey`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `AccessKey` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `AppId` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `Secret` varchar(128) NOT NULL DEFAULT '' COMMENT 'Secret',
  `IsEnabled` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: enabled, 0: disabled',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_AppId_Secret_DeletedAt` (`AppId`,`Secret`,`DeletedAt`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='访问密钥';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `AccessKey`
--

LOCK TABLES `AccessKey` WRITE;
/*!40000 ALTER TABLE `AccessKey` DISABLE KEYS */;
/*!40000 ALTER TABLE `AccessKey` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `App`
--

DROP TABLE IF EXISTS `App`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `App` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `AppId` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `Name` varchar(500) NOT NULL DEFAULT 'default' COMMENT '应用名',
  `OrgId` varchar(32) NOT NULL DEFAULT 'default' COMMENT '部门Id',
  `OrgName` varchar(64) NOT NULL DEFAULT 'default' COMMENT '部门名字',
  `OwnerName` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'ownerName',
  `OwnerEmail` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'ownerEmail',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_AppId_DeletedAt` (`AppId`,`DeletedAt`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_Name` (`Name`(191))
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='应用表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `App`
--

LOCK TABLES `App` WRITE;
/*!40000 ALTER TABLE `App` DISABLE KEYS */;
INSERT INTO `App` VALUES (1,'SampleApp','Sample App','TEST1','样例部门1','apollo','apollo@acme.com',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(2,'shenyu','application','TEST1','样例部门1','apollo','apollo@acme.com',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
/*!40000 ALTER TABLE `App` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `AppNamespace`
--

DROP TABLE IF EXISTS `AppNamespace`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `AppNamespace` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `Name` varchar(32) NOT NULL DEFAULT '' COMMENT 'namespace名字，注意，需要全局唯一',
  `AppId` varchar(64) NOT NULL DEFAULT '' COMMENT 'app id',
  `Format` varchar(32) NOT NULL DEFAULT 'properties' COMMENT 'namespace的format类型',
  `IsPublic` bit(1) NOT NULL DEFAULT b'0' COMMENT 'namespace是否为公共',
  `Comment` varchar(64) NOT NULL DEFAULT '' COMMENT '注释',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_AppId_Name_DeletedAt` (`AppId`,`Name`,`DeletedAt`),
  KEY `Name_AppId` (`Name`,`AppId`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='应用namespace定义';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `AppNamespace`
--

LOCK TABLES `AppNamespace` WRITE;
/*!40000 ALTER TABLE `AppNamespace` DISABLE KEYS */;
INSERT INTO `AppNamespace` VALUES (1,'application','SampleApp','properties',_binary '\0','default app namespace',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(2,'application','shenyu','properties',_binary '\0','default app namespace',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
/*!40000 ALTER TABLE `AppNamespace` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Audit`
--

DROP TABLE IF EXISTS `Audit`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Audit` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `EntityName` varchar(50) NOT NULL DEFAULT 'default' COMMENT '表名',
  `EntityId` int unsigned DEFAULT NULL COMMENT '记录ID',
  `OpName` varchar(50) NOT NULL DEFAULT 'default' COMMENT '操作类型',
  `Comment` varchar(500) DEFAULT NULL COMMENT '备注',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=16 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='日志审计表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Audit`
--

LOCK TABLES `Audit` WRITE;
/*!40000 ALTER TABLE `Audit` DISABLE KEYS */;
INSERT INTO `Audit` VALUES (1,'App',2,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 09:26:47',NULL,'2023-06-08 09:26:47'),(2,'AppNamespace',2,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 09:26:47',NULL,'2023-06-08 09:26:47'),(3,'Cluster',2,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 09:26:47',NULL,'2023-06-08 09:26:47'),(4,'Namespace',2,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 09:26:47',NULL,'2023-06-08 09:26:47'),(5,'Cluster',3,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 09:27:01',NULL,'2023-06-08 09:27:01'),(6,'Namespace',3,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 09:27:01',NULL,'2023-06-08 09:27:01'),(7,'Item',2,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:48',NULL,'2023-06-08 11:02:48'),(8,'Release',2,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:48',NULL,'2023-06-08 11:02:48'),(9,'ReleaseHistory',2,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:48',NULL,'2023-06-08 11:02:48'),(10,'Item',3,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49',NULL,'2023-06-08 11:02:49'),(11,'Release',3,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49',NULL,'2023-06-08 11:02:49'),(12,'ReleaseHistory',3,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49',NULL,'2023-06-08 11:02:49'),(13,'Item',4,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49',NULL,'2023-06-08 11:02:49'),(14,'Release',4,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49',NULL,'2023-06-08 11:02:49'),(15,'ReleaseHistory',4,'INSERT',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49',NULL,'2023-06-08 11:02:49');
/*!40000 ALTER TABLE `Audit` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Cluster`
--

DROP TABLE IF EXISTS `Cluster`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Cluster` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `Name` varchar(32) NOT NULL DEFAULT '' COMMENT '集群名字',
  `AppId` varchar(64) NOT NULL DEFAULT '' COMMENT 'App id',
  `ParentClusterId` int unsigned NOT NULL DEFAULT '0' COMMENT '父cluster',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_AppId_Name_DeletedAt` (`AppId`,`Name`,`DeletedAt`),
  KEY `IX_ParentClusterId` (`ParentClusterId`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='集群';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Cluster`
--

LOCK TABLES `Cluster` WRITE;
/*!40000 ALTER TABLE `Cluster` DISABLE KEYS */;
INSERT INTO `Cluster` VALUES (1,'default','SampleApp',0,_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(2,'default','shenyu',0,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(3,'test','shenyu',0,_binary '\0',0,'apollo','2023-06-08 09:27:01','apollo','2023-06-08 09:27:01');
/*!40000 ALTER TABLE `Cluster` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Commit`
--

DROP TABLE IF EXISTS `Commit`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Commit` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `ChangeSets` longtext NOT NULL COMMENT '修改变更集',
  `AppId` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `ClusterName` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'ClusterName',
  `NamespaceName` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'namespaceName',
  `Comment` varchar(500) DEFAULT NULL COMMENT '备注',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`),
  KEY `AppId` (`AppId`(191)),
  KEY `ClusterName` (`ClusterName`(191)),
  KEY `NamespaceName` (`NamespaceName`(191))
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='commit 历史表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Commit`
--

LOCK TABLES `Commit` WRITE;
/*!40000 ALTER TABLE `Commit` DISABLE KEYS */;
INSERT INTO `Commit` VALUES (1,'{\"createItems\":[{\"namespaceId\":3,\"key\":\"shenyu.plugin.json\",\"type\":0,\"value\":\"{\\\"request\\\":{\\\"id\\\":\\\"20\\\",\\\"name\\\":\\\"request\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":120},\\\"loggingPulsar\\\":{\\\"id\\\":\\\"35\\\",\\\"name\\\":\\\"loggingPulsar\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"serviceUrl\\\\\\\": \\\\\\\"pulsar://localhost:6650\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":185},\\\"jwt\\\":{\\\"id\\\":\\\"19\\\",\\\"name\\\":\\\"jwt\\\",\\\"config\\\":\\\"{\\\\\\\"secretKey\\\\\\\":\\\\\\\"key\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":30},\\\"paramMapping\\\":{\\\"id\\\":\\\"22\\\",\\\"name\\\":\\\"paramMapping\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":70},\\\"modifyResponse\\\":{\\\"id\\\":\\\"23\\\",\\\"name\\\":\\\"modifyResponse\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":220},\\\"sign\\\":{\\\"id\\\":\\\"1\\\",\\\"name\\\":\\\"sign\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":20},\\\"loggingElasticSearch\\\":{\\\"id\\\":\\\"32\\\",\\\"name\\\":\\\"loggingElasticSearch\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"localhost\\\\\\\", \\\\\\\"port\\\\\\\": \\\\\\\"9200\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":190},\\\"dubbo\\\":{\\\"id\\\":\\\"6\\\",\\\"name\\\":\\\"dubbo\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"zookeeper://localhost:2181\\\\\\\",\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"motan\\\":{\\\"id\\\":\\\"17\\\",\\\"name\\\":\\\"motan\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"oauth2\\\":{\\\"id\\\":\\\"21\\\",\\\"name\\\":\\\"oauth2\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"loggingKafka\\\":{\\\"id\\\":\\\"33\\\",\\\"name\\\":\\\"loggingKafka\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9092\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":180},\\\"casdoor\\\":{\\\"id\\\":\\\"39\\\",\\\"name\\\":\\\"casdoor\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\":\\\\\\\"http://localhost:8000\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"rateLimiter\\\":{\\\"id\\\":\\\"4\\\",\\\"name\\\":\\\"rateLimiter\\\",\\\"config\\\":\\\"{\\\\\\\"master\\\\\\\":\\\\\\\"mymaster\\\\\\\",\\\\\\\"mode\\\\\\\":\\\\\\\"standalone\\\\\\\",\\\\\\\"url\\\\\\\":\\\\\\\"192.168.1.1:6379\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"abc\\\\\\\"}\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":60},\\\"loggingConsole\\\":{\\\"id\\\":\\\"18\\\",\\\"name\\\":\\\"loggingConsole\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":160},\\\"websocket\\\":{\\\"id\\\":\\\"26\\\",\\\"name\\\":\\\"websocket\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"mqtt\\\":{\\\"id\\\":\\\"28\\\",\\\"name\\\":\\\"mqtt\\\",\\\"config\\\":\\\"{\\\\\\\"port\\\\\\\": 9500,\\\\\\\"bossGroupThreadCount\\\\\\\": 1,\\\\\\\"maxPayloadSize\\\\\\\": 65536,\\\\\\\"workerGroupThreadCount\\\\\\\": 12,\\\\\\\"userName\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"password\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"isEncryptPassword\\\\\\\": false,\\\\\\\"encryptMode\\\\\\\": \\\\\\\"\\\\\\\",\\\\\\\"leakDetectorLevel\\\\\\\": \\\\\\\"DISABLED\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":125},\\\"tars\\\":{\\\"id\\\":\\\"13\\\",\\\"name\\\":\\\"tars\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cryptorRequest\\\":{\\\"id\\\":\\\"24\\\",\\\"name\\\":\\\"cryptorRequest\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":100},\\\"divide\\\":{\\\"id\\\":\\\"5\\\",\\\"name\\\":\\\"divide\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"waf\\\":{\\\"id\\\":\\\"2\\\",\\\"name\\\":\\\"waf\\\",\\\"config\\\":\\\"{\\\\\\\"model\\\\\\\":\\\\\\\"black\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":50},\\\"redirect\\\":{\\\"id\\\":\\\"16\\\",\\\"name\\\":\\\"redirect\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":110},\\\"sentinel\\\":{\\\"id\\\":\\\"10\\\",\\\"name\\\":\\\"sentinel\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":140},\\\"hystrix\\\":{\\\"id\\\":\\\"9\\\",\\\"name\\\":\\\"hystrix\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":130},\\\"sofa\\\":{\\\"id\\\":\\\"11\\\",\\\"name\\\":\\\"sofa\\\",\\\"config\\\":\\\"{\\\\\\\"protocol\\\\\\\":\\\\\\\"zookeeper\\\\\\\",\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cache\\\":{\\\"id\\\":\\\"30\\\",\\\"name\\\":\\\"cache\\\",\\\"config\\\":\\\"{\\\\\\\"cacheType\\\\\\\":\\\\\\\"memory\\\\\\\"}\\\",\\\"role\\\":\\\"Cache\\\",\\\"enabled\\\":false,\\\"sort\\\":10},\\\"loggingTencentCls\\\":{\\\"id\\\":\\\"36\\\",\\\"name\\\":\\\"loggingTencentCls\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\": \\\\\\\"ap-guangzhou.cls.tencentcs.com\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":176},\\\"contextPath\\\":{\\\"id\\\":\\\"14\\\",\\\"name\\\":\\\"contextPath\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":true,\\\"sort\\\":80},\\\"generalContext\\\":{\\\"id\\\":\\\"27\\\",\\\"name\\\":\\\"generalContext\\\",\\\"role\\\":\\\"Common\\\",\\\"enabled\\\":true,\\\"sort\\\":125},\\\"brpc\\\":{\\\"id\\\":\\\"41\\\",\\\"name\\\":\\\"brpc\\\",\\\"config\\\":\\\"{\\\\\\\"address\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8005\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"rewrite\\\":{\\\"id\\\":\\\"3\\\",\\\"name\\\":\\\"rewrite\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":90},\\\"springCloud\\\":{\\\"id\\\":\\\"8\\\",\\\"name\\\":\\\"springCloud\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":200},\\\"grpc\\\":{\\\"id\\\":\\\"15\\\",\\\"name\\\":\\\"grpc\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"resilience4j\\\":{\\\"id\\\":\\\"12\\\",\\\"name\\\":\\\"resilience4j\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"loggingClickHouse\\\":{\\\"id\\\":\\\"38\\\",\\\"name\\\":\\\"loggingClickHouse\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8123\\\\\\\",\\\\\\\"database\\\\\\\":\\\\\\\"shenyu-gateway\\\\\\\",\\\\\\\"username\\\\\\\":\\\\\\\"\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":195},\\\"mock\\\":{\\\"id\\\":\\\"31\\\",\\\"name\\\":\\\"mock\\\",\\\"role\\\":\\\"Mock\\\",\\\"enabled\\\":false,\\\"sort\\\":1},\\\"cryptorResponse\\\":{\\\"id\\\":\\\"25\\\",\\\"name\\\":\\\"cryptorResponse\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":410},\\\"keyAuth\\\":{\\\"id\\\":\\\"40\\\",\\\"name\\\":\\\"keyAuth\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":150},\\\"loggingAliyunSls\\\":{\\\"id\\\":\\\"34\\\",\\\"name\\\":\\\"loggingAliyunSls\\\",\\\"config\\\":\\\"{\\\\\\\"projectName\\\\\\\": \\\\\\\"shenyu\\\\\\\", \\\\\\\"logStoreName\\\\\\\": \\\\\\\"shenyu-logstore\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":175},\\\"loggingRocketMQ\\\":{\\\"id\\\":\\\"29\\\",\\\"name\\\":\\\"loggingRocketMQ\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9876\\\\\\\",\\\\\\\"producerGroup\\\\\\\":\\\\\\\"shenyu-plugin-logging-rocketmq\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":170}}\",\"comment\":\"create config data\",\"lineNum\":1,\"id\":2,\"isDeleted\":false,\"deletedAt\":0,\"dataChangeCreatedBy\":\"apollo\",\"dataChangeCreatedTime\":\"2023-06-08 19:02:48\",\"dataChangeLastModifiedBy\":\"apollo\",\"dataChangeLastModifiedTime\":\"2023-06-08 19:02:48\"}],\"updateItems\":[],\"deleteItems\":[]}','shenyu','test','application',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:48','apollo','2023-06-08 11:02:48'),(2,'{\"createItems\":[{\"namespaceId\":3,\"key\":\"shenyu.selector.json\",\"type\":0,\"value\":\"{}\",\"comment\":\"create config data\",\"lineNum\":2,\"id\":3,\"isDeleted\":false,\"deletedAt\":0,\"dataChangeCreatedBy\":\"apollo\",\"dataChangeCreatedTime\":\"2023-06-08 19:02:48\",\"dataChangeLastModifiedBy\":\"apollo\",\"dataChangeLastModifiedTime\":\"2023-06-08 19:02:48\"}],\"updateItems\":[],\"deleteItems\":[]}','shenyu','test','application',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49'),(3,'{\"createItems\":[{\"namespaceId\":3,\"key\":\"shenyu.rule.json\",\"type\":0,\"value\":\"{}\",\"comment\":\"create config data\",\"lineNum\":3,\"id\":4,\"isDeleted\":false,\"deletedAt\":0,\"dataChangeCreatedBy\":\"apollo\",\"dataChangeCreatedTime\":\"2023-06-08 19:02:48\",\"dataChangeLastModifiedBy\":\"apollo\",\"dataChangeLastModifiedTime\":\"2023-06-08 19:02:48\"}],\"updateItems\":[],\"deleteItems\":[]}','shenyu','test','application',NULL,_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49');
/*!40000 ALTER TABLE `Commit` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `GrayReleaseRule`
--

DROP TABLE IF EXISTS `GrayReleaseRule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `GrayReleaseRule` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `AppId` varchar(64) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `ClusterName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'Cluster Name',
  `NamespaceName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'Namespace Name',
  `BranchName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'branch name',
  `Rules` varchar(16000) DEFAULT '[]' COMMENT '灰度规则',
  `ReleaseId` int unsigned NOT NULL DEFAULT '0' COMMENT '灰度对应的release',
  `BranchStatus` tinyint DEFAULT '1' COMMENT '灰度分支状态: 0:删除分支,1:正在使用的规则 2：全量发布',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_Namespace` (`AppId`,`ClusterName`,`NamespaceName`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='灰度规则表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `GrayReleaseRule`
--

LOCK TABLES `GrayReleaseRule` WRITE;
/*!40000 ALTER TABLE `GrayReleaseRule` DISABLE KEYS */;
/*!40000 ALTER TABLE `GrayReleaseRule` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Instance`
--

DROP TABLE IF EXISTS `Instance`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Instance` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `AppId` varchar(64) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `ClusterName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'ClusterName',
  `DataCenter` varchar(64) NOT NULL DEFAULT 'default' COMMENT 'Data Center Name',
  `Ip` varchar(32) NOT NULL DEFAULT '' COMMENT 'instance ip',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `IX_UNIQUE_KEY` (`AppId`,`ClusterName`,`Ip`,`DataCenter`),
  KEY `IX_IP` (`Ip`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='使用配置的应用实例';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Instance`
--

LOCK TABLES `Instance` WRITE;
/*!40000 ALTER TABLE `Instance` DISABLE KEYS */;
/*!40000 ALTER TABLE `Instance` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `InstanceConfig`
--

DROP TABLE IF EXISTS `InstanceConfig`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `InstanceConfig` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `InstanceId` int unsigned DEFAULT NULL COMMENT 'Instance Id',
  `ConfigAppId` varchar(64) NOT NULL DEFAULT 'default' COMMENT 'Config App Id',
  `ConfigClusterName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'Config Cluster Name',
  `ConfigNamespaceName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'Config Namespace Name',
  `ReleaseKey` varchar(64) NOT NULL DEFAULT '' COMMENT '发布的Key',
  `ReleaseDeliveryTime` timestamp NULL DEFAULT NULL COMMENT '配置获取时间',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `IX_UNIQUE_KEY` (`InstanceId`,`ConfigAppId`,`ConfigNamespaceName`),
  KEY `IX_ReleaseKey` (`ReleaseKey`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_Valid_Namespace` (`ConfigAppId`,`ConfigClusterName`,`ConfigNamespaceName`,`DataChange_LastTime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='应用实例的配置信息';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `InstanceConfig`
--

LOCK TABLES `InstanceConfig` WRITE;
/*!40000 ALTER TABLE `InstanceConfig` DISABLE KEYS */;
/*!40000 ALTER TABLE `InstanceConfig` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Item`
--

DROP TABLE IF EXISTS `Item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Item` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `NamespaceId` int unsigned NOT NULL DEFAULT '0' COMMENT '集群NamespaceId',
  `Key` varchar(128) NOT NULL DEFAULT 'default' COMMENT '配置项Key',
  `Type` tinyint unsigned NOT NULL DEFAULT '0' COMMENT '配置项类型，0: String，1: Number，2: Boolean，3: JSON',
  `Value` longtext NOT NULL COMMENT '配置项值',
  `Comment` varchar(1024) DEFAULT '' COMMENT '注释',
  `LineNum` int unsigned DEFAULT '0' COMMENT '行号',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  KEY `IX_GroupId` (`NamespaceId`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='配置项目';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Item`
--

LOCK TABLES `Item` WRITE;
/*!40000 ALTER TABLE `Item` DISABLE KEYS */;
INSERT INTO `Item` VALUES (1,1,'timeout',0,'100','sample timeout配置',1,_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(2,3,'shenyu.plugin.json',0,'{\"request\":{\"id\":\"20\",\"name\":\"request\",\"role\":\"HttpProcess\",\"enabled\":false,\"sort\":120},\"loggingPulsar\":{\"id\":\"35\",\"name\":\"loggingPulsar\",\"config\":\"{\\\"topic\\\":\\\"shenyu-access-logging\\\", \\\"serviceUrl\\\": \\\"pulsar://localhost:6650\\\"}\",\"role\":\"Logging\",\"enabled\":false,\"sort\":185},\"jwt\":{\"id\":\"19\",\"name\":\"jwt\",\"config\":\"{\\\"secretKey\\\":\\\"key\\\"}\",\"role\":\"Authentication\",\"enabled\":false,\"sort\":30},\"paramMapping\":{\"id\":\"22\",\"name\":\"paramMapping\",\"config\":\"{\\\"ruleHandlePageType\\\":\\\"custom\\\"}\",\"role\":\"HttpProcess\",\"enabled\":false,\"sort\":70},\"modifyResponse\":{\"id\":\"23\",\"name\":\"modifyResponse\",\"config\":\"{\\\"ruleHandlePageType\\\":\\\"custom\\\"}\",\"role\":\"HttpProcess\",\"enabled\":false,\"sort\":220},\"sign\":{\"id\":\"1\",\"name\":\"sign\",\"role\":\"Authentication\",\"enabled\":false,\"sort\":20},\"loggingElasticSearch\":{\"id\":\"32\",\"name\":\"loggingElasticSearch\",\"config\":\"{\\\"host\\\":\\\"localhost\\\", \\\"port\\\": \\\"9200\\\"}\",\"role\":\"Logging\",\"enabled\":false,\"sort\":190},\"dubbo\":{\"id\":\"6\",\"name\":\"dubbo\",\"config\":\"{\\\"register\\\":\\\"zookeeper://localhost:2181\\\",\\\"multiSelectorHandle\\\":\\\"1\\\",\\\"threadpool\\\":\\\"shared\\\",\\\"corethreads\\\":0,\\\"threads\\\":2147483647,\\\"queues\\\":0}\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":310},\"motan\":{\"id\":\"17\",\"name\":\"motan\",\"config\":\"{\\\"register\\\":\\\"127.0.0.1:2181\\\",\\\"corethreads\\\":0,\\\"threads\\\":2147483647,\\\"queues\\\":0,\\\"threadpool\\\":\\\"shared\\\"}\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":310},\"oauth2\":{\"id\":\"21\",\"name\":\"oauth2\",\"role\":\"Authentication\",\"enabled\":false,\"sort\":40},\"loggingKafka\":{\"id\":\"33\",\"name\":\"loggingKafka\",\"config\":\"{\\\"topic\\\":\\\"shenyu-access-logging\\\", \\\"namesrvAddr\\\": \\\"localhost:9092\\\"}\",\"role\":\"Logging\",\"enabled\":false,\"sort\":180},\"casdoor\":{\"id\":\"39\",\"name\":\"casdoor\",\"config\":\"{\\\"endpoint\\\":\\\"http://localhost:8000\\\"}\",\"role\":\"Authentication\",\"enabled\":false,\"sort\":40},\"rateLimiter\":{\"id\":\"4\",\"name\":\"rateLimiter\",\"config\":\"{\\\"master\\\":\\\"mymaster\\\",\\\"mode\\\":\\\"standalone\\\",\\\"url\\\":\\\"192.168.1.1:6379\\\",\\\"password\\\":\\\"abc\\\"}\",\"role\":\"FaultTolerance\",\"enabled\":false,\"sort\":60},\"loggingConsole\":{\"id\":\"18\",\"name\":\"loggingConsole\",\"role\":\"Logging\",\"enabled\":false,\"sort\":160},\"websocket\":{\"id\":\"26\",\"name\":\"websocket\",\"config\":\"{\\\"multiSelectorHandle\\\":\\\"1\\\"}\",\"role\":\"Proxy\",\"enabled\":true,\"sort\":200},\"mqtt\":{\"id\":\"28\",\"name\":\"mqtt\",\"config\":\"{\\\"port\\\": 9500,\\\"bossGroupThreadCount\\\": 1,\\\"maxPayloadSize\\\": 65536,\\\"workerGroupThreadCount\\\": 12,\\\"userName\\\": \\\"shenyu\\\",\\\"password\\\": \\\"shenyu\\\",\\\"isEncryptPassword\\\": false,\\\"encryptMode\\\": \\\"\\\",\\\"leakDetectorLevel\\\": \\\"DISABLED\\\"}\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":125},\"tars\":{\"id\":\"13\",\"name\":\"tars\",\"config\":\"{\\\"multiSelectorHandle\\\":\\\"1\\\",\\\"multiRuleHandle\\\":\\\"0\\\",\\\"threadpool\\\":\\\"shared\\\"}\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":310},\"cryptorRequest\":{\"id\":\"24\",\"name\":\"cryptorRequest\",\"role\":\"Cryptor\",\"enabled\":true,\"sort\":100},\"divide\":{\"id\":\"5\",\"name\":\"divide\",\"config\":\"{\\\"multiSelectorHandle\\\":\\\"1\\\",\\\"multiRuleHandle\\\":\\\"0\\\"}\",\"role\":\"Proxy\",\"enabled\":true,\"sort\":200},\"waf\":{\"id\":\"2\",\"name\":\"waf\",\"config\":\"{\\\"model\\\":\\\"black\\\"}\",\"role\":\"Authentication\",\"enabled\":false,\"sort\":50},\"redirect\":{\"id\":\"16\",\"name\":\"redirect\",\"role\":\"HttpProcess\",\"enabled\":false,\"sort\":110},\"sentinel\":{\"id\":\"10\",\"name\":\"sentinel\",\"role\":\"FaultTolerance\",\"enabled\":false,\"sort\":140},\"hystrix\":{\"id\":\"9\",\"name\":\"hystrix\",\"role\":\"FaultTolerance\",\"enabled\":false,\"sort\":130},\"sofa\":{\"id\":\"11\",\"name\":\"sofa\",\"config\":\"{\\\"protocol\\\":\\\"zookeeper\\\",\\\"register\\\":\\\"127.0.0.1:2181\\\",\\\"threadpool\\\":\\\"shared\\\"}\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":310},\"cache\":{\"id\":\"30\",\"name\":\"cache\",\"config\":\"{\\\"cacheType\\\":\\\"memory\\\"}\",\"role\":\"Cache\",\"enabled\":false,\"sort\":10},\"loggingTencentCls\":{\"id\":\"36\",\"name\":\"loggingTencentCls\",\"config\":\"{\\\"endpoint\\\": \\\"ap-guangzhou.cls.tencentcs.com\\\", \\\"topic\\\": \\\"shenyu-topic\\\"}\",\"role\":\"Logging\",\"enabled\":false,\"sort\":176},\"contextPath\":{\"id\":\"14\",\"name\":\"contextPath\",\"role\":\"HttpProcess\",\"enabled\":true,\"sort\":80},\"generalContext\":{\"id\":\"27\",\"name\":\"generalContext\",\"role\":\"Common\",\"enabled\":true,\"sort\":125},\"brpc\":{\"id\":\"41\",\"name\":\"brpc\",\"config\":\"{\\\"address\\\":\\\"127.0.0.1\\\",\\\"port\\\":\\\"8005\\\",\\\"corethreads\\\":0,\\\"threads\\\":2147483647,\\\"queues\\\":0,\\\"threadpool\\\":\\\"shared\\\"}\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":310},\"rewrite\":{\"id\":\"3\",\"name\":\"rewrite\",\"role\":\"HttpProcess\",\"enabled\":false,\"sort\":90},\"springCloud\":{\"id\":\"8\",\"name\":\"springCloud\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":200},\"grpc\":{\"id\":\"15\",\"name\":\"grpc\",\"config\":\"{\\\"multiSelectorHandle\\\":\\\"1\\\",\\\"multiRuleHandle\\\":\\\"0\\\",\\\"threadpool\\\":\\\"shared\\\"}\",\"role\":\"Proxy\",\"enabled\":false,\"sort\":310},\"resilience4j\":{\"id\":\"12\",\"name\":\"resilience4j\",\"role\":\"FaultTolerance\",\"enabled\":false,\"sort\":310},\"loggingClickHouse\":{\"id\":\"38\",\"name\":\"loggingClickHouse\",\"config\":\"{\\\"host\\\":\\\"127.0.0.1\\\",\\\"port\\\":\\\"8123\\\",\\\"database\\\":\\\"shenyu-gateway\\\",\\\"username\\\":\\\"\\\",\\\"password\\\":\\\"\\\"}\",\"role\":\"Logging\",\"enabled\":false,\"sort\":195},\"mock\":{\"id\":\"31\",\"name\":\"mock\",\"role\":\"Mock\",\"enabled\":false,\"sort\":1},\"cryptorResponse\":{\"id\":\"25\",\"name\":\"cryptorResponse\",\"role\":\"Cryptor\",\"enabled\":true,\"sort\":410},\"keyAuth\":{\"id\":\"40\",\"name\":\"keyAuth\",\"role\":\"Authentication\",\"enabled\":false,\"sort\":150},\"loggingAliyunSls\":{\"id\":\"34\",\"name\":\"loggingAliyunSls\",\"config\":\"{\\\"projectName\\\": \\\"shenyu\\\", \\\"logStoreName\\\": \\\"shenyu-logstore\\\", \\\"topic\\\": \\\"shenyu-topic\\\"}\",\"role\":\"Logging\",\"enabled\":false,\"sort\":175},\"loggingRocketMQ\":{\"id\":\"29\",\"name\":\"loggingRocketMQ\",\"config\":\"{\\\"topic\\\":\\\"shenyu-access-logging\\\", \\\"namesrvAddr\\\": \\\"localhost:9876\\\",\\\"producerGroup\\\":\\\"shenyu-plugin-logging-rocketmq\\\"}\",\"role\":\"Logging\",\"enabled\":false,\"sort\":170}}','create config data',1,_binary '\0',0,'apollo','2023-06-08 11:02:48','apollo','2023-06-08 11:02:48'),(3,3,'shenyu.selector.json',0,'{}','create config data',2,_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49'),(4,3,'shenyu.rule.json',0,'{}','create config data',3,_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49');
/*!40000 ALTER TABLE `Item` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Namespace`
--

DROP TABLE IF EXISTS `Namespace`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Namespace` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `AppId` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `ClusterName` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'Cluster Name',
  `NamespaceName` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'Namespace Name',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_AppId_ClusterName_NamespaceName_DeletedAt` (`AppId`(191),`ClusterName`(191),`NamespaceName`(191),`DeletedAt`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_NamespaceName` (`NamespaceName`(191))
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='命名空间';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Namespace`
--

LOCK TABLES `Namespace` WRITE;
/*!40000 ALTER TABLE `Namespace` DISABLE KEYS */;
INSERT INTO `Namespace` VALUES (1,'SampleApp','default','application',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(2,'shenyu','default','application',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(3,'shenyu','test','application',_binary '\0',0,'apollo','2023-06-08 09:27:01','apollo','2023-06-08 09:27:01');
/*!40000 ALTER TABLE `Namespace` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `NamespaceLock`
--

DROP TABLE IF EXISTS `NamespaceLock`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `NamespaceLock` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `NamespaceId` int unsigned NOT NULL DEFAULT '0' COMMENT '集群NamespaceId',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  `IsDeleted` bit(1) DEFAULT b'0' COMMENT '软删除',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_NamespaceId_DeletedAt` (`NamespaceId`,`DeletedAt`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='namespace的编辑锁';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `NamespaceLock`
--

LOCK TABLES `NamespaceLock` WRITE;
/*!40000 ALTER TABLE `NamespaceLock` DISABLE KEYS */;
/*!40000 ALTER TABLE `NamespaceLock` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Release`
--

DROP TABLE IF EXISTS `Release`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Release` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `ReleaseKey` varchar(64) NOT NULL DEFAULT '' COMMENT '发布的Key',
  `Name` varchar(64) NOT NULL DEFAULT 'default' COMMENT '发布名字',
  `Comment` varchar(256) DEFAULT NULL COMMENT '发布说明',
  `AppId` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `ClusterName` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'ClusterName',
  `NamespaceName` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'namespaceName',
  `Configurations` longtext NOT NULL COMMENT '发布配置',
  `IsAbandoned` bit(1) NOT NULL DEFAULT b'0' COMMENT '是否废弃',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_ReleaseKey_DeletedAt` (`ReleaseKey`,`DeletedAt`),
  KEY `AppId_ClusterName_GroupName` (`AppId`(191),`ClusterName`(191),`NamespaceName`(191)),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='发布';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Release`
--

LOCK TABLES `Release` WRITE;
/*!40000 ALTER TABLE `Release` DISABLE KEYS */;
INSERT INTO `Release` VALUES (1,'20161009155425-d3a0749c6e20bc15','20161009155424-release','Sample发布','SampleApp','default','application','{\"timeout\":\"100\"}',_binary '\0',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(2,'20230608190248-4a371fc6c67a5bd9','publish config data','','shenyu','test','application','{\"shenyu.plugin.json\":\"{\\\"request\\\":{\\\"id\\\":\\\"20\\\",\\\"name\\\":\\\"request\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":120},\\\"loggingPulsar\\\":{\\\"id\\\":\\\"35\\\",\\\"name\\\":\\\"loggingPulsar\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"serviceUrl\\\\\\\": \\\\\\\"pulsar://localhost:6650\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":185},\\\"jwt\\\":{\\\"id\\\":\\\"19\\\",\\\"name\\\":\\\"jwt\\\",\\\"config\\\":\\\"{\\\\\\\"secretKey\\\\\\\":\\\\\\\"key\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":30},\\\"paramMapping\\\":{\\\"id\\\":\\\"22\\\",\\\"name\\\":\\\"paramMapping\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":70},\\\"modifyResponse\\\":{\\\"id\\\":\\\"23\\\",\\\"name\\\":\\\"modifyResponse\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":220},\\\"sign\\\":{\\\"id\\\":\\\"1\\\",\\\"name\\\":\\\"sign\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":20},\\\"loggingElasticSearch\\\":{\\\"id\\\":\\\"32\\\",\\\"name\\\":\\\"loggingElasticSearch\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"localhost\\\\\\\", \\\\\\\"port\\\\\\\": \\\\\\\"9200\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":190},\\\"dubbo\\\":{\\\"id\\\":\\\"6\\\",\\\"name\\\":\\\"dubbo\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"zookeeper://localhost:2181\\\\\\\",\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"motan\\\":{\\\"id\\\":\\\"17\\\",\\\"name\\\":\\\"motan\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"oauth2\\\":{\\\"id\\\":\\\"21\\\",\\\"name\\\":\\\"oauth2\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"loggingKafka\\\":{\\\"id\\\":\\\"33\\\",\\\"name\\\":\\\"loggingKafka\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9092\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":180},\\\"casdoor\\\":{\\\"id\\\":\\\"39\\\",\\\"name\\\":\\\"casdoor\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\":\\\\\\\"http://localhost:8000\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"rateLimiter\\\":{\\\"id\\\":\\\"4\\\",\\\"name\\\":\\\"rateLimiter\\\",\\\"config\\\":\\\"{\\\\\\\"master\\\\\\\":\\\\\\\"mymaster\\\\\\\",\\\\\\\"mode\\\\\\\":\\\\\\\"standalone\\\\\\\",\\\\\\\"url\\\\\\\":\\\\\\\"192.168.1.1:6379\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"abc\\\\\\\"}\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":60},\\\"loggingConsole\\\":{\\\"id\\\":\\\"18\\\",\\\"name\\\":\\\"loggingConsole\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":160},\\\"websocket\\\":{\\\"id\\\":\\\"26\\\",\\\"name\\\":\\\"websocket\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"mqtt\\\":{\\\"id\\\":\\\"28\\\",\\\"name\\\":\\\"mqtt\\\",\\\"config\\\":\\\"{\\\\\\\"port\\\\\\\": 9500,\\\\\\\"bossGroupThreadCount\\\\\\\": 1,\\\\\\\"maxPayloadSize\\\\\\\": 65536,\\\\\\\"workerGroupThreadCount\\\\\\\": 12,\\\\\\\"userName\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"password\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"isEncryptPassword\\\\\\\": false,\\\\\\\"encryptMode\\\\\\\": \\\\\\\"\\\\\\\",\\\\\\\"leakDetectorLevel\\\\\\\": \\\\\\\"DISABLED\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":125},\\\"tars\\\":{\\\"id\\\":\\\"13\\\",\\\"name\\\":\\\"tars\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cryptorRequest\\\":{\\\"id\\\":\\\"24\\\",\\\"name\\\":\\\"cryptorRequest\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":100},\\\"divide\\\":{\\\"id\\\":\\\"5\\\",\\\"name\\\":\\\"divide\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"waf\\\":{\\\"id\\\":\\\"2\\\",\\\"name\\\":\\\"waf\\\",\\\"config\\\":\\\"{\\\\\\\"model\\\\\\\":\\\\\\\"black\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":50},\\\"redirect\\\":{\\\"id\\\":\\\"16\\\",\\\"name\\\":\\\"redirect\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":110},\\\"sentinel\\\":{\\\"id\\\":\\\"10\\\",\\\"name\\\":\\\"sentinel\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":140},\\\"hystrix\\\":{\\\"id\\\":\\\"9\\\",\\\"name\\\":\\\"hystrix\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":130},\\\"sofa\\\":{\\\"id\\\":\\\"11\\\",\\\"name\\\":\\\"sofa\\\",\\\"config\\\":\\\"{\\\\\\\"protocol\\\\\\\":\\\\\\\"zookeeper\\\\\\\",\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cache\\\":{\\\"id\\\":\\\"30\\\",\\\"name\\\":\\\"cache\\\",\\\"config\\\":\\\"{\\\\\\\"cacheType\\\\\\\":\\\\\\\"memory\\\\\\\"}\\\",\\\"role\\\":\\\"Cache\\\",\\\"enabled\\\":false,\\\"sort\\\":10},\\\"loggingTencentCls\\\":{\\\"id\\\":\\\"36\\\",\\\"name\\\":\\\"loggingTencentCls\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\": \\\\\\\"ap-guangzhou.cls.tencentcs.com\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":176},\\\"contextPath\\\":{\\\"id\\\":\\\"14\\\",\\\"name\\\":\\\"contextPath\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":true,\\\"sort\\\":80},\\\"generalContext\\\":{\\\"id\\\":\\\"27\\\",\\\"name\\\":\\\"generalContext\\\",\\\"role\\\":\\\"Common\\\",\\\"enabled\\\":true,\\\"sort\\\":125},\\\"brpc\\\":{\\\"id\\\":\\\"41\\\",\\\"name\\\":\\\"brpc\\\",\\\"config\\\":\\\"{\\\\\\\"address\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8005\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"rewrite\\\":{\\\"id\\\":\\\"3\\\",\\\"name\\\":\\\"rewrite\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":90},\\\"springCloud\\\":{\\\"id\\\":\\\"8\\\",\\\"name\\\":\\\"springCloud\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":200},\\\"grpc\\\":{\\\"id\\\":\\\"15\\\",\\\"name\\\":\\\"grpc\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"resilience4j\\\":{\\\"id\\\":\\\"12\\\",\\\"name\\\":\\\"resilience4j\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"loggingClickHouse\\\":{\\\"id\\\":\\\"38\\\",\\\"name\\\":\\\"loggingClickHouse\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8123\\\\\\\",\\\\\\\"database\\\\\\\":\\\\\\\"shenyu-gateway\\\\\\\",\\\\\\\"username\\\\\\\":\\\\\\\"\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":195},\\\"mock\\\":{\\\"id\\\":\\\"31\\\",\\\"name\\\":\\\"mock\\\",\\\"role\\\":\\\"Mock\\\",\\\"enabled\\\":false,\\\"sort\\\":1},\\\"cryptorResponse\\\":{\\\"id\\\":\\\"25\\\",\\\"name\\\":\\\"cryptorResponse\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":410},\\\"keyAuth\\\":{\\\"id\\\":\\\"40\\\",\\\"name\\\":\\\"keyAuth\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":150},\\\"loggingAliyunSls\\\":{\\\"id\\\":\\\"34\\\",\\\"name\\\":\\\"loggingAliyunSls\\\",\\\"config\\\":\\\"{\\\\\\\"projectName\\\\\\\": \\\\\\\"shenyu\\\\\\\", \\\\\\\"logStoreName\\\\\\\": \\\\\\\"shenyu-logstore\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":175},\\\"loggingRocketMQ\\\":{\\\"id\\\":\\\"29\\\",\\\"name\\\":\\\"loggingRocketMQ\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9876\\\\\\\",\\\\\\\"producerGroup\\\\\\\":\\\\\\\"shenyu-plugin-logging-rocketmq\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":170}}\"}',_binary '\0',_binary '\0',0,'apollo','2023-06-08 11:02:48','apollo','2023-06-08 11:02:48'),(3,'20230608190248-4a371fc6c67a5bda','publish config data','','shenyu','test','application','{\"shenyu.plugin.json\":\"{\\\"request\\\":{\\\"id\\\":\\\"20\\\",\\\"name\\\":\\\"request\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":120},\\\"loggingPulsar\\\":{\\\"id\\\":\\\"35\\\",\\\"name\\\":\\\"loggingPulsar\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"serviceUrl\\\\\\\": \\\\\\\"pulsar://localhost:6650\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":185},\\\"jwt\\\":{\\\"id\\\":\\\"19\\\",\\\"name\\\":\\\"jwt\\\",\\\"config\\\":\\\"{\\\\\\\"secretKey\\\\\\\":\\\\\\\"key\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":30},\\\"paramMapping\\\":{\\\"id\\\":\\\"22\\\",\\\"name\\\":\\\"paramMapping\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":70},\\\"modifyResponse\\\":{\\\"id\\\":\\\"23\\\",\\\"name\\\":\\\"modifyResponse\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":220},\\\"sign\\\":{\\\"id\\\":\\\"1\\\",\\\"name\\\":\\\"sign\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":20},\\\"loggingElasticSearch\\\":{\\\"id\\\":\\\"32\\\",\\\"name\\\":\\\"loggingElasticSearch\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"localhost\\\\\\\", \\\\\\\"port\\\\\\\": \\\\\\\"9200\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":190},\\\"dubbo\\\":{\\\"id\\\":\\\"6\\\",\\\"name\\\":\\\"dubbo\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"zookeeper://localhost:2181\\\\\\\",\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"motan\\\":{\\\"id\\\":\\\"17\\\",\\\"name\\\":\\\"motan\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"oauth2\\\":{\\\"id\\\":\\\"21\\\",\\\"name\\\":\\\"oauth2\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"loggingKafka\\\":{\\\"id\\\":\\\"33\\\",\\\"name\\\":\\\"loggingKafka\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9092\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":180},\\\"casdoor\\\":{\\\"id\\\":\\\"39\\\",\\\"name\\\":\\\"casdoor\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\":\\\\\\\"http://localhost:8000\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"rateLimiter\\\":{\\\"id\\\":\\\"4\\\",\\\"name\\\":\\\"rateLimiter\\\",\\\"config\\\":\\\"{\\\\\\\"master\\\\\\\":\\\\\\\"mymaster\\\\\\\",\\\\\\\"mode\\\\\\\":\\\\\\\"standalone\\\\\\\",\\\\\\\"url\\\\\\\":\\\\\\\"192.168.1.1:6379\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"abc\\\\\\\"}\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":60},\\\"loggingConsole\\\":{\\\"id\\\":\\\"18\\\",\\\"name\\\":\\\"loggingConsole\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":160},\\\"websocket\\\":{\\\"id\\\":\\\"26\\\",\\\"name\\\":\\\"websocket\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"mqtt\\\":{\\\"id\\\":\\\"28\\\",\\\"name\\\":\\\"mqtt\\\",\\\"config\\\":\\\"{\\\\\\\"port\\\\\\\": 9500,\\\\\\\"bossGroupThreadCount\\\\\\\": 1,\\\\\\\"maxPayloadSize\\\\\\\": 65536,\\\\\\\"workerGroupThreadCount\\\\\\\": 12,\\\\\\\"userName\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"password\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"isEncryptPassword\\\\\\\": false,\\\\\\\"encryptMode\\\\\\\": \\\\\\\"\\\\\\\",\\\\\\\"leakDetectorLevel\\\\\\\": \\\\\\\"DISABLED\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":125},\\\"tars\\\":{\\\"id\\\":\\\"13\\\",\\\"name\\\":\\\"tars\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cryptorRequest\\\":{\\\"id\\\":\\\"24\\\",\\\"name\\\":\\\"cryptorRequest\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":100},\\\"divide\\\":{\\\"id\\\":\\\"5\\\",\\\"name\\\":\\\"divide\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"waf\\\":{\\\"id\\\":\\\"2\\\",\\\"name\\\":\\\"waf\\\",\\\"config\\\":\\\"{\\\\\\\"model\\\\\\\":\\\\\\\"black\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":50},\\\"redirect\\\":{\\\"id\\\":\\\"16\\\",\\\"name\\\":\\\"redirect\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":110},\\\"sentinel\\\":{\\\"id\\\":\\\"10\\\",\\\"name\\\":\\\"sentinel\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":140},\\\"hystrix\\\":{\\\"id\\\":\\\"9\\\",\\\"name\\\":\\\"hystrix\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":130},\\\"sofa\\\":{\\\"id\\\":\\\"11\\\",\\\"name\\\":\\\"sofa\\\",\\\"config\\\":\\\"{\\\\\\\"protocol\\\\\\\":\\\\\\\"zookeeper\\\\\\\",\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cache\\\":{\\\"id\\\":\\\"30\\\",\\\"name\\\":\\\"cache\\\",\\\"config\\\":\\\"{\\\\\\\"cacheType\\\\\\\":\\\\\\\"memory\\\\\\\"}\\\",\\\"role\\\":\\\"Cache\\\",\\\"enabled\\\":false,\\\"sort\\\":10},\\\"loggingTencentCls\\\":{\\\"id\\\":\\\"36\\\",\\\"name\\\":\\\"loggingTencentCls\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\": \\\\\\\"ap-guangzhou.cls.tencentcs.com\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":176},\\\"contextPath\\\":{\\\"id\\\":\\\"14\\\",\\\"name\\\":\\\"contextPath\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":true,\\\"sort\\\":80},\\\"generalContext\\\":{\\\"id\\\":\\\"27\\\",\\\"name\\\":\\\"generalContext\\\",\\\"role\\\":\\\"Common\\\",\\\"enabled\\\":true,\\\"sort\\\":125},\\\"brpc\\\":{\\\"id\\\":\\\"41\\\",\\\"name\\\":\\\"brpc\\\",\\\"config\\\":\\\"{\\\\\\\"address\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8005\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"rewrite\\\":{\\\"id\\\":\\\"3\\\",\\\"name\\\":\\\"rewrite\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":90},\\\"springCloud\\\":{\\\"id\\\":\\\"8\\\",\\\"name\\\":\\\"springCloud\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":200},\\\"grpc\\\":{\\\"id\\\":\\\"15\\\",\\\"name\\\":\\\"grpc\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"resilience4j\\\":{\\\"id\\\":\\\"12\\\",\\\"name\\\":\\\"resilience4j\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"loggingClickHouse\\\":{\\\"id\\\":\\\"38\\\",\\\"name\\\":\\\"loggingClickHouse\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8123\\\\\\\",\\\\\\\"database\\\\\\\":\\\\\\\"shenyu-gateway\\\\\\\",\\\\\\\"username\\\\\\\":\\\\\\\"\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":195},\\\"mock\\\":{\\\"id\\\":\\\"31\\\",\\\"name\\\":\\\"mock\\\",\\\"role\\\":\\\"Mock\\\",\\\"enabled\\\":false,\\\"sort\\\":1},\\\"cryptorResponse\\\":{\\\"id\\\":\\\"25\\\",\\\"name\\\":\\\"cryptorResponse\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":410},\\\"keyAuth\\\":{\\\"id\\\":\\\"40\\\",\\\"name\\\":\\\"keyAuth\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":150},\\\"loggingAliyunSls\\\":{\\\"id\\\":\\\"34\\\",\\\"name\\\":\\\"loggingAliyunSls\\\",\\\"config\\\":\\\"{\\\\\\\"projectName\\\\\\\": \\\\\\\"shenyu\\\\\\\", \\\\\\\"logStoreName\\\\\\\": \\\\\\\"shenyu-logstore\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":175},\\\"loggingRocketMQ\\\":{\\\"id\\\":\\\"29\\\",\\\"name\\\":\\\"loggingRocketMQ\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9876\\\\\\\",\\\\\\\"producerGroup\\\\\\\":\\\\\\\"shenyu-plugin-logging-rocketmq\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":170}}\",\"shenyu.selector.json\":\"{}\"}',_binary '\0',_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49'),(4,'20230608190248-4a371fc6c67a5bdb','publish config data','','shenyu','test','application','{\"shenyu.plugin.json\":\"{\\\"request\\\":{\\\"id\\\":\\\"20\\\",\\\"name\\\":\\\"request\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":120},\\\"loggingPulsar\\\":{\\\"id\\\":\\\"35\\\",\\\"name\\\":\\\"loggingPulsar\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"serviceUrl\\\\\\\": \\\\\\\"pulsar://localhost:6650\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":185},\\\"jwt\\\":{\\\"id\\\":\\\"19\\\",\\\"name\\\":\\\"jwt\\\",\\\"config\\\":\\\"{\\\\\\\"secretKey\\\\\\\":\\\\\\\"key\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":30},\\\"paramMapping\\\":{\\\"id\\\":\\\"22\\\",\\\"name\\\":\\\"paramMapping\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":70},\\\"modifyResponse\\\":{\\\"id\\\":\\\"23\\\",\\\"name\\\":\\\"modifyResponse\\\",\\\"config\\\":\\\"{\\\\\\\"ruleHandlePageType\\\\\\\":\\\\\\\"custom\\\\\\\"}\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":220},\\\"sign\\\":{\\\"id\\\":\\\"1\\\",\\\"name\\\":\\\"sign\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":20},\\\"loggingElasticSearch\\\":{\\\"id\\\":\\\"32\\\",\\\"name\\\":\\\"loggingElasticSearch\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"localhost\\\\\\\", \\\\\\\"port\\\\\\\": \\\\\\\"9200\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":190},\\\"dubbo\\\":{\\\"id\\\":\\\"6\\\",\\\"name\\\":\\\"dubbo\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"zookeeper://localhost:2181\\\\\\\",\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"motan\\\":{\\\"id\\\":\\\"17\\\",\\\"name\\\":\\\"motan\\\",\\\"config\\\":\\\"{\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"oauth2\\\":{\\\"id\\\":\\\"21\\\",\\\"name\\\":\\\"oauth2\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"loggingKafka\\\":{\\\"id\\\":\\\"33\\\",\\\"name\\\":\\\"loggingKafka\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9092\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":180},\\\"casdoor\\\":{\\\"id\\\":\\\"39\\\",\\\"name\\\":\\\"casdoor\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\":\\\\\\\"http://localhost:8000\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":40},\\\"rateLimiter\\\":{\\\"id\\\":\\\"4\\\",\\\"name\\\":\\\"rateLimiter\\\",\\\"config\\\":\\\"{\\\\\\\"master\\\\\\\":\\\\\\\"mymaster\\\\\\\",\\\\\\\"mode\\\\\\\":\\\\\\\"standalone\\\\\\\",\\\\\\\"url\\\\\\\":\\\\\\\"192.168.1.1:6379\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"abc\\\\\\\"}\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":60},\\\"loggingConsole\\\":{\\\"id\\\":\\\"18\\\",\\\"name\\\":\\\"loggingConsole\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":160},\\\"websocket\\\":{\\\"id\\\":\\\"26\\\",\\\"name\\\":\\\"websocket\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"mqtt\\\":{\\\"id\\\":\\\"28\\\",\\\"name\\\":\\\"mqtt\\\",\\\"config\\\":\\\"{\\\\\\\"port\\\\\\\": 9500,\\\\\\\"bossGroupThreadCount\\\\\\\": 1,\\\\\\\"maxPayloadSize\\\\\\\": 65536,\\\\\\\"workerGroupThreadCount\\\\\\\": 12,\\\\\\\"userName\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"password\\\\\\\": \\\\\\\"shenyu\\\\\\\",\\\\\\\"isEncryptPassword\\\\\\\": false,\\\\\\\"encryptMode\\\\\\\": \\\\\\\"\\\\\\\",\\\\\\\"leakDetectorLevel\\\\\\\": \\\\\\\"DISABLED\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":125},\\\"tars\\\":{\\\"id\\\":\\\"13\\\",\\\"name\\\":\\\"tars\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cryptorRequest\\\":{\\\"id\\\":\\\"24\\\",\\\"name\\\":\\\"cryptorRequest\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":100},\\\"divide\\\":{\\\"id\\\":\\\"5\\\",\\\"name\\\":\\\"divide\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":true,\\\"sort\\\":200},\\\"waf\\\":{\\\"id\\\":\\\"2\\\",\\\"name\\\":\\\"waf\\\",\\\"config\\\":\\\"{\\\\\\\"model\\\\\\\":\\\\\\\"black\\\\\\\"}\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":50},\\\"redirect\\\":{\\\"id\\\":\\\"16\\\",\\\"name\\\":\\\"redirect\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":110},\\\"sentinel\\\":{\\\"id\\\":\\\"10\\\",\\\"name\\\":\\\"sentinel\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":140},\\\"hystrix\\\":{\\\"id\\\":\\\"9\\\",\\\"name\\\":\\\"hystrix\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":130},\\\"sofa\\\":{\\\"id\\\":\\\"11\\\",\\\"name\\\":\\\"sofa\\\",\\\"config\\\":\\\"{\\\\\\\"protocol\\\\\\\":\\\\\\\"zookeeper\\\\\\\",\\\\\\\"register\\\\\\\":\\\\\\\"127.0.0.1:2181\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"cache\\\":{\\\"id\\\":\\\"30\\\",\\\"name\\\":\\\"cache\\\",\\\"config\\\":\\\"{\\\\\\\"cacheType\\\\\\\":\\\\\\\"memory\\\\\\\"}\\\",\\\"role\\\":\\\"Cache\\\",\\\"enabled\\\":false,\\\"sort\\\":10},\\\"loggingTencentCls\\\":{\\\"id\\\":\\\"36\\\",\\\"name\\\":\\\"loggingTencentCls\\\",\\\"config\\\":\\\"{\\\\\\\"endpoint\\\\\\\": \\\\\\\"ap-guangzhou.cls.tencentcs.com\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":176},\\\"contextPath\\\":{\\\"id\\\":\\\"14\\\",\\\"name\\\":\\\"contextPath\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":true,\\\"sort\\\":80},\\\"generalContext\\\":{\\\"id\\\":\\\"27\\\",\\\"name\\\":\\\"generalContext\\\",\\\"role\\\":\\\"Common\\\",\\\"enabled\\\":true,\\\"sort\\\":125},\\\"brpc\\\":{\\\"id\\\":\\\"41\\\",\\\"name\\\":\\\"brpc\\\",\\\"config\\\":\\\"{\\\\\\\"address\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8005\\\\\\\",\\\\\\\"corethreads\\\\\\\":0,\\\\\\\"threads\\\\\\\":2147483647,\\\\\\\"queues\\\\\\\":0,\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"rewrite\\\":{\\\"id\\\":\\\"3\\\",\\\"name\\\":\\\"rewrite\\\",\\\"role\\\":\\\"HttpProcess\\\",\\\"enabled\\\":false,\\\"sort\\\":90},\\\"springCloud\\\":{\\\"id\\\":\\\"8\\\",\\\"name\\\":\\\"springCloud\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":200},\\\"grpc\\\":{\\\"id\\\":\\\"15\\\",\\\"name\\\":\\\"grpc\\\",\\\"config\\\":\\\"{\\\\\\\"multiSelectorHandle\\\\\\\":\\\\\\\"1\\\\\\\",\\\\\\\"multiRuleHandle\\\\\\\":\\\\\\\"0\\\\\\\",\\\\\\\"threadpool\\\\\\\":\\\\\\\"shared\\\\\\\"}\\\",\\\"role\\\":\\\"Proxy\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"resilience4j\\\":{\\\"id\\\":\\\"12\\\",\\\"name\\\":\\\"resilience4j\\\",\\\"role\\\":\\\"FaultTolerance\\\",\\\"enabled\\\":false,\\\"sort\\\":310},\\\"loggingClickHouse\\\":{\\\"id\\\":\\\"38\\\",\\\"name\\\":\\\"loggingClickHouse\\\",\\\"config\\\":\\\"{\\\\\\\"host\\\\\\\":\\\\\\\"127.0.0.1\\\\\\\",\\\\\\\"port\\\\\\\":\\\\\\\"8123\\\\\\\",\\\\\\\"database\\\\\\\":\\\\\\\"shenyu-gateway\\\\\\\",\\\\\\\"username\\\\\\\":\\\\\\\"\\\\\\\",\\\\\\\"password\\\\\\\":\\\\\\\"\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":195},\\\"mock\\\":{\\\"id\\\":\\\"31\\\",\\\"name\\\":\\\"mock\\\",\\\"role\\\":\\\"Mock\\\",\\\"enabled\\\":false,\\\"sort\\\":1},\\\"cryptorResponse\\\":{\\\"id\\\":\\\"25\\\",\\\"name\\\":\\\"cryptorResponse\\\",\\\"role\\\":\\\"Cryptor\\\",\\\"enabled\\\":true,\\\"sort\\\":410},\\\"keyAuth\\\":{\\\"id\\\":\\\"40\\\",\\\"name\\\":\\\"keyAuth\\\",\\\"role\\\":\\\"Authentication\\\",\\\"enabled\\\":false,\\\"sort\\\":150},\\\"loggingAliyunSls\\\":{\\\"id\\\":\\\"34\\\",\\\"name\\\":\\\"loggingAliyunSls\\\",\\\"config\\\":\\\"{\\\\\\\"projectName\\\\\\\": \\\\\\\"shenyu\\\\\\\", \\\\\\\"logStoreName\\\\\\\": \\\\\\\"shenyu-logstore\\\\\\\", \\\\\\\"topic\\\\\\\": \\\\\\\"shenyu-topic\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":175},\\\"loggingRocketMQ\\\":{\\\"id\\\":\\\"29\\\",\\\"name\\\":\\\"loggingRocketMQ\\\",\\\"config\\\":\\\"{\\\\\\\"topic\\\\\\\":\\\\\\\"shenyu-access-logging\\\\\\\", \\\\\\\"namesrvAddr\\\\\\\": \\\\\\\"localhost:9876\\\\\\\",\\\\\\\"producerGroup\\\\\\\":\\\\\\\"shenyu-plugin-logging-rocketmq\\\\\\\"}\\\",\\\"role\\\":\\\"Logging\\\",\\\"enabled\\\":false,\\\"sort\\\":170}}\",\"shenyu.selector.json\":\"{}\",\"shenyu.rule.json\":\"{}\"}',_binary '\0',_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49');
/*!40000 ALTER TABLE `Release` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ReleaseHistory`
--

DROP TABLE IF EXISTS `ReleaseHistory`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ReleaseHistory` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `AppId` varchar(64) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `ClusterName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'ClusterName',
  `NamespaceName` varchar(32) NOT NULL DEFAULT 'default' COMMENT 'namespaceName',
  `BranchName` varchar(32) NOT NULL DEFAULT 'default' COMMENT '发布分支名',
  `ReleaseId` int unsigned NOT NULL DEFAULT '0' COMMENT '关联的Release Id',
  `PreviousReleaseId` int unsigned NOT NULL DEFAULT '0' COMMENT '前一次发布的ReleaseId',
  `Operation` tinyint unsigned NOT NULL DEFAULT '0' COMMENT '发布类型，0: 普通发布，1: 回滚，2: 灰度发布，3: 灰度规则更新，4: 灰度合并回主分支发布，5: 主分支发布灰度自动发布，6: 主分支回滚灰度自动发布，7: 放弃灰度',
  `OperationContext` longtext NOT NULL COMMENT '发布上下文信息',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  KEY `IX_Namespace` (`AppId`,`ClusterName`,`NamespaceName`,`BranchName`),
  KEY `IX_ReleaseId` (`ReleaseId`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_PreviousReleaseId` (`PreviousReleaseId`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='发布历史';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ReleaseHistory`
--

LOCK TABLES `ReleaseHistory` WRITE;
/*!40000 ALTER TABLE `ReleaseHistory` DISABLE KEYS */;
INSERT INTO `ReleaseHistory` VALUES (1,'SampleApp','default','application','default',1,0,0,'{}',_binary '\0',0,'apollo','2023-06-08 09:24:45','apollo','2023-06-08 09:24:45'),(2,'shenyu','test','application','test',2,0,0,'{\"isEmergencyPublish\":false}',_binary '\0',0,'apollo','2023-06-08 11:02:48','apollo','2023-06-08 11:02:48'),(3,'shenyu','test','application','test',3,2,0,'{\"isEmergencyPublish\":false}',_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49'),(4,'shenyu','test','application','test',4,3,0,'{\"isEmergencyPublish\":false}',_binary '\0',0,'apollo','2023-06-08 11:02:49','apollo','2023-06-08 11:02:49');
/*!40000 ALTER TABLE `ReleaseHistory` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ReleaseMessage`
--

DROP TABLE IF EXISTS `ReleaseMessage`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ReleaseMessage` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `Message` varchar(1024) NOT NULL DEFAULT '' COMMENT '发布的消息内容',
  `DataChange_LastTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_Message` (`Message`(191))
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='发布消息';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ReleaseMessage`
--

LOCK TABLES `ReleaseMessage` WRITE;
/*!40000 ALTER TABLE `ReleaseMessage` DISABLE KEYS */;
INSERT INTO `ReleaseMessage` VALUES (1,'SampleApp+default+application','2023-06-08 09:24:45'),(4,'shenyu+test+application','2023-06-08 11:02:49');
/*!40000 ALTER TABLE `ReleaseMessage` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ServerConfig`
--

DROP TABLE IF EXISTS `ServerConfig`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ServerConfig` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `Key` varchar(64) NOT NULL DEFAULT 'default' COMMENT '配置项Key',
  `Cluster` varchar(32) NOT NULL DEFAULT 'default' COMMENT '配置对应的集群，default为不针对特定的集群',
  `Value` varchar(2048) NOT NULL DEFAULT 'default' COMMENT '配置项值',
  `Comment` varchar(1024) DEFAULT '' COMMENT '注释',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_Key_Cluster_DeletedAt` (`Key`,`Cluster`,`DeletedAt`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='配置服务自身配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ServerConfig`
--

LOCK TABLES `ServerConfig` WRITE;
/*!40000 ALTER TABLE `ServerConfig` DISABLE KEYS */;
INSERT INTO `ServerConfig` VALUES (1,'eureka.service.url','default','http://apollo-configservice:8080/eureka/','Eureka服务Url，多个service以英文逗号分隔',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(2,'namespace.lock.switch','default','false','一次发布只能有一个人修改开关',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(3,'item.key.length.limit','default','128','item key 最大长度限制',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(4,'item.value.length.limit','default','20000','item value最大长度限制',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45'),(5,'config-service.cache.enabled','default','false','ConfigService是否开启缓存，开启后能提高性能，但是会增大内存消耗！',_binary '\0',0,'default','2023-06-08 09:24:45','','2023-06-08 09:24:45');
/*!40000 ALTER TABLE `ServerConfig` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ServiceRegistry`
--

DROP TABLE IF EXISTS `ServiceRegistry`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ServiceRegistry` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `ServiceName` varchar(64) NOT NULL COMMENT '服务名',
  `Uri` varchar(64) NOT NULL COMMENT '服务地址',
  `Cluster` varchar(64) NOT NULL COMMENT '集群，可以用来标识apollo.cluster或者网络分区',
  `Metadata` varchar(1024) NOT NULL DEFAULT '{}' COMMENT '元数据，key value结构的json object，为了方面后面扩展功能而不需要修改表结构',
  `DataChange_CreatedTime` timestamp NOT NULL COMMENT '创建时间',
  `DataChange_LastTime` timestamp NOT NULL COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `IX_UNIQUE_KEY` (`ServiceName`,`Uri`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='注册中心';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ServiceRegistry`
--

LOCK TABLES `ServiceRegistry` WRITE;
/*!40000 ALTER TABLE `ServiceRegistry` DISABLE KEYS */;
/*!40000 ALTER TABLE `ServiceRegistry` ENABLE KEYS */;
UNLOCK TABLES;

