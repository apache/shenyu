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

CREATE DATABASE IF NOT EXISTS ApolloPortalDB DEFAULT CHARACTER SET = utf8mb4;

Use ApolloPortalDB;

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
INSERT INTO `App` VALUES (1,'SampleApp','Sample App','TEST1','样例部门1','apollo','apollo@acme.com',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(2,'shenyu','application','TEST1','样例部门1','apollo','apollo@acme.com',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
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
INSERT INTO `AppNamespace` VALUES (1,'application','SampleApp','properties',_binary '\0','default app namespace',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(2,'application','shenyu','properties',_binary '\0','default app namespace',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
/*!40000 ALTER TABLE `AppNamespace` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Authorities`
--

DROP TABLE IF EXISTS `Authorities`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Authorities` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `Username` varchar(64) NOT NULL,
  `Authority` varchar(50) NOT NULL,
  PRIMARY KEY (`Id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Authorities`
--

LOCK TABLES `Authorities` WRITE;
/*!40000 ALTER TABLE `Authorities` DISABLE KEYS */;
INSERT INTO `Authorities` VALUES (1,'apollo','ROLE_user');
/*!40000 ALTER TABLE `Authorities` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Consumer`
--

DROP TABLE IF EXISTS `Consumer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Consumer` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
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
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='开放API消费者';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Consumer`
--

LOCK TABLES `Consumer` WRITE;
/*!40000 ALTER TABLE `Consumer` DISABLE KEYS */;
INSERT INTO `Consumer` VALUES (1,'shenyu','application','TEST1','样例部门1','apollo','apollo@acme.com',_binary '\0',0,'apollo','2023-06-08 09:26:30','apollo','2023-06-08 09:26:30');
/*!40000 ALTER TABLE `Consumer` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ConsumerAudit`
--

DROP TABLE IF EXISTS `ConsumerAudit`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ConsumerAudit` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `ConsumerId` int unsigned DEFAULT NULL COMMENT 'Consumer Id',
  `Uri` varchar(1024) NOT NULL DEFAULT '' COMMENT '访问的Uri',
  `Method` varchar(16) NOT NULL DEFAULT '' COMMENT '访问的Method',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_ConsumerId` (`ConsumerId`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='consumer审计表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ConsumerAudit`
--

LOCK TABLES `ConsumerAudit` WRITE;
/*!40000 ALTER TABLE `ConsumerAudit` DISABLE KEYS */;
INSERT INTO `ConsumerAudit` VALUES (1,1,'/openapi/v1/envs/dev/apps/shenyu/clusters/test/namespaces/application/items/shenyu.plugin.json?createIfNotExists=true','PUT','2023-06-08 11:01:47','2023-06-08 11:01:47'),(2,1,'/openapi/v1/envs/dev/apps/shenyu/clusters/test/namespaces/application/items/shenyu.plugin.json?createIfNotExists=true','PUT','2023-06-08 11:02:48','2023-06-08 11:02:48'),(3,1,'/openapi/v1/envs/dev/apps/shenyu/clusters/test/namespaces/application/releases','POST','2023-06-08 11:02:48','2023-06-08 11:02:48'),(4,1,'/openapi/v1/envs/dev/apps/shenyu/clusters/test/namespaces/application/items/shenyu.selector.json?createIfNotExists=true','PUT','2023-06-08 11:02:48','2023-06-08 11:02:48'),(5,1,'/openapi/v1/envs/dev/apps/shenyu/clusters/test/namespaces/application/releases','POST','2023-06-08 11:02:49','2023-06-08 11:02:49'),(6,1,'/openapi/v1/envs/dev/apps/shenyu/clusters/test/namespaces/application/items/shenyu.rule.json?createIfNotExists=true','PUT','2023-06-08 11:02:49','2023-06-08 11:02:49'),(7,1,'/openapi/v1/envs/dev/apps/shenyu/clusters/test/namespaces/application/releases','POST','2023-06-08 11:02:49','2023-06-08 11:02:49');
/*!40000 ALTER TABLE `ConsumerAudit` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ConsumerRole`
--

DROP TABLE IF EXISTS `ConsumerRole`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ConsumerRole` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `ConsumerId` int unsigned DEFAULT NULL COMMENT 'Consumer Id',
  `RoleId` int unsigned DEFAULT NULL COMMENT 'Role Id',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_ConsumerId_RoleId_DeletedAt` (`ConsumerId`,`RoleId`,`DeletedAt`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_RoleId` (`RoleId`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='consumer和role的绑定表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ConsumerRole`
--

LOCK TABLES `ConsumerRole` WRITE;
/*!40000 ALTER TABLE `ConsumerRole` DISABLE KEYS */;
INSERT INTO `ConsumerRole` VALUES (1,1,7,_binary '\0',0,'apollo','2023-06-08 11:02:36','apollo','2023-06-08 11:02:36'),(2,1,8,_binary '\0',0,'apollo','2023-06-08 11:02:36','apollo','2023-06-08 11:02:36');
/*!40000 ALTER TABLE `ConsumerRole` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ConsumerToken`
--

DROP TABLE IF EXISTS `ConsumerToken`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `ConsumerToken` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `ConsumerId` int unsigned DEFAULT NULL COMMENT 'ConsumerId',
  `Token` varchar(128) NOT NULL DEFAULT '' COMMENT 'token',
  `Expires` datetime NOT NULL DEFAULT '2099-01-01 00:00:00' COMMENT 'token失效时间',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_Token_DeletedAt` (`Token`,`DeletedAt`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='consumer token表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ConsumerToken`
--

LOCK TABLES `ConsumerToken` WRITE;
/*!40000 ALTER TABLE `ConsumerToken` DISABLE KEYS */;
INSERT INTO `ConsumerToken` VALUES (1,1,'38d8911b852cd84ea4fe8c05227d5d0014bc3a95b9da42b0771ee7d9915aa379','2099-01-01 00:00:00',_binary '\0',0,'apollo','2023-06-08 09:26:30','apollo','2023-06-08 09:26:30');
/*!40000 ALTER TABLE `ConsumerToken` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Favorite`
--

DROP TABLE IF EXISTS `Favorite`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Favorite` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `UserId` varchar(32) NOT NULL DEFAULT 'default' COMMENT '收藏的用户',
  `AppId` varchar(500) NOT NULL DEFAULT 'default' COMMENT 'AppID',
  `Position` int NOT NULL DEFAULT '10000' COMMENT '收藏顺序',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_UserId_AppId_DeletedAt` (`UserId`,`AppId`,`DeletedAt`),
  KEY `AppId` (`AppId`(191)),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=23 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='应用收藏表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Favorite`
--

LOCK TABLES `Favorite` WRITE;
/*!40000 ALTER TABLE `Favorite` DISABLE KEYS */;
/*!40000 ALTER TABLE `Favorite` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Permission`
--

DROP TABLE IF EXISTS `Permission`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Permission` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `PermissionType` varchar(32) NOT NULL DEFAULT '' COMMENT '权限类型',
  `TargetId` varchar(256) NOT NULL DEFAULT '' COMMENT '权限对象类型',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_TargetId_PermissionType_DeletedAt` (`TargetId`,`PermissionType`,`DeletedAt`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=15 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='permission表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Permission`
--

LOCK TABLES `Permission` WRITE;
/*!40000 ALTER TABLE `Permission` DISABLE KEYS */;
INSERT INTO `Permission` VALUES (1,'CreateCluster','SampleApp',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(2,'CreateNamespace','SampleApp',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(3,'AssignRole','SampleApp',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(4,'ModifyNamespace','SampleApp+application',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(5,'ReleaseNamespace','SampleApp+application',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(6,'CreateApplication','SystemRole',_binary '\0',0,'apollo','2023-06-08 09:25:59','apollo','2023-06-08 09:25:59'),(7,'CreateNamespace','shenyu',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(8,'AssignRole','shenyu',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(9,'CreateCluster','shenyu',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(10,'ManageAppMaster','shenyu',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(11,'ModifyNamespace','shenyu+application',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(12,'ReleaseNamespace','shenyu+application',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(13,'ModifyNamespace','shenyu+application+DEV',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(14,'ReleaseNamespace','shenyu+application+DEV',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
/*!40000 ALTER TABLE `Permission` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Role`
--

DROP TABLE IF EXISTS `Role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Role` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `RoleName` varchar(256) NOT NULL DEFAULT '' COMMENT 'Role name',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_RoleName_DeletedAt` (`RoleName`,`DeletedAt`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=11 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='角色表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Role`
--

LOCK TABLES `Role` WRITE;
/*!40000 ALTER TABLE `Role` DISABLE KEYS */;
INSERT INTO `Role` VALUES (1,'Master+SampleApp',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(2,'ModifyNamespace+SampleApp+application',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(3,'ReleaseNamespace+SampleApp+application',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(4,'CreateApplication+SystemRole',_binary '\0',0,'apollo','2023-06-08 09:26:00','apollo','2023-06-08 09:26:00'),(5,'Master+shenyu',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(6,'ManageAppMaster+shenyu',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(7,'ModifyNamespace+shenyu+application',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(8,'ReleaseNamespace+shenyu+application',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(9,'ModifyNamespace+shenyu+application+DEV',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(10,'ReleaseNamespace+shenyu+application+DEV',_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
/*!40000 ALTER TABLE `Role` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `RolePermission`
--

DROP TABLE IF EXISTS `RolePermission`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `RolePermission` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `RoleId` int unsigned DEFAULT NULL COMMENT 'Role Id',
  `PermissionId` int unsigned DEFAULT NULL COMMENT 'Permission Id',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_RoleId_PermissionId_DeletedAt` (`RoleId`,`PermissionId`,`DeletedAt`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_PermissionId` (`PermissionId`)
) ENGINE=InnoDB AUTO_INCREMENT=15 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='角色和权限的绑定表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `RolePermission`
--

LOCK TABLES `RolePermission` WRITE;
/*!40000 ALTER TABLE `RolePermission` DISABLE KEYS */;
INSERT INTO `RolePermission` VALUES (1,1,1,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(2,1,2,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(3,1,3,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(4,2,4,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(5,3,5,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(6,4,6,_binary '\0',0,'apollo','2023-06-08 09:26:00','apollo','2023-06-08 09:26:00'),(7,5,7,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(8,5,8,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(9,5,9,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(10,6,10,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(11,7,11,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(12,8,12,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(13,9,13,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(14,10,14,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
/*!40000 ALTER TABLE `RolePermission` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SPRING_SESSION`
--

DROP TABLE IF EXISTS `SPRING_SESSION`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `SPRING_SESSION` (
  `PRIMARY_ID` char(36) NOT NULL,
  `SESSION_ID` char(36) NOT NULL,
  `CREATION_TIME` bigint NOT NULL,
  `LAST_ACCESS_TIME` bigint NOT NULL,
  `MAX_INACTIVE_INTERVAL` int NOT NULL,
  `EXPIRY_TIME` bigint NOT NULL,
  `PRINCIPAL_NAME` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`PRIMARY_ID`),
  UNIQUE KEY `SPRING_SESSION_IX1` (`SESSION_ID`),
  KEY `SPRING_SESSION_IX2` (`EXPIRY_TIME`),
  KEY `SPRING_SESSION_IX3` (`PRINCIPAL_NAME`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SPRING_SESSION`
--

LOCK TABLES `SPRING_SESSION` WRITE;
/*!40000 ALTER TABLE `SPRING_SESSION` DISABLE KEYS */;
INSERT INTO `SPRING_SESSION` VALUES ('41e72606-2ba5-446c-8e96-22c9cd191a8c','468b226f-617f-4504-8b72-f95c729c92e5',1686222119655,1686222248216,1800,1686224048216,'apollo');
/*!40000 ALTER TABLE `SPRING_SESSION` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `SPRING_SESSION_ATTRIBUTES`
--

DROP TABLE IF EXISTS `SPRING_SESSION_ATTRIBUTES`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `SPRING_SESSION_ATTRIBUTES` (
  `SESSION_PRIMARY_ID` char(36) NOT NULL,
  `ATTRIBUTE_NAME` varchar(200) NOT NULL,
  `ATTRIBUTE_BYTES` blob NOT NULL,
  PRIMARY KEY (`SESSION_PRIMARY_ID`,`ATTRIBUTE_NAME`),
  CONSTRAINT `SPRING_SESSION_ATTRIBUTES_FK` FOREIGN KEY (`SESSION_PRIMARY_ID`) REFERENCES `SPRING_SESSION` (`PRIMARY_ID`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `SPRING_SESSION_ATTRIBUTES`
--

LOCK TABLES `SPRING_SESSION_ATTRIBUTES` WRITE;
/*!40000 ALTER TABLE `SPRING_SESSION_ATTRIBUTES` DISABLE KEYS */;
INSERT INTO `SPRING_SESSION_ATTRIBUTES` VALUES ('41e72606-2ba5-446c-8e96-22c9cd191a8c','SPRING_SECURITY_CONTEXT',_binary '{\"@class\":\"org.springframework.security.core.context.SecurityContextImpl\",\"authentication\":{\"@class\":\"org.springframework.security.authentication.UsernamePasswordAuthenticationToken\",\"authorities\":[\"java.util.Collections$UnmodifiableRandomAccessList\",[{\"@class\":\"org.springframework.security.core.authority.SimpleGrantedAuthority\",\"authority\":\"ROLE_user\"}]],\"details\":{\"@class\":\"org.springframework.security.web.authentication.WebAuthenticationDetails\",\"remoteAddress\":\"192.168.128.1\",\"sessionId\":\"cde32e45-031b-407e-9491-2ad8232202e9\"},\"authenticated\":true,\"principal\":{\"@class\":\"org.springframework.security.core.userdetails.User\",\"password\":null,\"username\":\"apollo\",\"authorities\":[\"java.util.Collections$UnmodifiableSet\",[{\"@class\":\"org.springframework.security.core.authority.SimpleGrantedAuthority\",\"authority\":\"ROLE_user\"}]],\"accountNonExpired\":true,\"accountNonLocked\":true,\"credentialsNonExpired\":true,\"enabled\":true},\"credentials\":null}}');
/*!40000 ALTER TABLE `SPRING_SESSION_ATTRIBUTES` ENABLE KEYS */;
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
  `Value` varchar(2048) NOT NULL DEFAULT 'default' COMMENT '配置项值',
  `Comment` varchar(1024) DEFAULT '' COMMENT '注释',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_Key_DeletedAt` (`Key`,`DeletedAt`),
  KEY `DataChange_LastTime` (`DataChange_LastTime`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='配置服务自身配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ServerConfig`
--

LOCK TABLES `ServerConfig` WRITE;
/*!40000 ALTER TABLE `ServerConfig` DISABLE KEYS */;
INSERT INTO `ServerConfig` VALUES (1,'apollo.portal.envs','dev','可支持的环境列表',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(2,'organizations','[{\"orgId\":\"TEST1\",\"orgName\":\"样例部门1\"},{\"orgId\":\"TEST2\",\"orgName\":\"样例部门2\"}]','部门列表',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(3,'superAdmin','apollo','Portal超级管理员',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(4,'api.readTimeout','10000','http接口read timeout',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(5,'consumer.token.salt','someSalt','consumer token salt',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(6,'admin.createPrivateNamespace.switch','true','是否允许项目管理员创建私有namespace',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(7,'configView.memberOnly.envs','dev','只对项目成员显示配置信息的环境列表，多个env以英文逗号分隔',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(8,'apollo.portal.meta.servers','{}','各环境Meta Service列表',_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46');
/*!40000 ALTER TABLE `ServerConfig` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `UserRole`
--

DROP TABLE IF EXISTS `UserRole`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `UserRole` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `UserId` varchar(128) DEFAULT '' COMMENT '用户身份标识',
  `RoleId` int unsigned DEFAULT NULL COMMENT 'Role Id',
  `IsDeleted` bit(1) NOT NULL DEFAULT b'0' COMMENT '1: deleted, 0: normal',
  `DeletedAt` bigint NOT NULL DEFAULT '0' COMMENT 'Delete timestamp based on milliseconds',
  `DataChange_CreatedBy` varchar(64) NOT NULL DEFAULT 'default' COMMENT '创建人邮箱前缀',
  `DataChange_CreatedTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `DataChange_LastModifiedBy` varchar(64) DEFAULT '' COMMENT '最后修改人邮箱前缀',
  `DataChange_LastTime` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_UserId_RoleId_DeletedAt` (`UserId`,`RoleId`,`DeletedAt`),
  KEY `IX_DataChange_LastTime` (`DataChange_LastTime`),
  KEY `IX_RoleId` (`RoleId`)
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户和role的绑定表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `UserRole`
--

LOCK TABLES `UserRole` WRITE;
/*!40000 ALTER TABLE `UserRole` DISABLE KEYS */;
INSERT INTO `UserRole` VALUES (1,'apollo',1,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(2,'apollo',2,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(3,'apollo',3,_binary '\0',0,'default','2023-06-08 09:24:46','','2023-06-08 09:24:46'),(4,'apollo',5,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(5,'apollo',7,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47'),(6,'apollo',8,_binary '\0',0,'apollo','2023-06-08 09:26:47','apollo','2023-06-08 09:26:47');
/*!40000 ALTER TABLE `UserRole` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Users`
--

DROP TABLE IF EXISTS `Users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `Users` (
  `Id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '自增Id',
  `Username` varchar(64) NOT NULL DEFAULT 'default' COMMENT '用户登录账户',
  `Password` varchar(512) NOT NULL DEFAULT 'default' COMMENT '密码',
  `UserDisplayName` varchar(512) NOT NULL DEFAULT 'default' COMMENT '用户名称',
  `Email` varchar(64) NOT NULL DEFAULT 'default' COMMENT '邮箱地址',
  `Enabled` tinyint DEFAULT NULL COMMENT '是否有效',
  PRIMARY KEY (`Id`),
  UNIQUE KEY `UK_Username` (`Username`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Users`
--

LOCK TABLES `Users` WRITE;
/*!40000 ALTER TABLE `Users` DISABLE KEYS */;
INSERT INTO `Users` VALUES (1,'apollo','$2a$10$7r20uS.BQ9uBpf3Baj3uQOZvMVvB1RN3PYoKE94gtz2.WAOuiiwXS','apollo','apollo@acme.com',1);
/*!40000 ALTER TABLE `Users` ENABLE KEYS */;
UNLOCK TABLES;

