# Soul后端服务API

------

## 用户管理

### 1.查询用户

URL地址：[/dashboardUser](http://127.0.0.1:8082/dashboardUser)

请求方式：GET

请求参数：

|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|userName|String|否|-|用户名|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|

请求范例：[http://127.0.0.1:8082/dashboardUser?userName=ADMIN&currentPage=1&pageSize=10](http://127.0.0.1:8082/dashboardUser?userName=ADMIN&currentPage=1&pageSize=10)

响应结果：

```java
public class CommonPager<T> {

    /**
     * page.
     */
    private PageParameter page;

    /**
     * data.
     */
    private List<DashboardUserVO> dataList;
}
public class PageParameter {

    private int currentPage;

    private int prePage;

    private int nextPage;

    private int pageSize;

    private int offset;

    private int totalPage;

    private int totalCount;
}
public class DashboardUserVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user name.
     */
    private String userName;

    /**
     * user password.
     */
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "query dashboard users success",
    "data": {
        "page": {
            "currentPage": 1,
            "prePage": 1,
            "nextPage": 1,
            "pageSize": 10,
            "offset": 0,
            "totalPage": 1,
            "totalCount": 1
        },
        "dataList": [
            {
                "id": "1",
                "userName": "admin",
                "password": "123456",
                "role": 1,
                "enabled": true,
                "dateCreated": "2018-06-23 15:12:22",
                "dateUpdated": "2018-06-23 15:12:23"
            }
        ]
    }
}
```
### 2.明细用户

URL地址：[/dashboardUser/{id}](http://127.0.0.1:8082/dashboardUser/{id})

请求方式：GET

请求参数：无

请求范例：[http://127.0.0.1:8082/dashboardUser/1](http://127.0.0.1:8082/dashboardUser/1)

响应结果：
```java
public class DashboardUserVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user name.
     */
    private String userName;

    /**
     * user password.
     */
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "detail dashboard user success",
    "data": {
        "id": "1",
        "userName": "admin",
        "password": "123456",
        "role": 1,
        "enabled": true,
        "dateCreated": "2018-06-23 15:12:22",
        "dateUpdated": "2018-06-23 15:12:23"
    }
}
```
### 3.新增用户

URL地址：[/dashboardUser](http://127.0.0.1:8082/dashboardUser)

请求方式：POST

请求参数：
```java
public class DashboardUserDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user name.
     */
    private String userName;

    /**
     * user password.
     */
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * whether enabled.
     */
    private Boolean enabled;
}
```
参数范例：
```json
{
	"userName": "soul",
	"password": "soul",
	"role": 1,
	"enabled": true
}
```
响应结果：
```json
{
    "code": 200,
    "message": "create dashboard user success",
    "data": 1
}
```
### 4.编辑用户
URL地址：[/dashboardUser/{id}](http://127.0.0.1:8082/dashboardUser/{id})

请求方式：PUT

请求参数：
```java
public class DashboardUserDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user name.
     */
    private String userName;

    /**
     * user password.
     */
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * whether enabled.
     */
    private Boolean enabled;
}
```
参数范例：
```json
{
	"userName": "soul",
	"password": "soul",
	"role": 2,
	"enabled": true
}
```
响应结果：
```json
{
    "code": 200,
    "message": "update dashboard user success",
    "data": 1
}
```
### 5.删除用户

URL地址：[/dashboardUser/batch](http://127.0.0.1:8082/dashboardUser/batch)

请求方式：DELETE

请求参数：["1","2"]

响应结果：
```
{
    "code": 200,
    "message": "delete dashboard user success",
    "data": 1
}
```

-------------------
## 插件管理

### 1.查询插件

URL地址：[/plugin](http://127.0.0.1:8082/plugin)

请求方式：GET

请求参数：

|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|name|String|否|-|插件名|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|

请求范例：
[http://127.0.0.1:8082/plugin?name=sign&currentPage=1&pageSize=10](http://127.0.0.1:8082/plugin?name=sign&currentPage=1&pageSize=10)
响应结果：
```java
public class CommonPager<T> {

    /**
     * page.
     */
    private PageParameter page;

    /**
     * data.
     */
    private List<PluginVO> dataList;
}
public class PageParameter {

    private int currentPage;

    private int prePage;

    private int nextPage;

    private int pageSize;

    private int offset;

    private int totalPage;

    private int totalCount;
}
public class PluginVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin code.
     */
    private Integer code;

    /**
     * plugin name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "query plugins success",
    "data": {
        "page": {
            "currentPage": 1,
            "prePage": 1,
            "nextPage": 1,
            "pageSize": 10,
            "offset": 0,
            "totalPage": 1,
            "totalCount": 7
        },
        "dataList": [
            {
                "id": "1",
                "code": 2,
                "name": "sign",
                "enabled": false,
                "dateCreated": "2018-06-14 10:17:35",
                "dateUpdated": "2018-06-14 10:17:35"
            },
            {
                "id": "2",
                "code": 10,
                "name": "waf",
                "enabled": false,
                "dateCreated": "2018-06-23 10:26:30",
                "dateUpdated": "2018-06-13 15:43:10"
            },
            {
                "id": "3",
                "code": 30,
                "name": "rewrite",
                "enabled": false,
                "dateCreated": "2018-06-23 10:26:34",
                "dateUpdated": "2018-06-25 13:59:31"
            },
            {
                "id": "4",
                "code": 20,
                "name": "rate_limiter",
                "enabled": false,
                "dateCreated": "2018-06-23 10:26:37",
                "dateUpdated": "2018-06-13 15:34:48"
            },
            {
                "id": "5",
                "code": 50,
                "name": "divide",
                "enabled": true,
                "dateCreated": "2018-06-25 10:19:10",
                "dateUpdated": "2018-06-13 13:56:04"
            },
            {
                "id": "6",
                "code": 60,
                "name": "dubbo",
                "enabled": false,
                "dateCreated": "2018-06-23 10:26:41",
                "dateUpdated": "2018-06-11 10:11:47"
            },
            {
                "id": "7",
                "code": 80,
                "name": "monitor",
                "enabled": false,
                "dateCreated": "2018-06-25 13:47:57",
                "dateUpdated": "2018-06-25 13:47:57"
            }
        ]
    }
}
```
### 2.明细插件

URL地址：[/plugin/{id}](http://127.0.0.1:8082/plugin/{id})

请求范例：[http://127.0.0.1:8082/plugin/1](http://127.0.0.1:8082/plugin/1)

请求方式：GET

请求参数：无

响应结果：
```java
public class PluginVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin code.
     */
    private Integer code;

    /**
     * plugin name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "detail plugin success",
    "data": {
        "id": "1",
        "code": 2,
        "name": "sign",
        "enabled": false,
        "dateCreated": "2018-06-14 10:17:35",
        "dateUpdated": "2018-06-14 10:17:35"
    }
}
```
### 3.新增插件

URL地址：[/plugin](http://127.0.0.1:8082/plugin)

请求方式：POST

请求参数：
```java
public class PluginDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin code.
     */
    private Integer code;

    /**
     * whether enabled.
     */
    private Boolean enabled;
}
```
参数范例：
```json
{
	"code": 1,
	"enabled": true
}
```
响应结果：
```json
{
    "code": 200,
    "message": "create plugin success",
    "data": 1
}
```
### 4.编辑插件

URL地址：[/plugin/{id}](http://127.0.0.1:8082/plugin/{id})

请求方式：PUT

请求参数：
```java
public class PluginDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin code.
     */
    private Integer code;

    /**
     * whether enabled.
     */
    private Boolean enabled;
}
```
参数范例：
```json
{
	"code": 2,
	"enabled": false
}
```
响应结果：
```json
{
    "code": 200,
    "message": "update plugin success",
    "data": 1
}
```
### 5.删除插件

URL地址：[/plugin/batch](http://127.0.0.1:8082/plugin/batch)

请求方式：DELETE

请求参数：["1","2"]

响应结果：
```json
{
    "code": 200,
    "message": "delete plugin success",
    "data": 1
}
```

-------------------

## 选择器管理

### 1.查询选择器

URL地址：[/selector](http://127.0.0.1:8082/selector)

请求方式：GET

请求参数：

|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|pluginId|String|否|-|插件ID|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|

请求范例：[http://127.0.0.1:8082/selector?pluginId=1&currentPage=1&pageSize=10](http://127.0.0.1:8082/selector?pluginId=1&currentPage=1&pageSize=10)

响应结果：
```java
public class CommonPager<T> {

    /**
     * page.
     */
    private PageParameter page;

    /**
     * data.
     */
    private List<SelectorVO> dataList;
}
public class SelectorVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * selector name.
     */
    private String name;

    /**
     * match mode code.
     */
    private Integer matchMode;

    /**
     * match mode name.
     */
    private String matchModeName;

    /**
     * selector type code.
     */
    private Integer type;

    /**
     * selector type name.
     */
    private String typeName;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * whether continued.
     */
    private Boolean continued;

    /**
     * selector conditions.
     */
    private List<SelectorConditionVO> selectorConditions;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "query selectors success",
    "data": {
        "page": {
            "currentPage": 1,
            "prePage": 1,
            "nextPage": 1,
            "pageSize": 10,
            "offset": 0,
            "totalPage": 1,
            "totalCount": 1
        },
        "dataList": [
            {
                "id": "Xmj2tL1T",
                "pluginId": "1",
                "name": "selector",
                "matchMode": 1,
                "matchModeName": "or",
                "type": 1,
                "typeName": "custom flow",
                "sort": 1,
                "enabled": true,
                "loged": true,
                "continued": true,
                "selectorConditions": null,
                "dateCreated": "2018-08-09 16:16:21",
                "dateUpdated": "2018-08-09 16:16:21"
            }
        ]
    }
}
```

### 2.明细选择器

URL地址：[/selector/{id}](http://127.0.0.1:8082/selector/{id})

请求方式：GET

请求参数：无

请求范例：[http://127.0.0.1:8082/selector/1](http://127.0.0.1:8082/selector/1)

响应结果：
```java
public class SelectorVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * selector name.
     */
    private String name;

    /**
     * match mode code.
     */
    private Integer matchMode;

    /**
     * match mode name.
     */
    private String matchModeName;

    /**
     * selector type code.
     */
    private Integer type;

    /**
     * selector type name.
     */
    private String typeName;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * whether continued.
     */
    private Boolean continued;

    /**
     * selector conditions.
     */
    private List<SelectorConditionVO> selectorConditions;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
	"code": 200,
	"message": "detail selector success",
	"data": {
		"id": "Xmj2tL1T",
		"pluginId": "1",
		"name": "selector",
		"matchMode": 1,
		"matchModeName": "or",
		"type": 1,
		"typeName": "custom flow",
		"sort": 1,
		"enabled": true,
		"loged": true,
		"continued": true,
		"selectorConditions": [{
			"id": "PweXPL2t",
			"selectorId": "Xmj2tL1T",
			"paramType": "host",
			"paramTypeName": "HOST",
			"operator": "match",
			"operatorName": "MATCH",
			"paramName": "paramName",
			"paramValue": "paramValue",
			"dateCreated": "2018-08-09 17:07:51",
			"dateUpdated": "2018-08-09 16:16:21"
		}],
		"dateCreated": "2018-08-09 16:16:21",
		"dateUpdated": "2018-08-09 16:16:21"
	}
}
```

### 3.新增选择器

URL地址：[/selector](http://127.0.0.1:8082/selector)

请求方式：POST

请求参数：
```java
public class SelectorDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * selector name.
     */
    private String name;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * selector type.
     */
    private Integer type;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * whether continued.
     */
    private Boolean continued;

    /**
     * selector conditions.
     */
    private List<SelectorConditionDTO> selectorConditions;
}
public class SelectorConditionDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * parameter type.
     */
    private String paramType;

    /**
     * match operator.
     */
    private String operator;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;
}
```
参数范例：
```json
{
	"pluginId": "1",
	"name": "selector",
	"matchMode": 1,
	"type": 1,
	"sort": 1,
	"enabled": true,
	"loged": true,
	"continued": true,
	"selectorConditions": [{
		"paramType": "host",
		"operator": "match",
		"paramName": "paramName",
		"paramValue": "paramValue"
	}]
}
```
响应结果：
```json
{
    "code": 200,
    "message": "create selector success",
    "data": 1
}
```
### 4.编辑选择器

URL地址：[/selector/{id}](http://127.0.0.1:8082/selector/{id})

请求方式：PUT

请求参数：
```java
public class SelectorDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * selector name.
     */
    private String name;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * selector type.
     */
    private Integer type;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * whether continued.
     */
    private Boolean continued;

    /**
     * selector conditions.
     */
    private List<SelectorConditionDTO> selectorConditions;
}
public class SelectorConditionDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * parameter type.
     */
    private String paramType;

    /**
     * match operator.
     */
    private String operator;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;
}
```
参数范例：
```json
{
	"pluginId": "1",
	"name": "selector",
	"matchMode": 1,
	"type": 1,
	"sort": 1,
	"enabled": true,
	"loged": true,
	"continued": true,
	"selectorConditions": [{
		"paramType": "host",
		"operator": "match",
		"paramName": "paramNam",
		"paramValue": "paramValu"
	}]
}
```
响应结果：
```json
{
    "code": 200,
    "message": "update selector success",
    "data": 1
}
```

### 5.删除选择器

URL地址：[/selector/batch](http://127.0.0.1:8082/selector/batch)

请求方式：DELETE

请求参数：["1","2"]

响应结果：
```json
{
    "code": 200,
    "message": "delete selector success",
    "data": 1
}
```

-------------------

## 规则管理

### 1.查询规则

URL地址：[/rule](http://127.0.0.1:8082/rule)

请求方式：GET

请求参数：

|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|selectorId|String|否|-|选择器ID|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|

请求范例：[http://127.0.0.1:8082/rule?selectorId=1&currentPage=1&pageSize=10](http://127.0.0.1:8082/rule?selectorId=1&currentPage=1&pageSize=10)

响应结果：
```java
public class CommonPager<T> {

    /**
     * page.
     */
    private PageParameter page;

    /**
     * data.
     */
    private List<RuleVO> dataList;
}
public class RuleVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * match mode name.
     */
    private String matchModeName;

    /**
     * rule name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;

    /**
     * rule conditions.
     */
    private List<RuleConditionVO> ruleConditions;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
	"code": 200,
	"message": "query rules success",
	"data": {
		"page": {
			"currentPage": 1,
			"prePage": 1,
			"nextPage": 1,
			"pageSize": 15,
			"offset": 0,
			"totalPage": 1,
			"totalCount": 1
		},
		"dataList": [{
			"id": "dB4JdjWu",
			"selectorId": "DHSqDfqC",
			"matchMode": 0,
			"matchModeName": "and",
			"name": "rule1",
			"enabled": true,
			"loged": true,
			"sort": 1,
			"handle": "xxxxxx",
			"ruleConditions": null,
			"dateCreated": "2018-07-29 07:24:20",
			"dateUpdated": "2018-07-29 07:24:21"
		}]
	}
}
```

### 2.明细规则

URL地址：[/rule/{id}](http://127.0.0.1:8082/rule/{id})

请求方式：GET

请求参数：无

请求范例：[http://127.0.0.1:8082/rule/1](http://127.0.0.1:8082/rule/1)

响应结果：
```java
public class RuleVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * match mode name.
     */
    private String matchModeName;

    /**
     * rule name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;

    /**
     * rule conditions.
     */
    private List<RuleConditionVO> ruleConditions;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
	"code": 200,
	"message": "query rules success",
	"data": {
		"id": "dB4JdjWu",
		"selectorId": "DHSqDfqC",
		"matchMode": 0,
		"matchModeName": "and",
		"name": "rule1",
		"enabled": true,
		"loged": true,
		"sort": 1,
		"handle": "xxxxxx",
		"ruleConditions": [{
				"id": "L1d8rGVP",
				"ruleId": "dB4JdjWu",
				"paramType": "query",
				"paramTypeName": "QUERY",
				"operator": ">",
				"operatorName": "GT",
				"paramName": "param2",
				"paramValue": "val2",
				"dateCreated": "2018-07-29 07:24:22",
				"dateUpdated": "2018-07-29 07:24:22"
			},
			{
				"id": "L9CroLVq",
				"ruleId": "dB4JdjWu",
				"paramType": "post",
				"paramTypeName": "POST",
				"operator": "<",
				"operatorName": "LT",
				"paramName": "param1",
				"paramValue": "val1",
				"dateCreated": "2018-07-29 07:24:21",
				"dateUpdated": "2018-07-29 07:24:22"
			}
		],
		"dateCreated": "2018-07-29 07:24:20",
		"dateUpdated": "2018-07-29 07:24:21"
	}
}
```
### 3.新增规则

URL地址：[/rule](http://127.0.0.1:8082/rule)

请求方式：POST

请求参数：
```java
public class RuleDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * rule name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;

    /**
     * rule conditions.
     */
    private List<RuleConditionDTO> ruleConditions;
}
public class RuleConditionDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * rule id.
     */
    private String ruleId;

    /**
     * parameter type.
     */
    private String paramType;

    /**
     * match operator.
     */
    private String operator;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;
}
```
参数范例：
```json
{
	"selectorId": "1",
	"matchMode": 1,
	"name": "rule",
	"enabled": true,
	"loged": true,
	"sort": 1,
	"handle": "handle",
	"ruleConditionDTO": [{
		"paramType": "host",
		"operator": "match",
		"paramName": "paramName",
		"paramValue": "paramValue"
	}]
}
```
响应结果：
```json
{
    "code": 200,
    "message": "create rule success",
    "data": 1
}
```
**(4)编辑规则**
URL地址：
[/rule/{id}](http://127.0.0.1:8082/rule/{id})
请求方式：
PUT
请求参数：
```java
public class RuleDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * rule name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;

    /**
     * rule conditions.
     */
    private List<RuleConditionDTO> ruleConditions;
}
public class RuleConditionDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * rule id.
     */
    private String ruleId;

    /**
     * parameter type.
     */
    private String paramType;

    /**
     * match operator.
     */
    private String operator;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;
}
```
参数范例：
```json
{
	"selectorId": "1",
	"matchMode": 1,
	"name": "rule",
	"enabled": true,
	"loged": true,
	"sort": 1,
	"handle": "handle",
	"ruleConditionDTO": [{
		"id": "1",
		"ruleId": "1",
		"paramType": "host",
		"operator": "match",
		"paramName": "paramNam",
		"paramValue": "paramVal"
	}]
}
```
响应结果：
```json
{
    "code": 200,
    "message": "update rule success",
    "data": 1
}
```
### 5.删除规则

URL地址：[/rule/batch](http://127.0.0.1:8082/rule/batch)

请求方式：DELETE

请求参数：["1","2"]

响应结果：
```json
{
    "code": 200,
    "message": "delete rule success",
    "data": 1
}
```


-------------------
## 认证管理

### 1.查询认证

URL地址：[/appAuth](http://127.0.0.1:8082/appAuth)

请求方式：GET

请求参数：

|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|appKey|String|否|-|应用键|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|

请求范例：[http://127.0.0.1:8082/appAuth?appKey=applicationKey&currentPage=1&pageSize=10](http://127.0.0.1:8082/appAuth?appKey=applicationKey&currentPage=1&pageSize=10)

响应结果：
```java
public class CommonPager<T> {

    /**
     * page.
     */
    private PageParameter page;

    /**
     * data.
     */
    private List<AppAuthVO> dataList;
}
public class PageParameter {

    private int currentPage;

    private int prePage;

    private int nextPage;

    private int pageSize;

    private int offset;

    private int totalPage;

    private int totalCount;
}
public class AppAuthVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "query dashboard users success",
    "data": {
        "page": {
            "currentPage": 1,
            "prePage": 1,
            "nextPage": 1,
            "pageSize": 10,
            "offset": 0,
            "totalPage": 1,
            "totalCount": 1
        },
        "dataList": [
            {
                "id": "1",
                "appKey": "appKey",
                "appSecret": "appSecret",
                "enabled": true,
                "dateCreated": "2018-06-23 15:12:22",
                "dateUpdated": "2018-06-23 15:12:23"
            }
        ]
    }
}
```
### 2.明细认证

URL地址：[/appAuth/{id}](http://127.0.0.1:8082/appAuth/{id})

请求方式：GET

请求参数：无

请求范例：[http://127.0.0.1:8082/appAuth/1](http://127.0.0.1:8082/appAuth/1)

响应结果：
```java
public class AppAuthVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "detail dashboard user success",
    "data": {
        "id": "1",
        "appKey": "appKey",
        "appSecret": "appSecret",
        "enabled": true,
        "dateCreated": "2018-06-23 15:12:22",
        "dateUpdated": "2018-06-23 15:12:23"
    }
}
```
**(3)新增认证**
URL地址：
[/appAuth](http://127.0.0.1:8082/appAuth)
请求方式：
POST
请求参数：
```java
public class AppAuthDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    /**
     * whether enabled.
     */
    private Boolean enabled;
}
```
参数范例：
```json
{
	"appKey": "appKey",
	"appSecret": "appSecret",
	"enabled": true
}
```
响应结果：
```json
{
    "code": 200,
    "message": "create application authority success",
    "data": 1
}
```

### 4.编辑认证

URL地址：[/appAuth/{id}](http://127.0.0.1:8082/appAuth/{id})

请求方式：PUT

请求参数：
```java
public class AppAuthDTO {

    /**
     * primary key.
     */
    private String id;

    /**
     * application key.
     */
    private String appKey;

    /**
     * encryption secret.
     */
    private String appSecret;

    /**
     * whether enabled.
     */
    private Boolean enabled;
}
```
参数范例：
```json
{
	"appKey": "appKey",
	"appSecret": "appSecret",
	"enabled": false
}
```
响应结果：
```json
{
    "code": 200,
    "message": "update application authority success",
    "data": 1
}
```
### 5.删除认证

URL地址：[/appAuth/batch](http://127.0.0.1:8082/appAuth/batch)

请求方式：DELETE

请求参数：["1","2"]

响应结果：
```
{
    "code": 200,
    "message": "delete application authority success",
    "data": 1
}
```

-------------------
## 平台管理

URL地址：[/platform/login](http://127.0.0.1:8082/platform/login)

请求方式：GET

请求参数：

|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|userName|String|否|-|账号|
|password|String|否|-|密码|

请求范例：[http://127.0.0.1:8082/platform/login?userName=admin&password=123456](http://127.0.0.1:8082/platform/login?userName=admin&password=123456)

响应结果：
```java
public class DashboardUserVO {

    /**
     * primary key.
     */
    private String id;

    /**
     * user name.
     */
    private String userName;

    /**
     * user password.
     */
    private String password;

    /**
     * dashboard role.
     */
    private Integer role;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;
}
```
响应范例：
```json
{
    "code": 200,
    "message": "login dashboard user success",
    "data": {
        "id": "1",
        "userName": "admin",
        "password": "123456",
        "role": 1,
        "enabled": true,
        "dateCreated": "2018-06-23 15:12:22",
        "dateUpdated": "2018-06-23 15:12:23"
    }
}
```
### 查询枚举

URL地址：[/platform/enum](http://127.0.0.1:8082/platform/enum)

请求方式：GET

请求参数：无

请求范例：[http://127.0.0.1:8082/platform)

响应结果：
```java
public class EnumVO implements Serializable {

    /**
     * enum code.
     */
    private Object code;

    /**
     * enum name.
     */
    private String name;

    /**
     * whether support.
     */
    private Boolean support;
}
```
响应范例：
```json
{
	"code": 200,
	"message": null,
	"data": {
		"matchModeEnums": [{
			"code": 0,
			"name": "and",
			"support": true
		}, {
			"code": 1,
			"name": "or",
			"support": true
		}],
		"wafEnums": [{
			"code": 0,
			"name": "reject",
			"support": true
		}, {
			"code": 1,
			"name": "allow",
			"support": true
		}],
		"pluginEnums": [{
			"code": 1,
			"name": "global",
			"support": true
		}, {
			"code": 2,
			"name": "sign",
			"support": true
		}, {
			"code": 10,
			"name": "waf",
			"support": true
		}, {
			"code": 20,
			"name": "rate_limiter",
			"support": true
		}, {
			"code": 30,
			"name": "rewrite",
			"support": true
		}, {
			"code": 40,
			"name": "redirect",
			"support": true
		}, {
			"code": 50,
			"name": "divide",
			"support": true
		}, {
			"code": 60,
			"name": "dubbo",
			"support": true
		}, {
			"code": 70,
			"name": "springCloud",
			"support": true
		}, {
			"code": 80,
			"name": "monitor",
			"support": true
		}],
		"selectorTypeEnums": [{
			"code": 0,
			"name": "full flow",
			"support": true
		}, {
			"code": 1,
			"name": "custom flow",
			"support": true
		}],
		"rpcTypeEnums": [{
			"code": null,
			"name": "http",
			"support": true
		}, {
			"code": null,
			"name": "dubbo",
			"support": true
		}, {
			"code": null,
			"name": "springCloud",
			"support": true
		}, {
			"code": null,
			"name": "motan",
			"support": false
		}, {
			"code": null,
			"name": "grpc",
			"support": false
		}],
		"operatorEnums": [{
			"code": null,
			"name": "match",
			"support": true
		}, {
			"code": null,
			"name": "=",
			"support": true
		}, {
			"code": null,
			"name": ">",
			"support": false
		}, {
			"code": null,
			"name": "<",
			"support": false
		}, {
			"code": null,
			"name": "like",
			"support": true
		}],
		"paramTypeEnums": [{
			"code": null,
			"name": "post",
			"support": false
		}, {
			"code": null,
			"name": "uri",
			"support": false
		}, {
			"code": null,
			"name": "query",
			"support": false
		}, {
			"code": null,
			"name": "host",
			"support": true
		}, {
			"code": null,
			"name": "ip",
			"support": true
		}, {
			"code": null,
			"name": "header",
			"support": true
		}],
		"pluginTypeEnums": [{
			"code": null,
			"name": "before",
			"support": true
		}, {
			"code": null,
			"name": "function",
			"support": true
		}, {
			"code": null,
			"name": "last",
			"support": true
		}],
		"loadBalanceEnums": [{
			"code": 1,
			"name": "hash",
			"support": true
		}, {
			"code": 2,
			"name": "random",
			"support": true
		}, {
			"code": 3,
			"name": "roundRobin",
			"support": true
		}],
		"httpMethodEnums": [{
			"code": null,
			"name": "get",
			"support": true
		}, {
			"code": null,
			"name": "post",
			"support": true
		}, {
			"code": null,
			"name": "put",
			"support": false
		}, {
			"code": null,
			"name": "delete",
			"support": false
		}],
		"serializeEnums": [{
			"code": null,
			"name": "jdk",
			"support": true
		}, {
			"code": null,
			"name": "kryo",
			"support": true
		}, {
			"code": null,
			"name": "hessian",
			"support": true
		}, {
			"code": null,
			"name": "protostuff",
			"support": true
		}]
	}
}
```
