##环球易购网关管理系统后端服务API
**用户管理**
**(1)查询用户** 
URL地址：
[/dashboardUser](http://127.0.0.1:8082/dashboardUser)
请求方式：
GET
请求参数：
|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|userName|String|否|-|用户名|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|
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
            "userName": "ADMIN",
            "password": "123456",
            "role": 1,
            "enabled": true,
            "dateCreated": "2018-07-28 13:38:05",
            "dateUpdated": "2018-07-28 13:38:05"
        }
    ]
}
```
**(2)明细用户** 
URL地址：
[/dashboardUser/{id}](http://127.0.0.1:8082/dashboardUser/{id})
请求方式：
GET
请求参数：
无
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
    "id": "1",
    "userName": "ADMIN",
    "password": "123456",
    "role": 1,
    "enabled": true,
    "dateCreated": "2018-07-28 13:38:05",
    "dateUpdated": "2018-07-28 13:38:05"
}
```
**(3)新增用户** 
URL地址：
[/dashboardUser](http://127.0.0.1:8082/dashboardUser)
请求方式：
POST
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
响应结果：
1
**(4)编辑用户** 
URL地址：
[/dashboardUser/{id}](http://127.0.0.1:8082/dashboardUser/{id})
请求方式：
PUT
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
响应结果：
1
**(5)删除用户**
URL地址：
[/dashboardUser/{id}](http://127.0.0.1:8082/dashboardUser/{id})
请求方式：
DELETE
请求参数：
无
响应结果：
1

-------------------
**插件管理**
**(1)查询插件** 
URL地址：
[/plugin](http://127.0.0.1:8082/plugin)
请求方式：
GET
请求参数：
|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|name|String|否|-|插件名|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|
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
    "page": {
        "currentPage": 1,
        "prePage": 1,
        "nextPage": 1,
        "pageSize": 15,
        "offset": 0,
        "totalPage": 1,
        "totalCount": 1
    },
    "dataList": [
        {
            "id": "4",
            "code": 20,
            "name": "RATE_LIMITER",
            "enabled": false,
            "dateCreated": "2018-06-23 10:26:37",
            "dateUpdated": "2018-06-13 15:34:48"
        }
    ]
}
```
**(2)明细插件** 
URL地址：
[/plugin/{id}](http://127.0.0.1:8082/plugin/{id})
请求方式：
GET
请求参数：
无
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
    "id": "wFzuv7ZI",
    "pluginId": "1",
    "name": "selector1",
    "matchMode": 0,
    "matchModeName": "and",
    "type": 1,
    "typeName": "custom flow",
    "rank": 1,
    "enabled": true,
    "loged": true,
    "continued": true,
    "selectorConditions": [
        {
            "id": "2oMViT8t",
            "selectorId": "wFzuv7ZI",
            "paramType": "QUERY",
            "paramTypeName": "query",
            "operator": ">",
            "operatorName": "GT",
            "paramName": "param2",
            "paramValue": "value2",
            "dateCreated": "2018-07-29 00:03:33",
            "dateUpdated": "2018-07-29 00:03:33"
        },
        {
            "id": "BzH2VwMA",
            "selectorId": "wFzuv7ZI",
            "paramType": "POST",
            "paramTypeName": "post",
            "operator": "=",
            "operatorName": "EQ",
            "paramName": "param1",
            "paramValue": "value1",
            "dateCreated": "2018-07-29 00:03:33",
            "dateUpdated": "2018-07-29 00:03:33"
        }
    ],
    "dateCreated": "2018-07-29 00:03:31",
    "dateUpdated": "2018-07-29 00:03:31"
}
```
**(3)新增插件** 
URL地址：
[/plugin](http://127.0.0.1:8082/plugin)
请求方式：
POST
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
响应结果：
1
**(4)编辑插件** 
URL地址：
[/plugin/{id}](http://127.0.0.1:8082/plugin/{id})
请求方式：
PUT
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
响应结果：
1
**(5)删除插件**
URL地址：
[/plugin/{id}](http://127.0.0.1:8082/plugin/{id})
请求方式：
DELETE
请求参数：
无
响应结果：
1

-------------------

**选择器管理**
**(1)查询选择器** 
URL地址：
[/selector](http://127.0.0.1:8082/selector)
请求方式：
GET
请求参数：
|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|pluginId|String|否|-|插件ID|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|
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
     * sort rank.
     */
    private Integer rank;

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
    "page": {
        "currentPage": 1,
        "prePage": 1,
        "nextPage": 1,
        "pageSize": 15,
        "offset": 0,
        "totalPage": 1,
        "totalCount": 1
    },
    "dataList": [
        {
            "id": "DHSqDfqC",
            "pluginId": "1",
            "name": "selector1",
            "matchMode": 1,
            "matchModeName": "or",
            "type": 0,
            "typeName": "full flow",
            "rank": 2,
            "enabled": true,
            "loged": false,
            "continued": false,
            "selectorConditions": null,
            "dateCreated": "2018-07-29 07:16:28",
            "dateUpdated": "2018-07-29 07:16:28"
        }
    ]
}
```
**(2)明细选择器** 
URL地址：
[/selector/{id}](http://127.0.0.1:8082/selector/{id})
请求方式：
GET
请求参数：
无
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
     * sort rank.
     */
    private Integer rank;

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
    "id": "DHSqDfqC",
    "pluginId": "1",
    "name": "selector1",
    "matchMode": 1,
    "matchModeName": "or",
    "type": 0,
    "typeName": "full flow",
    "rank": 2,
    "enabled": true,
    "loged": false,
    "continued": false,
    "selectorConditions": [
        {
            "id": "KjXgN0dC",
            "selectorId": "DHSqDfqC",
            "paramType": "POST",
            "paramTypeName": "post",
            "operator": "<",
            "operatorName": "LT",
            "paramName": "parameter1",
            "paramValue": "val1",
            "dateCreated": "2018-07-29 07:16:28",
            "dateUpdated": "2018-07-29 07:16:28"
        },
        {
            "id": "yhDIotQr",
            "selectorId": "DHSqDfqC",
            "paramType": "QUERY",
            "paramTypeName": "query",
            "operator": ">",
            "operatorName": "GT",
            "paramName": "parameter2",
            "paramValue": "val2",
            "dateCreated": "2018-07-29 07:16:28",
            "dateUpdated": "2018-07-29 07:16:28"
        }
    ],
    "dateCreated": "2018-07-29 07:16:28",
    "dateUpdated": "2018-07-29 07:16:28"
}
```
**(3)新增选择器** 
URL地址：
[/selector](http://127.0.0.1:8082/selector)
请求方式：
POST
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
     * sort rank.
     */
    private Integer rank;

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
响应结果：
1
**(4)编辑选择器** 
URL地址：
[/selector/{id}](http://127.0.0.1:8082/selector/{id})
请求方式：
PUT
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
     * sort rank.
     */
    private Integer rank;

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
响应结果：
1
**(5)删除选择器**
URL地址：
[/selector/{id}](http://127.0.0.1:8082/selector/{id})
请求方式：
DELETE
请求参数：
无
响应结果：
1

-------------------
**规则管理**
**(1)查询规则** 
URL地址：
[/rule](http://127.0.0.1:8082/rule)
请求方式：
GET
请求参数：
|参数名|参数类型|是否必须|默认值|备注|
|:----|:-----|:------|:----|:---|
|selectorId|String|否|-|选择器ID|
|currentPage|Integer|否|-|当前页数|
|pageSize|Integer|否|-|页大小|
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
     * sort rank.
     */
    private Integer rank;

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
    "page": {
        "currentPage": 1,
        "prePage": 1,
        "nextPage": 1,
        "pageSize": 15,
        "offset": 0,
        "totalPage": 1,
        "totalCount": 1
    },
    "dataList": [
        {
            "id": "dB4JdjWu",
            "selectorId": "DHSqDfqC",
            "matchMode": 0,
            "matchModeName": "and",
            "name": "rule1",
            "enabled": true,
            "loged": true,
            "rank": 1,
            "handle": "xxxxxx",
            "ruleConditions": null,
            "dateCreated": "2018-07-29 07:24:20",
            "dateUpdated": "2018-07-29 07:24:21"
        }
    ]
}
```
**(2)明细规则** 
URL地址：
[/rule/{id}](http://127.0.0.1:8082/rule/{id})
请求方式：
GET
请求参数：
无
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
     * sort rank.
     */
    private Integer rank;

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
    "id": "dB4JdjWu",
    "selectorId": "DHSqDfqC",
    "matchMode": 0,
    "matchModeName": "and",
    "name": "rule1",
    "enabled": true,
    "loged": true,
    "rank": 1,
    "handle": "xxxxxx",
    "ruleConditions": [
        {
            "id": "L1d8rGVP",
            "ruleId": "dB4JdjWu",
            "paramType": "QUERY",
            "paramTypeName": "query",
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
            "paramType": "POST",
            "paramTypeName": "post",
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
```
**(3)新增规则** 
URL地址：
[/rule](http://127.0.0.1:8082/rule)
请求方式：
POST
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
     * sort rank.
     */
    private Integer rank;

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
响应结果：
1
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
     * sort rank.
     */
    private Integer rank;

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
响应结果：
1
**(5)删除规则**
URL地址：
[/rule/{id}](http://127.0.0.1:8082/rule/{id})
请求方式：
DELETE
请求参数：
无
响应结果：
1

-------------------