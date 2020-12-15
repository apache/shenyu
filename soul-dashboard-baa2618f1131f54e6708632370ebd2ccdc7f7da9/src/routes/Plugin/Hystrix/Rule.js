import React, {Component} from "react";
import {Modal, Form, Select, Input, Switch, Button, message} from "antd";
import {connect} from "dva";
import classnames from "classnames";
import styles from "../index.less";
import {getIntlContent} from "../../../utils/IntlUtils";

const FormItem = Form.Item;
const {Option} = Select;

@connect(({global}) = > ({
  platform: global.platform
})
)

class AddModal extends Component {
  constructor(props) {
    super(props);
    const ruleConditions = props.ruleConditions || [
      {
        paramType: "uri",
        operator: "=",
        paramName: "/",
        paramValue: ""
      }
    ];
    let requestVolumeThreshold = "20",
      errorThresholdPercentage = "50",
      maxConcurrentRequests = "100",
      sleepWindowInMilliseconds = "5000",
      groupKey = "",
      commandKey = "",
      callBackUri = "",
      executionIsolationStrategy = 1,
      hystrixThreadPoolConfig = {
        coreSize: 10,
        maximumSize: 10,
        maxQueueSize: 12
      }
    ;
    if (props.handle) {
      const myHandle = JSON.parse(props.handle);
      requestVolumeThreshold = myHandle.requestVolumeThreshold;
      errorThresholdPercentage = myHandle.errorThresholdPercentage;
      maxConcurrentRequests = myHandle.maxConcurrentRequests;
      sleepWindowInMilliseconds = myHandle.sleepWindowInMilliseconds;
      groupKey = myHandle.groupKey;
      commandKey = myHandle.commandKey;
      if (typeof (myHandle.executionIsolationStrategy) !== 'undefined') {
        executionIsolationStrategy = myHandle.executionIsolationStrategy;
      }
      if (myHandle.hystrixThreadPoolConfig) {
        hystrixThreadPoolConfig = myHandle.hystrixThreadPoolConfig;
      }
      callBackUri = myHandle.callBackUri;
    }
    this.state = {
      requestVolumeThreshold,
      errorThresholdPercentage,
      maxConcurrentRequests,
      sleepWindowInMilliseconds,
      groupKey,
      commandKey,
      executionIsolationStrategy,
      hystrixThreadPoolConfig,
      callBackUri
    };

    ruleConditions.forEach((item, index) = > {
      const {paramType} = item;

    let key = `paramTypeValueEn${index}`;
    if (paramType === "uri" || paramType === "host" || paramType === "ip") {
      this.state[key] = true;
      ruleConditions[index].paramName = "/";
    } else {
      this.state[key] = false;
    }
  })
    ;

    this.state.ruleConditions = ruleConditions;
  }

  checkConditions = (permission, statusCode) =
> {
  let {
  ruleConditions
}

= this.state;
let result = true;
if (ruleConditions) {
  ruleConditions.forEach((item, index) = > {
    const {paramType, operator, paramName, paramValue} = item;
  if (!paramType || !operator || !paramValue) {
    message.destroy();
    message.error(`第${index + 1}行条件不完整`);
    result = false;
  }
  if (paramType === "uri" || paramType === "host" || paramType === "ip") {
    // aaa
  } else {
    // eslint-disable-next-line no-lonely-if
    if (!paramName) {
      message.destroy();
      message.error(`第${index + 1}行条件不完整`);
      result = false;
    }
  }
})
  ;
} else {
  message.destroy();
  message.error(`条件不完整`);
  result = false;
}

if (permission === "reject" && !statusCode) {
  message.destroy();
  message.error(`请填写状态码`);
  result = false;
}

return result;
}
;

handleSubmit = e =
>
{
  e.preventDefault();
  const {form, handleOk} = this.props;
  const {
    ruleConditions,
    requestVolumeThreshold,
    errorThresholdPercentage,
    maxConcurrentRequests,
    sleepWindowInMilliseconds,
    groupKey,
    commandKey,
    executionIsolationStrategy,
    hystrixThreadPoolConfig,
    callBackUri
  } = this.state;
  const myRequestVolumeThreshold =
    requestVolumeThreshold > 0 ? requestVolumeThreshold : "0";
  const myErrorThresholdPercentage =
    errorThresholdPercentage > 0 ? errorThresholdPercentage : "0";
  const myMaxConcurrentRequests =
    maxConcurrentRequests > 0 ? maxConcurrentRequests : "0";
  const mySleepWindowInMilliseconds =
    sleepWindowInMilliseconds > 0 ? sleepWindowInMilliseconds : "0";
  const myCoreSize = hystrixThreadPoolConfig.coreSize > 0 ? hystrixThreadPoolConfig.coreSize : "0";
  const myMaximumSize = hystrixThreadPoolConfig.maximumSize > 0 ? hystrixThreadPoolConfig.maximumSize : "0";
  const myMaxQueueSize = hystrixThreadPoolConfig.maxQueueSize > 0 ? hystrixThreadPoolConfig.maxQueueSize : "0";
  form.validateFieldsAndScroll((err, values) = > {
    const {
      name,
      matchMode,
      permission,
      statusCode,
      loged,
      enabled
    } = values;
  const handle = {
    permission,
    statusCode,
    requestVolumeThreshold: myRequestVolumeThreshold,
    errorThresholdPercentage: myErrorThresholdPercentage,
    sleepWindowInMilliseconds: mySleepWindowInMilliseconds,
    executionIsolationStrategy,
    callBackUri,
    groupKey,
    commandKey
  };
  if (handle.executionIsolationStrategy === 1) {
    handle.maxConcurrentRequests = myMaxConcurrentRequests;
  } else {
    handle.hystrixThreadPoolConfig = {
      coreSize: myCoreSize,
      maximumSize: myMaximumSize,
      maxQueueSize: myMaxQueueSize
    }
  }

  if (!err) {
    const submit = this.checkConditions(permission, statusCode);
    if (submit) {
      handleOk({
        name,
        matchMode,
        handle: JSON.stringify(handle),
        loged,
        enabled,
        sort: Number(values.sort),
        ruleConditions
      });
    }
  }
})
  ;
}
;

handleAdd = () =
>
{
  let {ruleConditions} = this.state;
  ruleConditions.push({
    paramType: "uri",
    operator: "=",
    paramName: "/",
    paramValue: ""
  });
  this.setState({ruleConditions}, () = > {
    let len = ruleConditions.length || 0;
  let key = `paramTypeValueEn${len - 1}`;
  this.setState({[key]: true});
})
  ;
}
;

handleDelete = index =
>
{
  let {ruleConditions} = this.state;
  if (ruleConditions && ruleConditions.length > 1) {
    ruleConditions.splice(index, 1);
  } else {
    message.destroy();
    message.error("至少有一个条件");
  }
  this.setState({ruleConditions});
}
;

conditionChange = (index, name, value) =
>
{
  let {ruleConditions} = this.state;
  ruleConditions[index][name] = value;
  if (name === "paramType") {
    let key = `paramTypeValueEn${index}`;
    if (value === "uri" || value === "host" || value === "ip") {
      this.setState({[key]: true});
      ruleConditions[index].paramName = "";
    } else {
      this.setState({[key]: false});
    }
  }

  this.setState({ruleConditions});
}
;

onHandleChange = (key, value) =
>
{
  this.setState({[key]: value});
}
;

onHandleNumberChange = (key, value) =
>
{
  if (/^\d*$/.test(value)) {
    this.setState({[key]: value});
  }
}
;

render()
{
  let {
    onCancel,
    form,
    platform,
    name = "",
    matchMode = "",
    loged = true,
    enabled = true,
    sort = ""
  } = this.props;
  const {
    ruleConditions,
    requestVolumeThreshold,
    errorThresholdPercentage,
    maxConcurrentRequests,
    sleepWindowInMilliseconds,
    groupKey,
    commandKey,
    executionIsolationStrategy,
    hystrixThreadPoolConfig,
    callBackUri
  } = this.state;
  const labelWidth = 175

  let {matchModeEnums, operatorEnums, paramTypeEnums, hystrixIsolationModeEnums} = platform;

  if (operatorEnums) {
    operatorEnums = operatorEnums.filter(item = > {
      return item.support === true;
  })
    ;
  }

  if (paramTypeEnums) {
    paramTypeEnums = paramTypeEnums.filter(item = > {
      return item.support === true;
  })
    ;
  }

  const {getFieldDecorator} = form;
  const formItemLayout = {
    labelCol: {
      sm: {span: 3}
    },
    wrapperCol: {
      sm: {span: 21}
    }
  };
  const formCheckLayout = {
    labelCol: {
      sm: {span: 18}
    },
    wrapperCol: {
      sm: {span: 4}
    }
  };
  return (
    < Modal
  width = {900}
  centered
  title = {getIntlContent("SOUL.RULE.NAME"
)
}
  visible
  okText = {getIntlContent("SOUL.COMMON.SURE"
)
}
  cancelText = {getIntlContent("SOUL.COMMON.CALCEL"
)
}
  onOk = {this.handleSubmit}
  onCancel = {onCancel}
    >
    < Form
  onSubmit = {this.handleSubmit}
  className = "login-form" >
    < FormItem
  label = {getIntlContent("SOUL.PLUGIN.SELECTOR.LIST.COLUMN.NAME"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator("name", {
      rules: [{required: true, message: getIntlContent("SOUL.COMMON.INPUTNAME")}],
      initialValue: name
    })( < Input
    placeholder = {getIntlContent("SOUL.PLUGIN.SELECTOR.LIST.COLUMN.NAME"
  )
  }
    />)}
    < /FormItem>
    < FormItem
    label = {getIntlContent("SOUL.COMMON.MATCHTYPE"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("matchMode", {
        rules: [{required: true, message: getIntlContent("SOUL.COMMON.INPUTMATCHTYPE")}],
        initialValue: matchMode
      })(
      < Select >
      {
        matchModeEnums.map(item = > {
            return(
          < Option key = {item.code} value = {item.code} >
        {item.name}
        < /Option>
    )
      ;
    })
    }
    <
      /Select>
    )
    }
  <
    /FormItem>
    < div
    className = {styles.ruleConditions} >
      < h3
    className = {styles.header}
    style = {
    {
      width:105
    }
  }>
  <
    strong > * < /strong>
    {
      getIntlContent("SOUL.COMMON.CONDITION")
    }
  :
  <
    /h3>
    < div
    className = {styles.content} >
      {
        ruleConditions.map((item, index) = > {
            return(
          < ul key = {index} >
        < li >
        < Select
        onChange = {value = > {
          this.conditionChange(index, "paramType", value);
  }
  }
    value = {item.paramType}
    style = {
    {
      width: 90
    }
  }
  >
    {
      paramTypeEnums.map(type = > {
        return(
      < Option
      key = {type.name}
      value = {type.name} >
        {type.name}
        < /Option>
    )
      ;
    })
    }
  <
    /Select>
    < /li>
    < li
    style = {
    {
      display: this.state[`paramTypeValueEn${index}`] ? 'none' : 'block'
    }
  }>
  <
    Input
    onChange = {e = > {
      this.conditionChange(
        index,
        "paramName",
        e.target.value
      );
  }
  }
    value = {item.paramName}
    style = {
    {
      width: 100
    }
  }
    />
    < /li>
    < li >
    < Select
    onChange = {value = > {
      this.conditionChange(index, "operator", value);
  }
  }
    value = {item.operator}
    style = {
    {
      width: 80
    }
  }
  >
    {
      operatorEnums.map(opearte = > {
        return(
      < Option
      key = {opearte.name}
      value = {opearte.name} >
        {opearte.name}
        < /Option>
    )
      ;
    })
    }
  <
    /Select>
    < /li>

    < li >
    < Input
    onChange = {e = > {
      this.conditionChange(
        index,
        "paramValue",
        e.target.value
      );
  }
  }
    value = {item.paramValue}
    style = {
    {
      width: 280
    }
  }
    />
    < /li>
    < li >
    < Button
    type = "danger"
    onClick = {() =
  >
    {
      this.handleDelete(index);
    }
  }
  >
    {
      getIntlContent("SOUL.COMMON.DELETE.NAME")
    }
  <
    /Button>
    < /li>
    < /ul>
  )
    ;
  })
  }
  <
    /div>
    < div >
    < Button
    onClick = {this.handleAdd}
    type = "primary" >
      {getIntlContent("SOUL.COMMON.ADD"
  )
  }
  <
    /Button>
    < /div>
    < /div>
    {/* <FormItem label="处理" {...formItemLayout}>
            {getFieldDecorator("permission", {
              initialValue: permission,
              rules: [{ required: true, message: "请选择处理" }]
            })(
              <Select>
                {wafEnums.map(item => {
                  return (
                    <Option key={item.name} value={item.name}>
                      {item.name}
                    </Option>
                  );
                })}
              </Select>
            )}
          </FormItem> */
    }
  <
    FormItem
    label = {getIntlContent("SOUL.HYSTRIX.LSOLATION.MODE"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("executionIsolationStrategy", {
        rules: [{required: true, message: getIntlContent("SOUL.HYSTRIX.LSOLATION.SELECT")}],
        initialValue: executionIsolationStrategy
      })(
      < Select
      onChange = {value = > {
        this.onHandleChange("executionIsolationStrategy", value);
    }
    }
    >
      {
        hystrixIsolationModeEnums.map(item = > {
          return(
        < Option
        key = {item.code}
        value = {item.code} >
          {item.name}
          < /Option>
      )
        ;
      })
      }
    <
      /Select>
    )
    }
  <
    /FormItem>
    < div
    className = {styles.handleWrap} >
      < div
    className = {styles.header} >
      < h3 > {getIntlContent("SOUL.COMMON.DEAL"
  )
  }: <
    /h3>
    < /div>
    < ul
    className = {
        classnames({
                     [styles.handleUl]: true,
                     [styles.springUl]: true
                   })
      }
      >
      < li >
      < Input
    addonBefore = { < div
    style = {
    {
      width: labelWidth
    }
  }>
    {
      getIntlContent("SOUL.HYSTRIX.TRIPPING.REQUEST.NUMBER")
    }
  <
    /div>}
    value = {requestVolumeThreshold}
    placeholder = "requestVolumeThreshold"
    onChange = {e = > {
      const value = e.target.value;
    this.onHandleNumberChange("requestVolumeThreshold", value);
  }
  }
    />
    < /li>
    < li >
    < Input
    addonBefore = { < div
    style = {
    {
      width: labelWidth
    }
  }>
    {
      getIntlContent("SOUL.HYSTRIX.ERROR.PERCENT")
    }
  <
    /div>}
    value = {errorThresholdPercentage}
    placeholder = "errorThresholdPercentage"
    onChange = {e = > {
      const value = e.target.value;
    this.onHandleNumberChange(
      "errorThresholdPercentage",
      value
    );
  }
  }
    />
    < /li>
    {
      this.state.executionIsolationStrategy === 1 && (
      < li >
      < Input
      addonBefore = { < div
      style = {
      {
        width: labelWidth
      }
    }>
      {
        getIntlContent("SOUL.HYSTRIX.MAX.CONCURRENCY")
      }
    <
      /div>}
      value = {maxConcurrentRequests}
      placeholder = "maxConcurrentRequests"
      onChange = {e = > {
        const value = e.target.value;
      this.onHandleNumberChange("maxConcurrentRequests", value);
    }
    }
      />
      < /li>
    )
    }
  <
    li >
    < Input
    addonBefore = { < div
    style = {
    {
      width: labelWidth
    }
  }>
    {
      getIntlContent("SOUL.HYSTRIX.TRIPPING.SLEEPTIME")
    }
  <
    /div>}
    value = {sleepWindowInMilliseconds}
    placeholder = "sleepWindowInMilliseconds"
    onChange = {e = > {
      const value = e.target.value;
    this.onHandleNumberChange(
      "sleepWindowInMilliseconds",
      value
    );
  }
  }
    />
    < /li>
    < li >
    < Input
    addonBefore = { < div
    style = {
    {
      width: labelWidth
    }
  }>
    {
      getIntlContent("SOUL.HYSTRIX.GROUPKEY")
    }
  <
    /div>}
    value = {groupKey}
    placeholder = "GroupKey"
    onChange = {e = > {
      const value = e.target.value;
    this.onHandleChange("groupKey", value);
  }
  }
    />
    < /li>
    < li >
    < Input
    addonBefore = { < div
    style = {
    {
      width: labelWidth
    }
  }>
    {
      getIntlContent("SOUL.HYSTRIX.FAILEDDEMOTION")
    }
  <
    /div>}
    value = {callBackUri}
    placeholder = {getIntlContent("SOUL.HYSTRIX.FAILEDCALLBACK"
  )
  }
    onChange = {e = > {
      const value = e.target.value;
    this.onHandleChange("callBackUri", value);
  }
  }
    />
    < /li>
    < li >
    < Input
    addonBefore = { < div
    style = {
    {
      width: labelWidth
    }
  }>
    {
      getIntlContent("SOUL.HYSTRIX.COMMANDKEY")
    }
  <
    /div>}
    value = {commandKey}
    placeholder = "CommandKey"
    onChange = {e = > {
      const value = e.target.value;
    this.onHandleChange("commandKey", value);
  }
  }
    />
    < /li>
    {
      this.state.executionIsolationStrategy === 0 && (
      < li >
      < Input
      addonBefore = { < div
      style = {
      {
        width: labelWidth
      }
    }>
      {
        getIntlContent("SOUL.HYSTRIX.CORETHREADSIZE")
      }
    <
      /div>}
      value = {hystrixThreadPoolConfig.coreSize}
      placeholder = {getIntlContent("SOUL.HYSTRIX.CORENUM"
    )
    }
      onChange = {e = > {
        const value = e.target.value;
      hystrixThreadPoolConfig.coreSize = value;
      this.setState({hystrixThreadPoolConfig})
    }
    }
      />
      < /li>
    )
    }

    {
      this.state.executionIsolationStrategy === 0 && (
      < li >
      < Input
      addonBefore = { < div
      style = {
      {
        width: labelWidth
      }
    }>
      {
        getIntlContent("SOUL.HYSTRIX.MAXSIZE")
      }
    <
      /div>}
      value = {hystrixThreadPoolConfig.maximumSize}
      placeholder = {getIntlContent("SOUL.HYSTRIX.MAXTHREADNUM"
    )
    }
      onChange = {e = > {
        const value = e.target.value;
      hystrixThreadPoolConfig.maximumSize = value;
      this.setState({hystrixThreadPoolConfig})
    }
    }
      />
      < /li>
    )
    }
    {
      this.state.executionIsolationStrategy === 0 && (
      < li >
      < Input
      addonBefore = { < div
      style = {
      {
        width: labelWidth
      }
    }>
      {
        getIntlContent("SOUL.HYSTRIX.MAXTHREADQUEUE")
      }
    <
      /div>}
      value = {hystrixThreadPoolConfig.maxQueueSize}
      placeholder = {getIntlContent("SOUL.HYSTRIX.MAXTHREAD"
    )
    }
      onChange = {e = > {
        const value = e.target.value;
      hystrixThreadPoolConfig.maxQueueSize = value;
      this.setState({hystrixThreadPoolConfig})
    }
    }
      />
      < /li>
    )
    }


  <
    /ul>

    < /div>

    {/* <FormItem label="状态码" {...formItemLayout}>
            {getFieldDecorator("statusCode", {
              initialValue: statusCode,
              rules: [
                {
                  pattern: /^\d*$/,
                  message: "请输入数字"
                }
              ]
            })(<Input placeholder="请输入状态码" />)}
          </FormItem> */
    }
  <
    div
    className = {styles.layout} >
      < FormItem
    style = {
    {
      margin: "0 20px"
    }
  }
    {...
      formCheckLayout
    }
    label = {getIntlContent("SOUL.SELECTOR.PRINTLOG"
  )
  }
  >
    {
      getFieldDecorator("loged", {
        initialValue: loged,
        valuePropName: "checked",
        rules: [{required: true}]
      })( < Switch / >
    )
    }
  <
    /FormItem>
    < FormItem
    {...
      formCheckLayout
    }
    label = {getIntlContent("SOUL.SELECTOR.WHETHEROPEN"
  )
  }>
    {
      getFieldDecorator("enabled", {
        initialValue: enabled,
        valuePropName: "checked",
        rules: [{required: true}]
      })( < Switch / >
    )
    }
  <
    /FormItem>
    < /div>

    < FormItem
    label = {getIntlContent("SOUL.SELECTOR.EXEORDER"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("sort", {
        initialValue: sort,
        rules: [
          {
            required: true,
            message: getIntlContent("SOUL.SELECTOR.INPUTNUMBER")
          },
          {
            pattern: /^([1-9][0-9]{0,1}|100)$/,
            message: getIntlContent("SOUL.SELECTOR.INPUTNUMBER")
          }
        ]
      })( < Input
      placeholder = {getIntlContent("SOUL.SELECTOR.INPUTORDER"
    )
    }
      />)}
      < /FormItem>
      < /Form>
      < /Modal>
    )
      ;
    }
  }

  export default
    Form.create()(AddModal);
