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
      loadBalance = "",
      timeout = "3000",
      retry = "";

    if (props.handle) {
      const myHandle = JSON.parse(props.handle);
      requestVolumeThreshold = myHandle.requestVolumeThreshold;
      errorThresholdPercentage = myHandle.errorThresholdPercentage;
      maxConcurrentRequests = myHandle.maxConcurrentRequests;
      sleepWindowInMilliseconds = myHandle.sleepWindowInMilliseconds;
      groupKey = myHandle.groupKey;
      commandKey = myHandle.commandKey;
      loadBalance = myHandle.loadBalance;
      timeout = myHandle.timeout;
      retry = myHandle.retry;
    }

    this.state = {
      requestVolumeThreshold,
      errorThresholdPercentage,
      maxConcurrentRequests,
      sleepWindowInMilliseconds,
      groupKey,
      commandKey,
      loadBalance,
      timeout,
      retry
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

  checkConditions = (loadBalance, timeout) =
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

if (!loadBalance) {
  message.destroy();
  message.error(`负载策略不能为空`);
  result = false;
}

if (!timeout) {
  message.destroy();
  message.error(`超时时间不能为空`);
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
    loadBalance,
    timeout,
    upstreamList,
    retry
  } = this.state;

  const myRequestVolumeThreshold =
    requestVolumeThreshold > 0 ? requestVolumeThreshold : "0";
  const myErrorThresholdPercentage =
    errorThresholdPercentage > 0 ? errorThresholdPercentage : "0";
  const myMaxConcurrentRequests =
    maxConcurrentRequests > 0 ? maxConcurrentRequests : "0";
  const mySleepWindowInMilliseconds =
    sleepWindowInMilliseconds > 0 ? sleepWindowInMilliseconds : "0";
  const myRetry = retry > 0 ? retry : "0";

  form.validateFieldsAndScroll((err, values) = > {
    const {name, matchMode, loged, enabled} = values;
  const handle = {
    requestVolumeThreshold: myRequestVolumeThreshold,
    errorThresholdPercentage: myErrorThresholdPercentage,
    maxConcurrentRequests: myMaxConcurrentRequests,
    sleepWindowInMilliseconds: mySleepWindowInMilliseconds,
    groupKey,
    commandKey,
    loadBalance,
    timeout,
    upstreamList,
    retry: myRetry
  };
  if (!err) {
    const submit = this.checkConditions(loadBalance, timeout, upstreamList);

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
      ruleConditions[index].paramName = "/";
    } else {
      this.setState({[key]: false});
    }
  }

  this.setState({ruleConditions});
}
;

divideHandleNumberChange = (index, name, value) =
>
{
  if (/^\d*$/.test(value)) {
    let {upstreamList} = this.state;
    upstreamList[index][name] = value;
    this.setState({upstreamList});
  }
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
  const {ruleConditions, loadBalance, retry} = this.state;

  let {
    matchModeEnums,
    operatorEnums,
    paramTypeEnums,
    loadBalanceEnums
  } = platform;

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
  if (loadBalanceEnums) {
    loadBalanceEnums = loadBalanceEnums.filter(item = > {
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
      width: 105
    }
  }>
  <
    strong > * < /strong>{getIntlContent("SOUL.COMMON.CONDITION")}:
    < /h3>
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
      display: this.state[`paramTypeValueEn${index}`]
        ? "none"
        : "block"
    }
  }
  >
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

    {/* <div className={styles.handleWrap}>
            <div className={styles.header}>
              <h3>Hystrix处理: </h3>
            </div>
            <ul
              className={classnames({
                [styles.handleUl]: true,
                [styles.springUl]: true
              })}
            >
              <li>
                <Input
                  addonBefore={<div>跳闸最小请求数量</div>}
                  value={requestVolumeThreshold}
                  style={{ width: 320 }}
                  placeholder="requestVolumeThreshold"
                  onChange={e => {
                    const value = e.target.value;
                    this.onHandleNumberChange("requestVolumeThreshold", value);
                  }}
                />
              </li>
              <li>
                <Input
                  addonBefore={<div>错误半分比阀值</div>}
                  value={errorThresholdPercentage}
                  style={{ width: 320 }}
                  placeholder="errorThresholdPercentage"
                  onChange={e => {
                    const value = e.target.value;
                    this.onHandleNumberChange(
                      "errorThresholdPercentage",
                      value
                    );
                  }}
                />
              </li>
              <li>
                <Input
                  addonBefore={<div>最大并发量</div>}
                  value={maxConcurrentRequests}
                  style={{ width: 280 }}
                  placeholder="maxConcurrentRequests"
                  onChange={e => {
                    const value = e.target.value;
                    this.onHandleNumberChange("maxConcurrentRequests", value);
                  }}
                />
              </li>
              <li>
                <Input
                  addonBefore={<div>跳闸休眠时间(ms)</div>}
                  value={sleepWindowInMilliseconds}
                  style={{ width: 360 }}
                  placeholder="sleepWindowInMilliseconds"
                  onChange={e => {
                    const value = e.target.value;
                    this.onHandleNumberChange(
                      "sleepWindowInMilliseconds",
                      value
                    );
                  }}
                />
              </li>
              <li>
                <Input
                  addonBefore={<div>分组Key</div>}
                  value={groupKey}
                  style={{ width: 210 }}
                  placeholder="groupKey"
                  onChange={e => {
                    const value = e.target.value;
                    this.onHandleChange("groupKey", value);
                  }}
                />
              </li>
              <li>
                <Input
                  addonBefore={<div>命令Key</div>}
                  value={commandKey}
                  style={{ width: 210 }}
                  placeholder="commandKey"
                  onChange={e => {
                    const value = e.target.value;
                    this.onHandleChange("commandKey", value);
                  }}
                />
              </li>
              <li>
                <Input
                  addonBefore={<div>超时时间(ms)</div>}
                  value={timeout}
                  style={{ width: 210 }}
                  placeholder="timeout(ms)"
                  onChange={e => {
                    const value = e.target.value;
                    this.onHandleNumberChange("timeout", value);
                  }}
                />
              </li>
            </ul>
          </div> */
    }

  <
    div
    className = {styles.handleWrap} >
      < div
    className = {styles.header}
    style = {
    {
      width:100
    }
  }>
  <
    h3 > {getIntlContent("SOUL.COMMON.LOAD"
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
      < li
    className = {styles.loadbalanceLine} >
      < div
    className = {styles.loadText} > {getIntlContent("SOUL.COMMON.LOADSTRATEGY"
  )
  }<
    /div>
    < Select
    onChange = {value = > {
      this.onHandleChange("loadBalance", value);
  }
  }
    value = {loadBalance}
    style = {
    {
      width: 280
    }
  }
    placeholder = "loadBalance"
      >
      {
        loadBalanceEnums.map(item = > {
            return(
          < Option key = {item.name} value = {item.name} >
        {item.name}
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
    addonBefore = { < div > {getIntlContent("SOUL.COMMON.RETRYCOUNT"
  )
  }<
    /div>}
    onChange = {e = > {
      this.onHandleChange("retry", e.target.value);
  }
  }
    placeholder = "retry"
    value = {retry}
    style = {
    {
      width: 330
    }
  }
    />
    < /li>
    < /ul>
    < /div>

    < div
    className = {styles.layout} >
      < FormItem
    style = {
    {
      margin: "0 30px"
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
