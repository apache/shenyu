import React, {Component} from "react";
import {Modal, Button, Form, Input, Switch, message} from "antd";
import styles from "./index.less";
import {getIntlContent} from "../../../utils/IntlUtils";

const FormItem = Form.Item;
const {TextArea} = Input;

class AddModal extends Component {
  constructor(props) {
    super(props);
    const selectorConditions = props.authParamVOList || [{
      "appName": "",
      "appParam": ""
    }];
    this.state = {
      selectorConditions,
    }
  }

  handleSubmit = e =
> {
  console
.

  log(

  "push"
)
  const {
  form
,
  handleOk
,
  id = ""
}

= this.props;
const {selectorConditions} = this.state;
e.preventDefault();
form.validateFieldsAndScroll((err, values) = > {
  if(
!err
)
{
  handleOk({authParamDTOList: selectorConditions, id, ...values});
}
})
;
}
;

conditionChange = (index, name, value) =
>
{
  let {selectorConditions} = this.state;
  selectorConditions[index][name] = value;
  this.setState({selectorConditions});
}
;

handleDelete = index =
>
{
  let {selectorConditions} = this.state;
  if (selectorConditions && selectorConditions.length > 1) {
    selectorConditions.splice(index, 1);
  } else {
    message.destroy();
    message.error("至少有一个条件");
  }
  this.setState({selectorConditions});
}
;

handleAdd = () =
>
{
  let {selectorConditions} = this.state;
  selectorConditions.push({
    appName: "",
    appParam: ""
  });
  this.setState({selectorConditions});
}
;

render()
{
  // console.log("**********")
  // console.log(this.props)
  let {
    handleCancel,
    form,
    appKey = "",
    appSecret = "",
    userId,
    phone,
    extInfo,
    enabled = true
  } = this.props;
  const {getFieldDecorator} = form;
  const formItemLayout = {
    labelCol: {
      sm: {span: 6}
    },
    wrapperCol: {
      sm: {span: 18}
    }
  };
  return (
    < Modal
  width = {550}
  centered
  title = {getIntlContent("SOUL.AUTH.AUTH"
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
  onCancel = {handleCancel}
    >
    < Form
  onSubmit = {this.handleSubmit}
  className = "login-form" >
    < FormItem
  label = {getIntlContent("SOUL.AUTH.APPID"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator("appKey", {
      rules: [{required: true, message: `${getIntlContent("SOUL.AUTH.INPUT")}AppKey`}],
      initialValue: appKey
    })( < Input
    placeholder = {`${getIntlContent("SOUL.AUTH.INPUT")}AppKey`
  }
    />)}
    < /FormItem>
    < FormItem
    label = {getIntlContent("SOUL.AUTH.APPPASSWORD"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("appSecret", {
        rules: [{required: true, message: `${getIntlContent("SOUL.AUTH.INPUT")}AppSecret`}],
        initialValue: appSecret
      })( < Input
      placeholder = {`${getIntlContent("SOUL.AUTH.INPUT")}AppSecret`
    }
      />)}
      < /FormItem>
      < FormItem
      label = {`${getIntlContent("SOUL.SYSTEM.USER")}Id`
    }
      {...
        formItemLayout
      }
    >
      {
        getFieldDecorator("userId", {
          rules: [{required: true, message: getIntlContent("SOUL.AUTH.INPUTUSERID")}],
          initialValue: userId
        })( < Input
        placeholder = {getIntlContent("SOUL.AUTH.INPUTUSERID"
      )
      }
        />)}
        < /FormItem>
        < FormItem
        label = {getIntlContent("SOUL.AUTH.TEL"
      )
      }
        {...
          formItemLayout
        }
      >
        {
          getFieldDecorator("phone", {
            rules: [{required: true, message: getIntlContent("SOUL.AUTH.TELPHONE")}],
            initialValue: phone
          })( < Input
          placeholder = {getIntlContent("SOUL.AUTH.TELPHONE"
        )
        }
          />)}
          < /FormItem>
          < FormItem
          label = {getIntlContent("SOUL.AUTH.EXPANDINFO"
        )
        }
          {...
            formItemLayout
          }
        >
          {
            getFieldDecorator("extInfo", {
              rules: [{message: getIntlContent("SOUL.AUTH.EXPANDINFO")}],
              initialValue: extInfo
            })( < TextArea
            placeholder = {getIntlContent("SOUL.AUTH.INPUTEXPANDINFO"
          )
          }
            rows = {3}
            />)}
            < /FormItem>

            {/* 添加删除行 */
            }
          <
            div
            className = {styles.condition} >
              {/* 输入框左侧标题
            <h3 className={styles.header}>
              authParamVOList:{" "}
            </h3> */
              }
              < div >
              {
                this.state.selectorConditions.map((item, index) = > {
                    return(
                  < ul key = {index} >
                < li >
                < div className = {styles.title} > {getIntlContent("SOUL.AUTH.APPNAME"
          )
          }:<
            /div>
            < /li>
            < li >
            < Input
            onChange = {e = > {this.conditionChange(index, "appName", e.target.value)}
          }
            value = {item.appName}
            className = {styles.appName}
            />
            < /li>
            < li >
            < div
            className = {styles.title} > {getIntlContent("SOUL.AUTH.PARAMS"
          )
          }:<
            /div>
            < /li>
            < li >
            < TextArea
            rows = {3}
            onChange = {e = > {this.conditionChange(index, "appParam", e.target.value);
          }
          }
            value = {item.appParam}
            className = {styles.appParam}
            />
            < /li>
            < li >
            < Button
            className = {styles.btn}
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
          })
          }
          <
            /div>
            < Button
            onClick = {this.handleAdd}
            className = {styles.btn}
            type = "primary" >
              {getIntlContent("SOUL.COMMON.ADD"
          )
          }
          <
            /Button>
            < /div>


            < FormItem
            {...
              formItemLayout
            }
            label = {getIntlContent("SOUL.SYSTEM.STATUS"
          )
          }>
            {
              getFieldDecorator("enabled", {
                initialValue: enabled,
                valuePropName: "checked"
              })( < Switch / >
            )
            }
          <
            /FormItem>
            < /Form>
            < /Modal>
          )
            ;
          }
        }

      export default
        Form.create()(AddModal);
