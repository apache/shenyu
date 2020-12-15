import React, {Component, Fragment} from "react";
import {Modal, Form, Switch, Input, Select, Divider} from "antd";
import {connect} from "dva";
import {getIntlContent} from "../../../utils/IntlUtils";

const {Option} = Select;
const {TextArea} = Input;
const FormItem = Form.Item;

@connect(({global}) = > ({
  platform: global.platform
})
)

class AddModal extends Component {
  handleSubmit = e =
> {
  const {
  form
,
  handleOk
,
  id = ""
}

= this.props;
e.preventDefault();
form.validateFieldsAndScroll((err, values) = > {
  if(
!err
)
{

  let {name, role, enabled, master, mode, url, password, config} = values;
  if (name === 'rate_limiter') {
    config = JSON.stringify({master, mode, url, password})
  }
  handleOk({name, role, enabled, config, id});
}
})
;
}
;

render()
{
  let {handleCancel, platform, form, config, name, enabled = true, role = "1", id} = this.props;

  let disable = false;
  if (id) {
    disable = true;
  } else {
    role = "1";
  }

  const {getFieldDecorator} = form;

  const formItemLayout = {
    labelCol: {
      sm: {span: 5}
    },
    wrapperCol: {
      sm: {span: 19}
    }
  };
  let {
    redisModeEnums,
  } = platform;


  let configWrap = ''


  if (name === 'rate_limiter') {
    try {
      config = JSON.parse(config)
    } catch (error) {
      config = {}
    }

    const ruleMaster = this.props.form.getFieldValue('mode')
    const reMaster = !!((ruleMaster === 'cluster' || ruleMaster === 'sentinel'))
    configWrap = (
      < Fragment >
      < Divider > redis
    {
      getIntlContent("SOUL.COMMON.SETTING")
    }
  <
    /Divider>
    < FormItem
    label = {getIntlContent("SOUL.PLUGIN.WAY"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("mode", {
        rules: [{required: true, message: getIntlContent("SOUL.PLUGIN.SELECT.WAY")}],
        initialValue: config.mode
      })(
      < Select >
      {
        redisModeEnums.map(item = > {
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
    )
    }
  <
    /FormItem>
    < FormItem
    label = "master"
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("master", {
        rules: reMaster ? [{required: true, message: getIntlContent("SOUL.PLUGIN.INPUT.MASTER")}] : [],
        initialValue: config.master,
      })(
      < Input
      placeholder = {getIntlContent("SOUL.PLUGIN.INPUT.MASTER"
    )
    }
      />
    )
    }
  <
    /FormItem>

    < FormItem
    label = "URL"
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("url", {
        rules: [{required: true, message: getIntlContent("SOUL.PLUGIN.INPUT.URL")}],
        initialValue: config.url,
      })(
      < TextArea
      placeholder = {getIntlContent("SOUL.PLUGIN.INPUT"
    )
    }
      rows = {3}
      />
    )
    }
  <
    /FormItem>
    < FormItem
    label = {getIntlContent("SOUL.SYSTEM.PASSWORD"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("password", {
        rules: [],
        initialValue: config.password,
      })(
      < Input
      placeholder = {getIntlContent("SOUL.SYSTEM.USER.PASSWORD"
    )
    }
      />
    )
    }
  <
    /FormItem>

    < Divider / >
    < /Fragment>
  )
  } else {
    configWrap = (
      < FormItem
    label = {getIntlContent("SOUL.COMMON.SETTING"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("config", {
        initialValue: config
      })(
      < TextArea
      placeholder = {getIntlContent("SOUL.PLUGIN.INPUTSETTING"
    )
    }
      rows = {4}
      />
    )
    }
  <
    /FormItem>
  )
  }

  return (
    < Modal
  width = {520}
  centered
  title = {getIntlContent("SOUL.PLUGIN"
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
  label = {getIntlContent("SOUL.PLUGIN"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator("name", {
      rules: [{required: true, message: getIntlContent("SOUL.PLUGIN.SELECT")}],
      initialValue: name,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.PLUGIN.PLUGIN.NAME"
  )
  }
    disabled = {disable}
    />
  )
  }
<
  /FormItem>
  {
    configWrap
  }
<
  FormItem
  label = {getIntlContent("SOUL.SYSTEM.ROLE"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('role', {
      rules: [{required: true, message: getIntlContent("SOUL.SYSTEM.SELECTROLE")}],
      initialValue: `${role}`,
    })(
    < Select
    disabled >
    < Option
    value = "0" > {getIntlContent("SOUL.SYSTEM.SYSTEM"
  )
  }<
    /Option>
    < Option
    value = "1" > {getIntlContent("SOUL.SYSTEM.CUSTOM"
  )
  }<
    /Option>
    < /Select>
  )
  }
<
  /FormItem>

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

export default Form.create()(AddModal);
