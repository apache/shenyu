import React, {Component} from 'react';
import {Modal, Form, Select, Input, Switch} from 'antd';

import {connect} from "dva";
import {getIntlContent} from '../../../utils/IntlUtils';

const FormItem = Form.Item;
const {Option} = Select;
const {TextArea} = Input;
@connect(({global}) = > ({
  platform: global.platform
})
)

class AddModal extends Component {

  handleSubmit = (e) =
> {
  const {
  form
,
  handleOk
,
  id = ''
}

= this.props;
e.preventDefault();
form.validateFieldsAndScroll((err, values) = > {
  if(
!err
)
{
  handleOk({...values, id});
}
})
;
}

render()
{
  let {handleCancel, platform, form, appName = '', serviceName = '', rpcType = '', methodName = '', rpcExt = '', path = '', pathDesc, parameterTypes = '', enabled = true} = this.props;
  let {
    rpcTypeEnums,

  } = platform;

  const {getFieldDecorator} = form;
  const formItemLayout = {
    labelCol: {
      sm: {span: 8},
    },
    wrapperCol: {
      sm: {span: 16},
    },
  };
  return (
    < Modal
  width = {450}
  centered
  title = {getIntlContent("SOUL.META.DATA"
)
}
  visible
  okText = {getIntlContent("SOUL.COMMON.SURE"
)
}
  cancelText = {getIntlContent( "SOUL.COMMON.CALCEL"
)
}
  onOk = {this.handleSubmit}
  onCancel = {handleCancel}
    >
    < Form
  onSubmit = {this.handleSubmit}
  className = "login-form" >
    < FormItem
  label = {getIntlContent("SOUL.AUTH.APPNAME"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('appName', {
      rules: [{required: true, message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.AUTH.APPNAME")`}],
      initialValue: appName,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.AUTH.APPNAME"
  )
  }
    />
  )
  }
<
  /FormItem>

  < FormItem
  label = {getIntlContent("SOUL.META.FUNC.NAME"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('methodName', {
      rules: [{required: true, message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.META.FUNC.NAME")`}],
      initialValue: methodName,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.META.FUNC.NAME"
  )
  }
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {getIntlContent("SOUL.META.PATH"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('path', {
      rules: [{required: true, message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.META.PATH")`}],
      initialValue: path,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.META.PATH"
  )
  }
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {getIntlContent("SOUL.AUTH.PATH.DESCRIBE"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('pathDesc', {
      rules: [{
        required: true,
        message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.AUTH.PATH.DESCRIBE")`
      }],
      initialValue: pathDesc,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.AUTH.PATH.DESCRIBE"
  )
  }
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {`${getIntlContent("SOUL.AUTH.PARAMS")}${getIntlContent("SOUL.COMMON.TYPE")}`
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('parameterTypes', {
      rules: [{
        required: true,
        message: `${getIntlContent("SOUL.AUTH.INPUT")}${getIntlContent("SOUL.AUTH.PARAMS")}${getIntlContent("SOUL.COMMON.TYPE")}`
      }],
      initialValue: parameterTypes,
    })(
    < Input
    placeholder = {`${getIntlContent("SOUL.AUTH.PARAMS")}${getIntlContent("SOUL.COMMON.TYPE")}`
  }
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {`Rpc${getIntlContent("SOUL.META.EXPAND.PARAMS")}`
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('rpcExt', {
      rules: [{message: `${getIntlContent("SOUL.AUTH.INPUT")}Rpc${getIntlContent("SOUL.META.EXPAND.PARAMS")}`}],
      initialValue: rpcExt,
    })(
    < TextArea
    placeholder = {`Rpc${getIntlContent("SOUL.META.EXPAND.PARAMS")}`
  }
    rows = {3}
    />
    // <Input placeholder="rpc扩展参数" />
  )
  }
<
  /FormItem>
  < FormItem
  label = {getIntlContent("SOUL.META.SERVER.INTER"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('serviceName', {
      rules: [{required: true, message: getIntlContent("SOUL.META.INPUTSERVICEINTERFACE")}],
      initialValue: serviceName,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.META.SERVER.INTER"
  )
  }
    />
  )
  }
<
  /FormItem>
  {/* 下拉 */
  }
<
  FormItem
  label = {`Rpc${getIntlContent("SOUL.COMMON.TYPE")}`
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('rpcType', {
      rules: [{required: true, message: getIntlContent("SOUL.META.SELECTRPCTYPE")}],
      initialValue: rpcType,
    })(
    < Select >
    {
      rpcTypeEnums.map(item = > {
          return(
        < Option key = {item.name} value = {`${item.name}`
    } >
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
  {/* 状态 */
  }
  {
    this.props.isShow ?
      (
      < FormItem
      {...formItemLayout}
    label = {getIntlContent("SOUL.SYSTEM.STATUS"
  )
  }
  >
    {
      getFieldDecorator('enabled', {
        initialValue: enabled,
        valuePropName: 'checked',
      })(
      < Switch / >
    )
    }
  <
    /FormItem>
  ) :
    ''
  }
<
  /Form>
  < /Modal>
)
}
}

export default Form.create()(AddModal);
