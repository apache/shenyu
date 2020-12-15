import React, {Component} from 'react';
import {Modal, Form, Input, Switch} from 'antd';

import {connect} from "dva";
import {getIntlContent} from '../../../utils/IntlUtils';

const FormItem = Form.Item;
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
  let {handleCancel, form, type = '', dictCode = '', dictName = '', dictValue = '', desc = '', sort = '0', enabled = true} = this.props;

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
  title = {getIntlContent("SOUL.DIC"
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
  label = {getIntlContent("SOUL.DIC.TYPE"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('type', {
      rules: [{required: true, message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.DIC.TYPE")`}],
      initialValue: type,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.DIC.TYPE"
  )
  }
    />
  )
  }
<
  /FormItem>

  < FormItem
  label = {getIntlContent("SOUL.DIC.CODE"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('dictCode', {
      rules: [{required: true, message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.DIC.CODE")`}],
      initialValue: dictCode,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.DIC.CODE"
  )
  }
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {getIntlContent("SOUL.DIC.NAME"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('dictName', {
      rules: [{required: true, message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.DIC.NAME")`}],
      initialValue: dictName,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.DIC.NAME"
  )
  }
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {getIntlContent("SOUL.DIC.VALUE"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('dictValue', {
      rules: [{required: true, message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.DIC.VALUE")`}],
      initialValue: dictValue,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.DIC.VALUE"
  )
  }
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {getIntlContent("SOUL.DIC.DESCRIBE"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('desc', {
      initialValue: desc,
    })(
    < TextArea
    placeholder = {getIntlContent("SOUL.DIC.DESCRIBE"
  )
  }
    rows = {3}
    />
  )
  }
<
  /FormItem>
  < FormItem
  label = {getIntlContent("SOUL.PLUGIN.SORT"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('sort', {
      rules: [
        {
          required: true,
          message: `getIntlContent("SOUL.AUTH.INPUT") + getIntlContent("SOUL.PLUGIN.SORT")`
        }
      ],
      initialValue: sort,
    })(
    < Input
    placeholder = {getIntlContent("SOUL.PLUGIN.SORT"
  )
  }
    type = "number" / >
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
