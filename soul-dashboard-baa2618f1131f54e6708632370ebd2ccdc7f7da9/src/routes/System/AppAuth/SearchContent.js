import React from "react";
import {Form, Input, Button} from 'antd';
import {getIntlContent} from "../../../utils/IntlUtils";

class InlineSearch extends React.Component {

  handleSubmit = e =
> {
  e
.

  preventDefault();

  const
  searchCont = this.props.form.getFieldsValue()
  this
.
  props
.

  onClick(searchCont)

  // console.log(searchCont)

};

render()
{
  const {getFieldDecorator} = this.props.form;
  return (
    < Form
  layout = "inline"
  onSubmit = {this.handleSubmit} >
    < Form.Item >
    {getFieldDecorator('appKey',
  {
    initialValue:null
  }
)
  (
  < Input
  placeholder = {getIntlContent("SOUL.AUTH.INPUTAPPKEY"
)
}
  />,
)
}
<
  /Form.Item>
  < Form.Item >
  {getFieldDecorator('phone',
  {
    initialValue:null
  }
)
  (
  < Input
  type = "phone"
  placeholder = {getIntlContent("SOUL.AUTH.TELPHONE"
)
}
  />,
)
}
<
  /Form.Item>
  < Form.Item >
  < Button
  type = "primary"
  htmlType = "submit" >
    {getIntlContent("SOUL.SYSTEM.SEARCH"
)
}
<
  /Button>
  < /Form.Item>
  < /Form>
)
  ;
}
}
const SearchContent = Form.create({})(InlineSearch);
export default SearchContent
