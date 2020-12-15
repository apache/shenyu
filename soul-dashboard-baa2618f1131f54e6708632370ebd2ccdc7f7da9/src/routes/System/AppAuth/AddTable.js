import React, {Component} from "react";
import {Modal, Form, Select, Input, Table, Button, Popconfirm} from 'antd';
import {getIntlContent} from "../../../utils/IntlUtils";

const FormItem = Form.Item;
const {Option} = Select;

class AddTable extends Component {
  constructor(props) {
    super(props);
    this.columns = [
      {
        title: getIntlContent("SOUL.AUTH.RESOUCE.PATH"),
        dataIndex: 'path',
        editable: 'true',
        render: (text, record) = > (
      < Input
      placeholder = "/"
      value = {text}
      onChange = {(e) = > this.handleTableInput({path: e.target.value}, record)
  }
    />
  )
  },
    {
      title: getIntlContent("SOUL.AUTH.PATH.DESCRIBE"),
        dataIndex
    :
      'pathDesc',
        editable
    :
      'true',
        render
    :
      (text, record) =
    >
      (
      < Input
      value = {text}
      onChange = {(e) =
    >
      this.handleTableInput({pathDesc: e.target.value}, record)
    }
      />
    )
    }
  ,
    {
      title: getIntlContent("SOUL.COMMON.OPERAT"),
        dataIndex
    :
      'operation',
        render
    :
      (text, record) =
    >
      this.state.allData.length > 1 ? (
        < Popconfirm title = {getIntlContent("SOUL.COMMON.DELETE"
    )
    }
      onConfirm = {() =
    >
      this.handleDelete(record.key)
    }>
    <
      a > {getIntlContent("SOUL.COMMON.DELETE.NAME"
    )
    }<
      /a>
      < /Popconfirm>
    ) :
      null,
    }
  ,
  ]
    ;
    this.state = {
      selectedRowKeys: [], // Check here to configure the default column
      tableInput: [],
      allData: [],
      newSelectInput: []
    };
  }

  //   下拉框事件
  handleChange = (value) =
> {
  this
.
  props
.
  form
.

  setFieldsValue({appName: value});

  this
.

  setState({
             allData:

(
  this
.
  props
.
  metaGroup
  [value]
===
  undefined
||
  this
.
  props
.
  metaGroup
  [value]
===
  null
) ?
  [{
    key: 0,
    path: '',
    pathDesc: '',
  }]: this
.
  props
.
  metaGroup
  [value]
}

)
;
}

handleSubmit = e =
>
{
  const {form, handleOk} = this.props;
  const {selectedRowKeys, allData} = this.state;
  const pathList = allData.filter(item = > {
    let cur = item.id === undefined ? item.key : item.id
    return selectedRowKeys.includes(cur)
  }
).
  map(item = > {
    return item.path
  }
)
  ;
  e.preventDefault();
  form.validateFieldsAndScroll((err, values) = > {
    if(
  !err
)
  {
    handleOk({pathList, ...values});
  }
})
  ;
}
;

onSelectChange = selectedRowKeys =
>
{
  this.setState({selectedRowKeys});
}

handleTableInput = (value, record) =
>
{
  // eslint-disable-next-line guard-for-in
  for (let i in value) {
    record[i] = value[i];// 这一句是必须的，不然状态无法更改
    this.setState({
      // eslint-disable-next-line react/no-access-state-in-setstate
      tableInput: this.state.tableInput.map((item) = > item.key === record.key ? {...item, [i]: value[i]} : item)
  })
  }
}

handleAddTd = () =
>
{
  const allData = this.state.allData

  const newData = {
    key: allData.length,
    path: '',
    pathDesc: '',
  };

  this.setState({
    allData: [...allData, newData
]
})
}

handleDelete = (key) =
>
{
  // const allData = [...this.state.allData];
  this.setState((prev) = > ({
    allData: prev.allData.filter((item) = > item.key !== key),
}))
  ;
}
;

onSearchSelect = (value, index) =
>
{
  const {newSelectInput} = this.state;
  const flag = !!value
  if (flag) {
    newSelectInput[index] = value || '';
    this.setState({newSelectInput});
  }
}

onBlurSelect = (index) =
>
{
  const {newSelectInput} = this.state;
  const value = newSelectInput[index];
  const flag = !!value
  if (flag) {
    this.handleChange(value);
    delete newSelectInput[index]; // 在onBlur后将对应的key删除，防止当从下拉框中选择后再次触发onBlur时经过此处恢复成原来的值
  }
}

render()
{
  let {
    handleCancel,
    form,
    metaGroup
  } = this.props;
  // 下拉框数据
  const appNameGroup = Object.getOwnPropertyNames(metaGroup)
  // 表格数据
  const {selectedRowKeys} = this.state;
  const rowSelection = {
    selectedRowKeys,
    onChange: this.onSelectChange,
  };
  // const columns = [
  //   {
  //     title: getIntlContent("SOUL.AUTH.RESOUCE.PATH"),
  //     dataIndex: 'path',
  //     render: (text, record) => (
  //       <Input
  //         value={text}
  //         onChange={(e) => this.handleTableInput({path: e.target.value}, record)}
  //       />
  //     )
  //   },
  //   {
  //     title: getIntlContent("SOUL.AUTH.PATH.DESCRIBE"),
  //     dataIndex: 'pathDesc',
  //     render: (text, record) => (
  //       <Input
  //         value={text}
  //         onChange={(e) => this.handleTableInput({pathDesc: e.target.value}, record)}
  //       />
  //     )
  //   },
  // ];
  const columns = this.columns


  //   根据下拉框选项自动更换数据
  const data = this.state.allData;
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
  label = {getIntlContent("SOUL.AUTH.APPNAME"
)
}
  {...
    formItemLayout
  }
>
  {
    getFieldDecorator('appName', {
      rules: [{
        required: true,
        message: getIntlContent("SOUL.AUTH.SELECT.APP"),
      }],
    })
    (
    < Select
    showSearch
    placeholder = {getIntlContent("SOUL.AUTH.SELECT.APP"
  )
  }
    onChange = {this.handleChange}
    onSearch = {value = > this.onSearchSelect(value, '0')
  }
    onBlur = {() =
  >
    this.onBlurSelect('0')
  }
  >
    {
      appNameGroup.map((item, index) = > {
        return(
      < Option
      key = {index}
      value = {`${item}`
    }>
      {
        item
      }
    <
      /Option>
    )
    })
    }
  <
    /Select>)}
    < /FormItem>
    < FormItem
    label = {getIntlContent("SOUL.AUTH.PHONE"
  )
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("phone", {
        rules: [{
          required: true,
          message: `${getIntlContent("SOUL.AUTH.INPUT")}Phone`
        }],
      })(
      < Input
      placeholder = "Phone" / >
    )
    }
  <
    /FormItem>
    < FormItem
    label = {`App${getIntlContent("SOUL.AUTH.PARAMS")}`
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("appParam", {
        rules: [{
          required: true,
          message: `${getIntlContent("SOUL.SYSTEM.USER")}AppParam`
        }],
      })(
      < Input
      placeholder = "AppParams" / >
    )
    }
  <
    /FormItem>
    < FormItem
    label = {`${getIntlContent("SOUL.SYSTEM.USER")}Id`
  }
    {...
      formItemLayout
    }
  >
    {
      getFieldDecorator("userId", {
        rules: [{
          required: true,
          message: `${getIntlContent("SOUL.AUTH.INPUT")}UserId`
        }],
      })( < Input
      placeholder = "UserId" / >
    )
    }
  <
    /FormItem>
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
        rules: [{
          required: true,
          message: `${getIntlContent("SOUL.AUTH.INPUT")}ExpandInfo`
        }],
      })( < Input
      placeholder = "ExpandInfo" / >
    )
    }
  <
    /FormItem>
    {/* 下拉框关联表格 */
    }
  <
    div >
    {
      data.length < 1 ?
        (
        < Button
        disabled
      onClick = {this.handleAddTd}
      type = "primary"
      style = {
    {
      marginBottom: 16,
    }
  }
  >
    {
      getIntlContent("SOUL.AUTH.ADD")
    }
  <
    /Button>
  ) :
    (
    < Button
    onClick = {this.handleAddTd}
    type = "primary"
    style = {
    {
      marginBottom: 16,
    }
  }
  >
    {
      getIntlContent("SOUL.AUTH.ADD")
    }
  <
    /Button>
  )
  }
  <
    Table
    bordered
    rowSelection = {rowSelection}
    columns = {columns}
    dataSource = {data}
    rowKey = {record = > record.id
  }
    pagination = {
    {
      current: 1, pageSize
    :
      10
    }
  }
    />
    < /div>
    < /Form>
    < /Modal>
  )
    ;
  }
}

export default Form.create()(AddTable);

