import React, {Component} from "react";
import {Table, Button, Popconfirm, message} from "antd";
import {connect} from "dva";
import AddPluginHandle from "./AddPluginHandle";
import {getCurrentLocale, getIntlContent} from "../../../utils/IntlUtils";
import {emit} from "../../../utils/emit";

@connect(({pluginHandle, loading}) = > ({
  pluginHandle,
  loading: loading.effects["pluginHandle/fetch"]
})
)
export default class PluginHandle extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      selectedRowKeys: [],
      pluginId: this.props.match.params.pluginId,
      popup: "",
      localeName: ''
    };
  }

  componentWillMount() {
    let {currentPage} = this.state;
    this.getPluginHandlesByPluginId(currentPage);
  }

  componentDidMount() {
    emit.on('change_language', lang = > this.changeLocale(lang)
  )
  }

  getPluginHandlesByPluginId = page =
> {
  const {
  dispatch
}
= this.props;
const {pluginId} = this.state;
dispatch({
  type: "pluginHandle/fetch",
  payload: {
    pluginId,
    currentPage: page,
    pageSize: 12
  }
});
}
;


closeModal = () =
>
{
  this.setState({popup: ""});
}
;


editClick = record =
>
{
  const {dispatch} = this.props;
  const {currentPage} = this.state;
  const localPluginId = this.state.pluginId;
  dispatch({
    type: "pluginHandle/fetchItem",
    payload: {
      id: record.id
    },
    callback: pluginHandle = > {
    let obj = JSON.parse(pluginHandle.extObj)
    Object.assign(
      pluginHandle,
      obj
    )
    this.setState({
      popup: (
        < AddPluginHandle
      disabled = {true}
  {...
    pluginHandle
  }
  handleOk = {values = > {
    const {field, label, id, pluginId, dataType, type, sort, required, defaultValue} = values;
  let extObj
  if (required || defaultValue) {
    extObj = JSON.stringify({
      'required': required,
      'defaultValue': defaultValue
    })
  }
  dispatch({
    type: "pluginHandle/update",
    payload: {
      field,
      label,
      id,
      pluginId,
      dataType,
      type,
      sort,
      extObj
    },
    fetchValue: {
      name: localPluginId,
      currentPage,
      pageSize: 12
    },
    callback: () = > {
    this.closeModal();
  dispatch({
    type: "pluginHandle/fetch",
    payload: {
      pluginId,
      currentPage: {currentPage},
      pageSize: 12
    }
  });
}
})
  ;
}
}
  handleCancel = {() =
>
  {
    this.closeModal();
  }
}
  />
)
})
  ;
}
})
  ;
}
;


addClick = () =
>
{
  const {currentPage} = this.state;
  const localPluginId = this.state.pluginId;
  this.setState({
    popup: (
      < AddPluginHandle
    pluginId = {localPluginId}
    handleOk = {values = > {
      const {dispatch} = this.props;
  const {pluginId, label, field, dataType, type, sort, required, defaultValue} = values;
  let extObj
  if (required || defaultValue) {
    extObj = JSON.stringify({
      'required': required,
      'defaultValue': defaultValue
    })
  }
  dispatch({
    type: "pluginHandle/add",
    payload: {
      pluginId,
      label,
      field,
      dataType,
      type,
      sort,
      extObj
    },
    fetchValue: {
      pluginId: localPluginId,
      currentPage,
      pageSize: 12
    },
    callback: () = > {
    this.closeModal();
  dispatch({
    type: "pluginHandle/fetch",
    payload: {
      pluginId,
      currentPage: {currentPage},
      pageSize: 12
    }
  });
}
})
  ;
}
}
  handleCancel = {() =
>
  {
    this.closeModal();
  }
}
  />
)
})
  ;
}
;

onSelectChange = selectedRowKeys =
>
{

  this.setState({selectedRowKeys});
}
;

deleteClick = () =
>
{
  const {dispatch} = this.props;
  const {pluginId, currentPage, selectedRowKeys} = this.state;
  if (selectedRowKeys && selectedRowKeys.length > 0) {
    dispatch({
      type: "pluginHandle/delete",
      payload: {
        list: selectedRowKeys
      },
      fetchValue: {
        pluginId,
        currentPage,
        pageSize: 12
      },
      callback: () = > {
      this.setState({selectedRowKeys: []});
    dispatch({
      type: "global/fetchPlugins",
      payload: {
        callback: () = > {}
    }
  })
    ;

    dispatch({
      type: "pluginHandle/fetch",
      payload: {
        pluginId,
        currentPage: {currentPage},
        pageSize: 12
      }
    });
  }
  })
    ;
  } else {
    message.destroy();
    message.warn("请选择数据");
  }
}
;

changeLocale(locale)
{
  this.setState({
    localeName: locale
  });
  getCurrentLocale(this.state.localeName);
}

render()
{
  const {pluginHandle, loading} = this.props;
  const {pluginHandleList, total} = pluginHandle;
  const {currentPage, selectedRowKeys, popup} = this.state;
  if (pluginHandleList) {
    pluginHandleList.forEach(item = > {
      if(item.extObj
  )
    {
      let obj = JSON.parse(item.extObj)
      if (obj.required) {
        item.required = obj.required
      }
      if (obj.defaultValue) {
        item.defaultValue = obj.defaultValue
      }
    }
  })
  }

  const pluginColumns = [
    {
      align: "center",
      title: getIntlContent("SOUL.PLUGIN.FIELDNAME"),
      dataIndex: "field",
      key: "field",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.PLUGIN.LABEL"),
      dataIndex: "label",
      key: "label",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.PLUGIN.DATATYPE"),
      dataIndex: "dataType",
      key: "dataType",
      ellipsis: true,
      render: text = > {
      if(text === 1
)
  {
    return
  <
    div > {getIntlContent("SOUL.PLUGIN.DIGITAL"
  )
  }<
    /div>;
  }
else
  if (text === 2) {
    return
  <
    div > {getIntlContent("SOUL.PLUGIN.STRING"
  )
  }<
    /div>;
  } else if (text === 3) {
    return
  <
    div > {getIntlContent("SOUL.PLUGIN.DROPDOWN"
  )
  }<
    /div>;
  }
  return
<
  div > {getIntlContent("SOUL.PLUGIN.UNDEFINETYPE"
)
}<
  /div>;
}
},
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.PLUGIN.FIELDTYPE"),
      dataIndex
  :
    "type",
      key
  :
    "type",
      ellipsis
  :
    true,
      render
  :
    text =
  >
    {
      if (text === 1) {
        return
      <
        div > {getIntlContent("SOUL.SELECTOR.NAME"
      )
      }<
        /div>;
      } else if (text === 2) {
        return
      <
        div > {getIntlContent("SOUL.PLUGIN.RULES"
      )
      }<
        /div>;
      }
      return
    <
      div > {getIntlContent("SOUL.PLUGIN.UNDEFINETYPE"
    )
    }<
      /div>;
    }
  }
,
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.PLUGIN.SORT"),
      dataIndex
  :
    "sort",
      key
  :
    "sort",
      ellipsis
  :
    true,
  }
,
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.PLUGIN.REQUIRED"),
      dataIndex
  :
    "required",
      key
  :
    "required",
      ellipsis
  :
    true,
      render
  :
    text =
  >
    {
      if (text === "1") {
        return
      <
        div > {getIntlContent("SOUL.COMMON.YES"
      )
      }<
        /div>;
      } else if (text === "0") {
        return
      <
        div > {getIntlContent("SOUL.COMMON.NO"
      )
      }<
        /div>;
      }
    }
  }
,
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.PLUGIN.DEFAULTVALUE"),
      dataIndex
  :
    "defaultValue",
      key
  :
    "defaultValue",
      ellipsis
  :
    true,
  }
,
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.SYSTEM.CREATETIME"),
      dataIndex
  :
    "dateCreated",
      key
  :
    "dateCreated",
      ellipsis
  :
    true,
  }
,
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.SYSTEM.UPDATETIME"),
      dataIndex
  :
    "dateUpdated",
      key
  :
    "dateUpdated",
      ellipsis
  :
    true,
  }
,

  {
    align: "center",
      title
  :
    getIntlContent("SOUL.COMMON.OPERAT"),
      dataIndex
  :
    "time",
      key
  :
    "time",
      ellipsis
  :
    true,
      render
  :
    (text, record) =
  >
    {
      return (
        < div >
        < div
      className = "edit"
      onClick = {() =
    >
      {
        this.editClick(record);
      }
    }
    >
      {
        getIntlContent("SOUL.SYSTEM.EDITOR")
      }
    <
      /div>
      < /div>

    )
      ;
    }
  }
]
  ;

  const rowSelection = {
    selectedRowKeys,
    onChange: this.onSelectChange
  };

  return (
    < div
  className = "plug-content-wrap" >
    < div
  style = {
  {
    display: "flex"
  }
}>
<
  Popconfirm
  title = {getIntlContent("SOUL.COMMON.DELETE"
)
}
  placement = 'bottom'
  onConfirm = {() =
>
  {
    this.deleteClick()
  }
}
  okText = {getIntlContent("SOUL.COMMON.SURE"
)
}
  cancelText = {getIntlContent("SOUL.COMMON.CALCEL"
)
}
>
<
  Button
  style = {
  {
    marginLeft: 20
  }
}
  type = "danger"
    >
    {getIntlContent("SOUL.COMMON.DELETE"
)
}
<
  /Button>
  < /Popconfirm>
  < Button
  style = {
  {
    marginLeft: 20
  }
}
  type = "primary"
  onClick = {this.addClick}
    >
    {getIntlContent("SOUL.SYSTEM.ADDDATA"
)
}
<
  /Button>
  < /div>
  < Table
  size = "small"
  style = {
  {
    marginTop: 30
  }
}
  bordered
  loading = {loading}
  columns = {pluginColumns}
  dataSource = {pluginHandleList}
  rowSelection = {rowSelection}
  pagination = {
  {
    total,
      current
  :
    currentPage,
      pageSize
  :
    12,
      onChange
  :
    this.pageOnchange
  }
}
  />
  {
    popup
  }
<
  /div>
)
  ;
}
}
