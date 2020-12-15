import React, {Component} from "react";
import {Table, Input, Button, message, Popconfirm} from "antd";
import {connect} from "dva";
import AddModal from "./AddModal";
import {getCurrentLocale, getIntlContent} from "../../../utils/IntlUtils";
import {emit} from '../../../utils/emit'

@connect(({soulDict, loading}) = > ({
  soulDict,
  loading: loading.effects["soulDict/fetch"]
})
)
export default class SoulDict extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      selectedRowKeys: [],
      type: "",
      dictName: "",
      dictCode: "",
      popup: "",
      localeName: ''
    };
  }

  componentWillMount() {
    const {currentPage} = this.state;
    this.getAllDict(currentPage);
  }

  componentDidMount() {
    emit.on('change_language', lang = > this.changeLocale(lang)
  )
  }

  onSelectChange = selectedRowKeys =
> {
  this
.

  setState({selectedRowKeys});
};

getAllDict = page =
>
{
  const {dispatch} = this.props;
  const {type, dictName, dictCode} = this.state;
  dispatch({
    type: "soulDict/fetch",
    payload: {
      type,
      dictName,
      dictCode,
      currentPage: page,
      pageSize: 12
    }
  });
}
;

pageOnchange = page =
>
{
  this.setState({currentPage: page});
  this.getAllDict(page);
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
  const currentType = this.state.type;
  const currentDictCode = this.state.dictCode;
  const currentDictName = this.state.dictName;
  dispatch({
    type: "soulDict/fetchItem",
    payload: {
      id: record.id
    },
    callback: user = > {
    // console.log(user)
    this.setState({

      popup: (
        < AddModal
      isShow = {false}
  {...
    user
  }
  handleOk = {values = > {
    const {type, dictCode, id, dictName, dictValue, desc, sort, enabled} = values;

  dispatch({
    type: "soulDict/update",
    payload: {
      type,
      dictCode,
      dictName,
      dictValue,
      id,
      desc,
      sort,
      enabled
    },
    fetchValue: {
      type: currentType,
      dictCode: currentDictCode,
      dictName: currentDictName,
      currentPage,
      pageSize: 12
    },
    callback: () = > {
    this.closeModal();
  dispatch({
    type: "soulDict/fetch",
    payload: {
      type: currentType,
      dictName: currentDictName,
      dictCode: currentDictCode,
      pageSize: 12,
      callback: () = > {}
  }
})
  ;
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

searchTypeOnchange = e =
>
{
  const type = e.target.value;
  this.setState({type});
}
;

searchDictCodeOnchange = e =
>
{
  const dictCode = e.target.value;
  this.setState({dictCode});
}
;

searchDictNameOnchange = e =
>
{
  const dictName = e.target.value;
  this.setState({dictName});
}
;

searchClick = () =
>
{
  this.getAllDict(1);
  this.setState({currentPage: 1});
}
;

deleteClick = () =
>
{
  const {dispatch} = this.props;
  const {type, dictCode, dictName, currentPage, selectedRowKeys} = this.state;
  if (selectedRowKeys && selectedRowKeys.length > 0) {
    dispatch({
      type: "soulDict/delete",
      payload: {
        list: selectedRowKeys
      },
      fetchValue: {
        type,
        dictCode,
        dictName,
        currentPage,
        pageSize: 12
      },
      callback: () = > {
      this.setState({selectedRowKeys: []});
    dispatch({
      type: "soulDict/fetch",
      payload: {
        type,
        dictName,
        dictCode,
        pageSize: 12,
        callback: () = > {}
    }
  })
    ;
  }
  })
    ;
  } else {
    message.destroy();
    message.warn("请选择数据");
  }
}
;

addClick = () =
>
{
  const {currentPage} = this.state;
  this.setState({
    popup: (
      < AddModal
    isShow = {true}
    handleOk = {values = > {
      const {dispatch} = this.props;
  const {type, dictCode, dictName, dictValue, desc, sort, enabled} = values;
  dispatch({
    type: "soulDict/add",
    payload: {
      type,
      dictCode,
      dictName,
      desc,
      dictValue,
      sort,
      enabled
    },
    fetchValue: {
      currentPage,
      pageSize: 12
    },
    callback: () = > {
    this.setState({selectedRowKeys: []});
  this.closeModal();
  dispatch({
    type: "soulDict/fetch",
    payload: {
      callback: () = > {}
  }
})
  ;
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

enableClick = () =
>
{
  const {dispatch} = this.props;
  const {type, dictName, dictCode, selectedRowKeys} = this.state;
  if (selectedRowKeys && selectedRowKeys.length > 0) {

    dispatch({
      type: "soulDict/fetchItem",
      payload: {
        id: selectedRowKeys[0]
      },
      callback: user = > {

      dispatch({
                 type: "soulDict/updateEn",
                 payload: {
                   list: selectedRowKeys,
                   enabled:
    !user.enabled
  },
    fetchValue: {
    }
  ,
    callback: () =
  >
    {
      this.setState({selectedRowKeys: []});
      dispatch({
        type: "soulDict/fetch",
        payload: {
          type,
          dictName,
          dictCode,
          pageSize: 12,
          callback: () = > {}
      }
    })
      ;
    }
  })
    ;
  }
  })
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
;

render()
{
  const {soulDict, loading} = this.props;
  const {soulDictList, total} = soulDict;

  const {currentPage, selectedRowKeys, type, dictCode, dictName, popup} = this.state;
  const userColumns = [
    {
      align: "center",
      title: getIntlContent("SOUL.DIC.TYPE"),
      dataIndex: "type",
      key: "type",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.DIC.CODE"),
      dataIndex: "dictCode",
      key: "dictCode",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.DIC.NAME"),
      dataIndex: "dictName",
      key: "dictName",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.DIC.VALUE"),
      dataIndex: "dictValue",
      key: "dictValue",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.DIC.DESCRIBE"),
      dataIndex: "desc",
      key: "desc",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.PLUGIN.SORT"),
      dataIndex: "sort",
      key: "sort",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.SYSTEM.STATUS"),
      dataIndex: "enabled",
      ellipsis: true,
      key: "enabled",
      render: text = > {
      if(text) {
        return
      <
        div
        className = "open" > {getIntlContent("SOUL.COMMON.OPEN"
      )
      }<
        /div>;
      } else {
        return < div className = "close" > {getIntlContent("SOUL.COMMON.CLOSE"
)
}<
  /div>;
}
}
},
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.COMMON.OPERAT"),
      ellipsis
  :
    true,
      dataIndex
  :
    "operate",
      key
  :
    "operate",
      render
  :
    (text, record) =
  >
    {
      return (
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
  Input
  value = {type}
  placeholder = {getIntlContent("SOUL.DIC.INPUTTYPE"
)
}
  onChange = {this.searchTypeOnchange}
  style = {
  {
    width: 240
  }
}
  />&nbsp;&nbsp;
  < Input
  value = {dictCode}
  placeholder = {getIntlContent("SOUL.DIC.INPUTCODE"
)
}
  onChange = {this.searchDictCodeOnchange}
  style = {
  {
    width: 240
  }
}
  />&nbsp;&nbsp;
  < Input
  value = {dictName}
  placeholder = {getIntlContent("SOUL.DIC.INPUTNAME"
)
}
  onChange = {this.searchDictNameOnchange}
  style = {
  {
    width: 240
  }
}
  />
  < Button
  style = {
  {
    marginLeft: 20
  }
}
  type = "primary"
  onClick = {this.searchClick}
    >
    {getIntlContent("SOUL.META.PAGE.QUERY"
)
}
<
  /Button>
  < Popconfirm
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
    {getIntlContent("SOUL.SYSTEM.DELETEDATA"
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
    {getIntlContent("SOUL.COMMON.ADD"
)
}
<
  /Button>
  < Button
  style = {
  {
    marginLeft: 20
  }
}
  type = "primary"
  onClick = {this.enableClick}
    >
    {getIntlContent("SOUL.PLUGIN.BATCH"
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
  rowKey = {record = > record.id
}
  loading = {loading}
  columns = {userColumns}
  dataSource = {soulDictList}
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
