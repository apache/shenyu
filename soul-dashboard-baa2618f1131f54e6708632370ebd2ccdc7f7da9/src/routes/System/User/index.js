import React, {Component} from "react";
import {Table, Input, Button, message, Popconfirm} from "antd";
import {connect} from "dva";
import AddModal from "./AddModal";
import {getCurrentLocale, getIntlContent} from "../../../utils/IntlUtils";
import {emit} from '../../../utils/emit';

@connect(({manage, loading}) = > ({
  manage,
  loading: loading.effects["manage/fetch"]
})
)
export default class Manage extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      selectedRowKeys: [],
      userName: "",
      popup: "",
      localeName: ''
    };
  }

  componentWillMount() {
    const {currentPage} = this.state;
    this.getAllUsers(currentPage);
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

getAllUsers = page =
>
{
  const {dispatch} = this.props;
  const {userName} = this.state;
  dispatch({
    type: "manage/fetch",
    payload: {
      userName,
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
  this.getAllUsers(page);
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
  const name = this.state.userName;
  dispatch({
    type: "manage/fetchItem",
    payload: {
      id: record.id
    },
    callback: user = > {
    this.setState({
      popup: (
        < AddModal
  {...
    user
  }
  handleOk = {values = > {
    const {userName, password, role, enabled, id} = values;
  dispatch({
    type: "manage/update",
    payload: {
      userName,
      password,
      role,
      enabled,
      id
    },
    fetchValue: {
      userName: name,
      currentPage,
      pageSize: 12
    },
    callback: () = > {
    this.closeModal();
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

searchOnchange = e =
>
{
  const userName = e.target.value;
  this.setState({userName});
}
;

searchClick = () =
>
{
  this.getAllUsers(1);
  this.setState({currentPage: 1});
}
;

deleteClick = () =
>
{
  const {dispatch} = this.props;
  const {userName, currentPage, selectedRowKeys} = this.state;
  if (selectedRowKeys && selectedRowKeys.length > 0) {
    dispatch({
      type: "manage/delete",
      payload: {
        list: selectedRowKeys
      },
      fetchValue: {
        userName,
        currentPage,
        pageSize: 12
      },
      callback: () = > {
      this.setState({selectedRowKeys: []});
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
  const name = this.state.userName;
  this.setState({
    popup: (
      < AddModal
    handleOk = {values = > {
      const {dispatch} = this.props;
  const {userName, password, role, enabled} = values;
  dispatch({
    type: "manage/add",
    payload: {
      userName,
      password,
      role,
      enabled
    },
    fetchValue: {
      userName: name,
      currentPage,
      pageSize: 12
    },
    callback: () = > {
    this.setState({selectedRowKeys: []});
  this.closeModal();
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
  const {manage, loading} = this.props;
  const {userList, total} = manage;
  const {currentPage, selectedRowKeys, userName, popup} = this.state;
  const userColumns = [
    {
      align: "center",
      title: getIntlContent("SOUL.SYSTEM.USERNAME"),
      dataIndex: "userName",
      key: "userName",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.SYSTEM.STATUS"),
      dataIndex: "enabled",
      key: "enabled",
      ellipsis: true,
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
    "operate",
      key
  :
    "operate",
      ellipsis
  :
    true,
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
  value = {userName}
  onChange = {this.searchOnchange}
  placeholder = {getIntlContent("SOUL.SYSTEM.USER.NAME"
)
}
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
    {getIntlContent("SOUL.SYSTEM.SEARCH"
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
  columns = {userColumns}
  dataSource = {userList}
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
