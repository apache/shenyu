import React, {Component} from "react";
import {Table, Button, message, Popconfirm} from "antd";
import {connect} from "dva";
import dayjs from "dayjs";
import AddModal from "./AddModal";
import RelateMetadata from "./RelateMetadata"
import AddTable from "./AddTable"
import SearchContent from "./SearchContent"
import {getCurrentLocale, getIntlContent} from "../../../utils/IntlUtils";
import {emit} from '../../../utils/emit'

@connect(({auth, loading}) = > ({
  auth,
  loading: loading.effects["auth/fetch"]
})
)
export default class Auth extends Component {
  constructor(props) {
    super(props);
    this.state = {
      currentPage: 1,
      selectedRowKeys: [],
      appKey: "",
      phone: "",
      popup: "",
      localeName: ''
    };
  }

  componentWillMount() {
    const {currentPage} = this.state;
    this.getAllAuths(currentPage);
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

// 发送请求获取数据

getAllAuths = page =
>
{
  const {dispatch} = this.props;
  const {appKey, phone} = this.state;
  dispatch({
    type: "auth/fetch",
    payload: {
      appKey,
      phone,
      currentPage: page,
      pageSize: 20
    }
  });
}
;

// 发送请求获取所有元数据信息
// getAllMetaDel = () => {
//   const { dispatch } = this.props;
//   dispatch({
//     type: "auth/fetchMeta",
//     payload: {
//       currentPage: 1,
//       pageSize: 10
//     },
//     callback: datas => datas
//   })
// }

// 分页数据

pageOnchange = page =
>
{
  this.setState({currentPage: page});
  this.getAllAuths(page);
}
;

// 关闭弹框

closeModal = () =
>
{
  this.setState({popup: ""});
}
;

// 编辑弹框

editClick = record =
>
{
  const {dispatch} = this.props;
  const {currentPage} = this.state;
  // const authName = this.state.appKey;
  dispatch({
    type: "auth/fetchItem",
    payload: {
      id: record.id
    },
    callback: (auth) = > {

    this.setState({
      popup: (
        < AddModal
  {...
    auth
  }
  handleOk = {values = > {
    // const { appKey, appSecret, authParamVOList, enabled, id, phone,userId } = values;
    // 发送更新请求
    dispatch({
               type: "auth/update",
               payload: {
                 extInfo:null,
                 ...values
               },
               fetchValue: {

                 currentPage,
                 pageSize: 20
               },
               callback:() =
>
  {
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

editClickMeta = record =
>
{
  const {currentPage} = this.state;
  const {dispatch} = this.props;
  dispatch({
    type: "auth/fetchItemDel",
    payload: {
      id: record.id
    },
    callback: (auth) = > {

    dispatch({
               type: "auth/fetchMeta",
               payload: {
                 // currentPage,
                 // pageSize: 10
               },
               callback: datas = > {
    this.setState({
      popup: (
        < RelateMetadata
  {...
    auth
  }
  {...
    datas
  }
  authName = {`appKey:  ${record.appKey}`
}
  id = {record.id}
  handleCancel = {() =
>
  {
    this.closeModal();
  }
}
  handleOk = {values = > {
    // const { appKey, appSecret, enabled, id, authParamVOList } = values;
    // 发送更新请求
    dispatch({
               type: "auth/updateDel",
               payload: values,
               fetchValue: {

                 currentPage,
                 pageSize: 20
               },
               callback:() =
>
  {
    this.closeModal();
  }
})
  ;
}
}
  />
)
})
}
})

}
})
}


// 点击搜索事件

searchClick = res =
>
{
  // console.log(res)
  const {dispatch} = this.props;
  dispatch({
    type: "auth/fetch",
    payload: {
      appKey: res.appKey ? res.appKey : null,
      phone: res.phone ? res.phone : null,
      pageSize: 20,
      currentPage: 1
    }
  })


  this.setState({currentPage: 1});
}
;

// 点击删除事件

deleteClick = () =
>
{
  const {dispatch} = this.props;
  const {selectedRowKeys} = this.state;
  if (selectedRowKeys && selectedRowKeys.length > 0) {
    // 发送删除请求
    dispatch({
      type: "auth/delete",
      payload: {
        list: selectedRowKeys
      },
      fetchValue: {},
      callback: () = > {
      this.setState({selectedRowKeys: []})
    }
  })
    ;
  } else {
    message.destroy();
    message.warn("请选择数据");
  }
}
;

// 添加表格数据事件

addClick = () =
>
{
  const {currentPage} = this.state;
  const {dispatch} = this.props;
  dispatch({
    type: "auth/fetchMetaGroup",
    payload: {},
    callback: (metaGroup) = > {

    this.setState({
      popup: (
        < AddTable
      metaGroup = {metaGroup}
      handleOk = {values = > {
        // const { appKey, appSecret, enabled } = values;
        // 发送添加请求
        dispatch({
                   type: "auth/add",
                   payload: values,
                   fetchValue: {
                     // appKey: authName,
                     currentPage,
                     pageSize: 20
                   },
                   callback:() = > {
      this.setState({selectedRowKeys: []})
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
}
;

// 批量启用或禁用

enableClick = () =
>
{
  const {dispatch} = this.props;
  const {selectedRowKeys} = this.state;
  if (selectedRowKeys && selectedRowKeys.length > 0) {
    dispatch({
      type: "auth/fetchItem",
      payload: {
        id: selectedRowKeys[0]
      },
      callback: user = > {
      dispatch({
                 type: "auth/updateEn",
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
    }
  })
  }
  })
  } else {
    message.destroy();
    message.warn("请选择数据");
  }
}

// 同步数据事件

syncData = () =
>
{
  const {dispatch} = this.props;
  dispatch({
    type: "auth/syncDa",
    payload: {}
  })
}

changeLocale(locale)
{
  this.setState({
    localeName: locale
  });
  getCurrentLocale(this.state.localeName);
}

render()
{
  const {auth, loading} = this.props;
  const {authList, total} = auth;
  const {currentPage, selectedRowKeys, popup} = this.state;
  const authColumns = [
    {
      align: "center",
      title: "AppKey",
      dataIndex: "appKey",
      key: "appKey",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.AUTH.ENCRYPTKEY"),
      dataIndex: "appSecret",
      key: "appSecret",
      ellipsis: true,
    },
    {
      align: "center",
      title: `${getIntlContent("SOUL.SYSTEM.USER")}Id`,
      dataIndex: "userId",
      key: "userId",
      ellipsis: true,
    },
    {
      align: "center",
      title: getIntlContent("SOUL.AUTH.TEL"),
      dataIndex: "phone",
      key: "phone",
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
  // {
  //   align: "center",
  //   title: "创建时间",
  //   dataIndex: "dateCreated",
  //   key: "dateCreated"
  // },
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.SYSTEM.UPDATETIME"),
      dataIndex
  :
    "dateUpdated",
      render
  :
    dateUpdated =
  >
    dayjs(dateUpdated).format('YYYY-MM-DD HH:mm:ss'),
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
        // 弹窗中的编辑事件
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
,
  {
    align: "center",
      title
  :
    getIntlContent("SOUL.AUTH.OPERATPATH"),
      dataIndex
  :
    "operates",
      key
  :
    "operates",
      ellipsis
  :
    true,
      render
  :
    (text, record) =
  >
    {
      return (
        // 弹窗中的编辑事件
        < div
      className = "edit"
      onClick = {() =
    >
      {
        this.editClickMeta(record);
      }
    }
    >
      {
        getIntlContent("SOUL.AUTH.EDITOR.RESOURCE")
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
    {/* 头部导航栏 */}
    < div
  style = {
  {
    display: "flex", alignItems
  :
    'center'
  }
}>


  {/* 内联查询 */
  }
<
  SearchContent
  onClick = {res = > this.searchClick(res)
}
  />

  {/* 删除勾选按钮 */
  }
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
    {getIntlContent("SOUL.SYSTEM.DELETEDATA"
)
}
<
  /Button>
  < /Popconfirm>
  {/* 添加数据按钮 */
  }
<
  Button
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
  {/* 批量启用或禁用按钮 */
  }
<
  Button
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
  {/* 同步数据按钮 */
  }
<
  Button
  style = {
  {
    marginLeft: 20
  }
}
  type = "primary"
  onClick = {this.syncData}
    >
    {getIntlContent("SOUL.AUTH.SYNCDATA"
)
}
<
  /Button>
  < /div>
  {/* 表格 */
  }
<
  Table
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
  columns = {authColumns}
  dataSource = {authList}
  rowSelection = {rowSelection}
  pagination = {
  {
    total,
      current
  :
    currentPage,
      pageSize
  :
    20,
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
