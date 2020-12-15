import React, {Component} from "react";
import {Table, Row, Col, Button, message, Popconfirm} from "antd";
import {connect} from "dva";
import Selector from "./Selector";
import Rule from "./Rule";
import {getIntlContent} from '../../../utils/IntlUtils'

@connect(({rewrite, global, loading}) = > ({
  ...global,
  ...rewrite,
  loading: loading.effects["global/fetchPlatform"]
})
)
export default class Rewrite extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectorPage: 1,
      rulePage: 1,
      popup: "",
    };
  }

  componentDidMount() {
    const {dispatch} = this.props;
    dispatch({
      type: "global/fetchPlugins",
      payload: {
        callback: (plugins) = > {
        this.getAllSelectors(1, plugins);
  }
  }
  })
  }

  getAllSelectors = (page, plugins) =
> {
  const {
  dispatch
}
= this.props;
const pluginId = this.getPluginId(plugins, "rewrite");
dispatch({
  type: "rewrite/fetchSelector",
  payload: {
    currentPage: page,
    pageSize: 12,
    pluginId
  }
});
}
;

getAllRules = page =
>
{
  const {dispatch, currentSelector} = this.props;
  const selectorId = currentSelector ? currentSelector.id : "";
  dispatch({
    type: "rewrite/fetchRule",
    payload: {
      selectorId,
      currentPage: page,
      pageSize: 12
    }
  });
}
;

getPluginId = (plugins, name) =
>
{
  const plugin = plugins.filter(item = > {
    return item.name === name;
})
  ;
  if (plugin && plugin.length > 0) {
    return plugin[0].id;
  } else {
    return "";
  }
}
;

closeModal = () =
>
{
  this.setState({popup: ""});
}
;

addSelector = () =
>
{
  const {selectorPage} = this.state;
  const {dispatch, plugins} = this.props;
  const pluginId = this.getPluginId(plugins, "rewrite");
  this.setState({
    popup: (
      < Selector
    pluginId = {pluginId}
    handleOk = {selector = > {
      dispatch({
                 type: "rewrite/addSelector",
                 payload: {pluginId, ...selector},
                 fetchValue: {pluginId, currentPage: selectorPage, pageSize: 12},
                 callback:() = > {
    this.closeModal();
}
})
  ;
}
}
  onCancel = {this.closeModal}
  />
)
})
  ;
}
;

addRule = () =
>
{
  const {rulePage} = this.state;
  const {dispatch, currentSelector} = this.props;
  if (currentSelector && currentSelector.id) {
    const selectorId = currentSelector.id;
    this.setState({
      popup: (
        < Rule
      handleOk = {rule = > {
        dispatch({
                   type: "rewrite/addRule",
                   payload: {selectorId, ...rule},
                   fetchValue: {
                     selectorId,
                     currentPage: rulePage,
                     pageSize: 12
                   },
                   callback:() = > {
      this.closeModal();
  }
  })
    ;
  }
  }
    onCancel = {this.closeModal}
    />
  )
  })
    ;
  } else {
    message.destroy();
    message.warn("请先添加选择器");
  }
}
;

editSelector = record =
>
{
  const {dispatch, plugins} = this.props;
  const {selectorPage} = this.state;
  const pluginId = this.getPluginId(plugins, "rewrite");
  const {id} = record;
  dispatch({
    type: "rewrite/fetchSeItem",
    payload: {
      id
    },
    callback: selector = > {
    this.setState({
      popup: (
        < Selector
  {...
    selector
  }
  handleOk = {values = > {
    dispatch({
               type: "rewrite/updateSelector",
               payload: {
                 pluginId,
                 ...values,
                 id
               },
               fetchValue: {
                 pluginId,
                 currentPage: selectorPage,
                 pageSize: 12
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
  onCancel = {this.closeModal}
  />
)
})
  ;
}
})
  ;
}
;

deleteSelector = record =
>
{
  const {dispatch, plugins} = this.props;
  const {selectorPage} = this.state;
  const pluginId = this.getPluginId(plugins, "rewrite");
  dispatch({
    type: "rewrite/deleteSelector",
    payload: {
      list: [record.id]
    },
    fetchValue: {
      pluginId,
      currentPage: selectorPage,
      pageSize: 12
    }
  });
}
;

pageSelectorChange = page =
>
{
  const {plugins} = this.props;
  this.setState({selectorPage: page});
  this.getAllSelectors(page, plugins);
}
;

pageRuleChange = page =
>
{
  this.setState({rulePage: page});
  this.getAllRules(page);
}
;

// 点击选择器
rowClick = record =
>
{
  const {id} = record;
  const {dispatch} = this.props;
  dispatch({
    type: "rewrite/saveCurrentSelector",
    payload: {
      currentSelector: record
    }
  });
  dispatch({
    type: "rewrite/fetchRule",
    payload: {
      currentPage: 1,
      pageSize: 12,
      selectorId: id
    }
  });
}
;

editRule = record =
>
{
  const {dispatch, currentSelector} = this.props;
  const {rulePage} = this.state;
  const selectorId = currentSelector ? currentSelector.id : "";
  const {id} = record;
  dispatch({
    type: "rewrite/fetchRuleItem",
    payload: {
      id
    },
    callback: rule = > {
    this.setState({
      popup: (
        < Rule
  {...
    rule
  }
  handleOk = {values = > {
    dispatch({
               type: "rewrite/updateRule",
               payload: {
                 selectorId,
                 ...values,
                 id
               },
               fetchValue: {
                 selectorId,
                 currentPage: rulePage,
                 pageSize: 12
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
  onCancel = {this.closeModal}
  />
)
})
  ;
}
})
  ;
}
;

deleteRule = record =
>
{
  const {dispatch, currentSelector} = this.props;
  const {rulePage} = this.state;
  dispatch({
    type: "rewrite/deleteRule",
    payload: {
      list: [record.id]
    },
    fetchValue: {
      selectorId: currentSelector.id,
      currentPage: rulePage,
      pageSize: 12
    }
  });
}
;

asyncClick = () =
>
{
  const {dispatch, plugins} = this.props;
  const id = this.getPluginId(plugins, "rewrite");
  dispatch({
    type: "global/asyncPlugin",
    payload: {
      id
    }
  });
}
;

render()
{
  const {popup, selectorPage, rulePage} = this.state;
  const {
    selectorList,
    ruleList,
    selectorTotal,
    ruleTotal,
    currentSelector
  } = this.props;
  const selectColumns = [
    {
      align: "center",
      title: getIntlContent("SOUL.PLUGIN.SELECTOR.LIST.COLUMN.NAME"),
      dataIndex: "name",
      key: "name"
    },
    {
      align: "center",
      title: getIntlContent("SOUL.COMMON.OPEN"),
      dataIndex: "enabled",
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
        < div >
        < span
      style = {
      {
        marginRight: 8
      }
    }
      className = "edit"
      onClick = {e = > {
        e.stopPropagation();
      this.editSelector(record);
    }
    }
    >
      {
        getIntlContent("SOUL.COMMON.CHANGE")
      }
    <
      /span>
      < Popconfirm
      title = {getIntlContent("SOUL.COMMON.DELETE"
    )
    }
      placement = 'bottom'
      onCancel = {(e) =
    >
      {
        e.stopPropagation()
      }
    }
      onConfirm = {(e) =
    >
      {
        e.stopPropagation()
        this.deleteSelector(record);
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
      span
      className = "edit"
      onClick = {(e) =
    >
      {
        e.stopPropagation()
      }
    }
    >
      {
        getIntlContent("SOUL.COMMON.DELETE.NAME")
      }
    <
      /span>
      < /Popconfirm>
      < /div>
    )
      ;
    }
  }
]
  ;

  const rulesColumns = [
    {
      align: "center",
      title: getIntlContent("SOUL.COMMON.RULE.NAME"),
      dataIndex: "name",
      key: "name"
    },
    {
      align: "center",
      title: getIntlContent("SOUL.COMMON.OPEN"),
      dataIndex: "enabled",
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
    getIntlContent("SOUL.SYSTEM.UPDATETIME"),
      dataIndex
  :
    "dateCreated",
      key
  :
    "dateCreated"
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
      render
  :
    (text, record) =
  >
    {
      return (
        < div >
        < span
      className = "edit"
      style = {
      {
        marginRight: 8
      }
    }
      onClick = {e = > {
        e.stopPropagation();
      this.editRule(record);
    }
    }
    >
      {
        getIntlContent("SOUL.COMMON.CHANGE")
      }
    <
      /span>
      < Popconfirm
      title = {getIntlContent("SOUL.COMMON.DELETE"
    )
    }
      placement = 'bottom'
      onCancel = {(e) =
    >
      {
        e.stopPropagation()
      }
    }
      onConfirm = {(e) =
    >
      {
        e.stopPropagation()
        this.deleteRule(record);
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
      span
      className = "edit"
      onClick = {(e) =
    >
      {
        e.stopPropagation()
      }
    }
    >
      {
        getIntlContent("SOUL.COMMON.DELETE.NAME")
      }
    <
      /span>
      < /Popconfirm>
      < /div>
    )
      ;
    }
  }
]
  ;

  return (
    < div
  className = "plug-content-wrap" >
    < Row
  gutter = {20} >
    < Col
  span = {8} >
    < div
  className = "table-header" >
    < h3 > {getIntlContent("SOUL.PLUGIN.SELECTOR.LIST.TITLE"
)
}<
  /h3>
  < Button
  type = "primary"
  onClick = {this.addSelector} >
    {getIntlContent("SOUL.PLUGIN.SELECTOR.LIST.ADD"
)
}
<
  /Button>
  < /div>
  < Table
  size = "small"
  onRow = {record = > {
    return {
      onClick: () = > {
      this.rowClick(record);
}
}
  ;
}
}
  style = {
  {
    marginTop: 30
  }
}
  bordered
  columns = {selectColumns}
  dataSource = {selectorList}
  pagination = {
  {
    total: selectorTotal,
      current
  :
    selectorPage,
      pageSize
  :
    12,
      onChange
  :
    this.pageSelectorChange
  }
}
  rowClassName = {item = > {
    if(currentSelector && currentSelector.id === item.id
)
  {
    return "table-selected";
  }
else
  {
    return "";
  }
}
}
  />
  < /Col>
  < Col
  span = {16} >
    < div
  className = "table-header" >
    < div
  style = {
  {
    display: "flex"
  }
}>
<
  h3
  style = {
  {
    marginRight: 30
  }
}>
  {
    getIntlContent("SOUL.PLUGIN.SELECTOR.RULE.LIST")
  }
<
  /h3>
  < Button
  icon = "reload"
  onClick = {this.asyncClick}
  type = "primary" >
    {getIntlContent("SOUL.COMMON.SYN"
)
}
  rewrite
  < /Button>
  < /div>
  < Button
  type = "primary"
  onClick = {this.addRule} >
    {getIntlContent("SOUL.COMMON.ADD.RULE"
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
  columns = {rulesColumns}
  expandedRowRender = {record = > < p > {record.handle} < /p>}
  dataSource = {ruleList}
  pagination = {
  {
    total: ruleTotal,
      current
  :
    rulePage,
      pageSize
  :
    12,
      onChange
  :
    this.pageRuleChange
  }
}
  />
  < /Col>
  < /Row>
  {
    popup
  }
<
  /div>
)
  ;
}
}
