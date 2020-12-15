import React, {Component} from "react";
import {Table, Row, Col, Button, message, Popconfirm} from "antd";
import {connect} from "dva";
import Selector from "./Selector";
import Rule from "./Rule";
import {getIntlContent, getCurrentLocale} from '../../../utils/IntlUtils'

import {emit} from "../../../utils/emit";

@connect(({common, global, loading}) = > ({
  ...global,
  ...common,
  loading: loading.effects["global/fetchPlatform"]
})
)
export default class Common extends Component {
  constructor(props) {
    super(props);
    this.state = {
      selectorPage: 1,
      rulePage: 1,
      popup: "",
      localeName: ''
    };
  }

  componentDidMount() {
    emit.on('change_language', lang = > this.changeLocales(lang)
  )
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

  componentDidUpdate(prevProps) {

    const preId = prevProps.match.params.id
    const newId = this.props.match.params.id;

    if (newId !== preId) {
      const {dispatch} = this.props;

      dispatch({
        type: "common/resetData",
      });

      dispatch({
        type: "global/fetchPlugins",
        payload: {
          callback: (plugins) = > {
          this.getAllSelectors(1, plugins);
    }
    }
    })
    }
  }

  componentWillUnmount() {
    const {dispatch} = this.props;
    dispatch({
      type: "common/resetData",
    });
  }

  getAllSelectors = (page, plugins) =
> {
  const {
  dispatch
}
= this.props;
let name = this.props.match.params ? this.props.match.params.id : '';
const tempPluginId = this.getPluginId(plugins, name);
this.setState({pluginId: tempPluginId});
dispatch({
  type: "common/fetchSelector",
  payload: {
    currentPage: page,
    pageSize: 12,
    pluginId: tempPluginId
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
    type: "common/fetchRule",
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
  let name = this.props.match.params ? this.props.match.params.id : ''
  const pluginId = this.getPluginId(plugins, name);
  this.setState({
    popup: (
      < Selector
    pluginId = {pluginId}
    handleOk = {selector = > {
      dispatch({
                 type: "common/addSelector",
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
  const {rulePage, pluginId} = this.state;
  const {dispatch, currentSelector} = this.props;

  if (currentSelector && currentSelector.id) {
    const selectorId = currentSelector.id;
    this.setState({
      popup: (
        < Rule
      pluginId = {pluginId}
      handleOk = {rule = > {
        dispatch({
                   type: "common/addRule",
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
  let name = this.props.match.params ? this.props.match.params.id : ''
  const pluginId = this.getPluginId(plugins, name);
  const {id} = record;
  dispatch({
    type: "common/fetchSeItem",
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
               type: "common/updateSelector",
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
  let name = this.props.match.params ? this.props.match.params.id : ''
  const pluginId = this.getPluginId(plugins, name);
  dispatch({
    type: "common/deleteSelector",
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
    type: "common/saveCurrentSelector",
    payload: {
      currentSelector: record
    }
  });
  dispatch({
    type: "common/fetchRule",
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
  const {rulePage, pluginId} = this.state;
  const selectorId = currentSelector ? currentSelector.id : "";
  const {id} = record;
  dispatch({
    type: "common/fetchRuleItem",
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
  pluginId = {pluginId}
  handleOk = {values = > {
    dispatch({
               type: "common/updateRule",
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
    type: "common/deleteRule",
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
  let name = this.props.match.params ? this.props.match.params.id : ''
  const id = this.getPluginId(plugins, name);
  dispatch({
    type: "global/asyncPlugin",
    payload: {
      id
    }
  });
}
;

changeLocales(locale)
{
  this.setState({
    localeName: locale
  });
  getCurrentLocale(this.state.localeName);
}

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
        className = "open" > 开启 < /div>;
      } else {
        return < div className = "close" > 关闭 < /div>;
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
  {
    this.props.match.params ? this.props.match.params.id : ''
  }
<
  /Button>
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
