import React from "react";
import PropTypes from "prop-types";
import {Layout, message} from "antd";
import DocumentTitle from "react-document-title";
import {connect} from "dva";
import {Route, Redirect, Switch} from "dva/router";
import {ContainerQuery} from "react-container-query";
import classNames from "classnames";
import pathToRegexp from "path-to-regexp";
import GlobalHeader from "../components/GlobalHeader";
import SiderMenu from "../components/SiderMenu";
import NotFound from "../routes/Exception/404";
import {getRoutes} from "../utils/utils";
import Authorized from "../utils/Authorized";
import {getMenuData} from "../common/menu";
import logo from "../assets/logo.svg";

message.config({
  top: 200,
  duration: 2,
  maxCount: 3
});

const {Content, Header} = Layout;
const {AuthorizedRoute, check} = Authorized;

/**
 * 根据菜单取得重定向地址.
 */
const redirectData = [];
const getRedirect = item =
>
{
  if (item && item.children) {
    if (item.children[0] && item.children[0].path) {
      redirectData.push({
        from: `${item.path}`,
        to: `${item.children[0].path}`
      });
      item.children.forEach(children = > {
        getRedirect(children);
    })
      ;
    }
  }
}
;
getMenuData().forEach(getRedirect);

/**
 * 获取面包屑映射
 * @param {Object} menuData 菜单配置
 * @param {Object} routerData 路由配置
 */
const getBreadcrumbNameMap = (menuData, routerData) =
>
{
  const result = {};
  const childResult = {};
  for (const i of menuData) {
    if (!routerData[i.path]) {
      result[i.path] = i;
    }
    if (i.children) {
      Object.assign(childResult, getBreadcrumbNameMap(i.children, routerData));
    }
  }
  return Object.assign({}, routerData, result, childResult);
}
;

const query = {
  "screen-lg": {
    minWidth: 992,
    maxWidth: 1199
  },
  "screen-xl": {
    minWidth: 1200,
    maxWidth: 1599
  },
  "screen-xxl": {
    minWidth: 1600
  }
};

@connect(({global, loading}) = > ({
  plugins: global.plugins,
  loading: loading.effects["global/fetchPlugins"]
})
)

class BasicLayout extends React.PureComponent {
  static childContextTypes = {
    location: PropTypes.object,
    breadcrumbNameMap: PropTypes.object
  };

  getChildContext() {
    const {location, routerData} = this.props;
    return {
      location,
      breadcrumbNameMap: getBreadcrumbNameMap(getMenuData(), routerData)
    };
  }

  componentDidMount() {
    const {dispatch} = this.props;
    dispatch({
      type: "global/fetchPlugins",
      payload: {
        callback: () = > {}
    }
  })
    ;
    dispatch({
      type: "global/fetchPlatform"
    });
  }

  getPageTitle() {
    const {routerData, location} = this.props;
    const {pathname} = location;
    let title = "网关管理";
    let currRouterData = null;
    // match params path
    Object.keys(routerData).forEach(key = > {
      if(pathToRegexp(key).test(pathname)
  )
    {
      currRouterData = routerData[key];
    }
  })
    ;
    if (currRouterData && currRouterData.name) {
      title = `soul - 网关管理`;
    }
    return title;
  }

  getBaseRedirect = () =
> {
  // According to the url parameter to redirect
  // 这里是重定向的,重定向到 url 的 redirect 参数所示地址
  const
  urlParams = new URL(window.location.href);

  const
  redirect = urlParams.searchParams.get("redirect");

  // Remove the parameters in the url
  if(redirect) {
    urlParams.searchParams.delete("redirect");
    window.history.replaceState(null, "redirect", urlParams.href);
  }

  else {
  const {
  routerData
}

= this.props;
// get the first authorized route path in routerData
const authorizedPath = Object.keys(routerData).find(
  item = > check(routerData[item].authority, item) && item !== "/"
)
;
return authorizedPath;
}
return redirect;
}
;

handleLogout = () =
>
{
  const {dispatch} = this.props;
  dispatch({
    type: "login/logout"
  });
}
;

render()
{
  const {collapsed, routerData, match, location, plugins, dispatch,} = this.props;
  const bashRedirect = this.getBaseRedirect();
  const systemRoute = ["divide", "hystrix"];
  let menus = getMenuData();
  plugins.forEach((item) = > {
    if(systemRoute.indexOf(item.name) === -1
)
  {
    menus[0].children.push({name: item.name, path: `/plug/${item.name}`, authority: undefined, id: item.id})
  }
})
  const layout = (
    < Layout >
    < SiderMenu
  logo = {logo}
  // 不带Authorized参数的情况下如果没有权限,会强制跳到403界面
  // If you do not have the Authorized parameter
  // you will be forced to jump to the 403 interface without permission
  Authorized = {Authorized}
  dispatch = {dispatch}
  menuData = {menus}
  collapsed = {collapsed}
  location = {location}
  onCollapse = {this.handleMenuCollapse}
  />
  < Layout >
  < Header
  style = {
  {
    padding: 0
  }
}>
<
  GlobalHeader
  logo = {logo}
  collapsed = {collapsed}
  onCollapse = {this.handleMenuCollapse}
  onLogout = {this.handleLogout}
  />
  < /Header>
  < Content
  className = "content-wrap"
  style = {
  {
    height: "100%",
      position
  :
    "relative"
  }
}
>
<
  Switch >
  {
    redirectData.map(item = > (
      < Redirect key = {item.from} exact from = {item.from} to = {item.to}
  />
))
}
  {
    getRoutes(match.path, routerData).map(item = > (
    < AuthorizedRoute
    key = {item.key}
    path = {item.path}
    component = {item.component}
    exact = {item.exact}
    authority = {item.authority}
    redirectPath = "/exception/403"
      / >
  ))
  }
<
  Redirect
  exact
  from = "/"
  to = {bashRedirect}
  />
  < Route
  render = {NotFound}
  />
  < /Switch>
  < /Content>
  < /Layout>
  < /Layout>
)
  ;

  return (
    < DocumentTitle
  title = {this.getPageTitle()} >
    < ContainerQuery
  query = {query} >
    {params = > (
    < div
  style = {
  {
    minWidth: 1200
  }
}
  className = {classNames(params)} >
    {layout}
    < /div>
)
}
<
  /ContainerQuery>
  < /DocumentTitle>
)
  ;
}
}

export default connect(({global = {}}) = > ({
  collapsed: global.collapsed
})
)
(BasicLayout);
