import React, {Fragment} from "react";
import {Link, Redirect, Switch, Route} from "dva/router";
import DocumentTitle from "react-document-title";
import {connect} from "dva";
import {Icon, message} from "antd";
import GlobalFooter from "../components/GlobalFooter";
import styles from "./UserLayout.less";
import logo from "../assets/logo.svg";
import {getRoutes, getPageQuery, getQueryPath} from "../utils/utils";

message.config({
  top: 200,
  duration: 2,
  maxCount: 3
});
const links = [];

const copyright = (
  < Fragment >
  Copyright < Icon
type = "copyright" / > 2018
出品
< /Fragment>
)
;

function getLoginPathWithRedirectPath() {
  const params = getPageQuery();
  const {redirect} = params;
  return getQueryPath("/user/login", {
    redirect
  });
}

class UserLayout extends React.PureComponent {
  getPageTitle() {
    const {routerData, location} = this.props;
    const {pathname} = location;
    let title = "登录";
    if (routerData[pathname] && routerData[pathname].name) {
      title = `${routerData[pathname].name} - bbex`;
    }
    return title;
  }

  render() {
    const {routerData, match} = this.props;
    return (
      < DocumentTitle
    title = {this.getPageTitle()} >
      < div
    className = {styles.container} >
      < div
    className = {styles.content} >
      < div
    className = {styles.top} >
      < div
    className = {styles.header} >
      < Link
    to = "/" >
      < img
    alt = "logo"
    className = {styles.logo}
    src = {logo}
    />
    < span
    className = {styles.title} > Soul
    Gateway
    Management < /span>
    < /Link>
    < /div>
    < div
    className = {styles.desc} > Soul
    Gateway
    Management
    System < /div>
    < /div>
    < Switch >
    {getRoutes(match.path, routerData
  ).
    map(item = > (
    < Route
    key = {item.key}
    path = {item.path}
    component = {item.component}
    exact = {item.exact}
    />
  ))
  }
  <
    Redirect
    from = "/user"
    to = {getLoginPathWithRedirectPath()}
    />
    < /Switch>
    < /div>
    < GlobalFooter
    links = {links}
    copyright = {copyright}
    />
    < /div>
    < /DocumentTitle>
  )
    ;
  }
}

export default connect(({global = {}}) = > ({
  collapsed: global.collapsed
})
)
(UserLayout);
