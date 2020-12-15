import React, {createElement} from 'react';
import {Spin} from 'antd';
import pathToRegexp from 'path-to-regexp';
import Loadable from 'react-loadable';
import {getMenuData} from './menu';

let routerDataCache;

const modelNotExisted = (app, model) =
>
// eslint-disable-next-line
!app._models.some(({namespace}) = > {
  return namespace === model.substring(model.lastIndexOf('/') + 1);
})
;

// wrapper of dynamic
const dynamicWrapper = (app, models, component) =
>
{
  // register models
  models.forEach(model = > {
    if(modelNotExisted(app, model)
)
  {
    // eslint-disable-next-line
    app.model(require(`../models/${model}`).default);
  }
})
  ;

  // () => require('module')
  // transformed by babel-plugin-dynamic-import-node-sync
  if (component.toString().indexOf('.then(') < 0) {
    return props =
  >
    {
      if (!routerDataCache) {
        routerDataCache = getRouterData(app);
      }
      return createElement(component().default, {
        ...props,
        routerData: routerDataCache,
      });
    }
    ;
  }
  // () => import('module')
  return Loadable({
    loader: () = > {
    if(
  !routerDataCache
)
  {
    routerDataCache = getRouterData(app);
  }
  return component().then(raw = > {
    const Component = raw.default || raw;
  return props =
>
  createElement(Component, {
    ...props,
    routerData: routerDataCache,
  });
})
  ;
},
  loading: () =
>
  {
    return
  <
    Spin
    size = "large"
    className = "global-spin" / >;
  }
,
})
  ;
}
;

function getFlatMenuData(menus) {
  let keys = {};
  menus.forEach(item = > {
    if(item.children
)
  {
    keys[item.path] = {...item};
    keys = {...keys, ...getFlatMenuData(item.children)};
  }
else
  {
    keys[item.path] = {...item};
  }
})
  ;
  return keys;
}

export const getRouterData = app =
>
{
  const routerConfig = {
    '/': {
      component: dynamicWrapper(app, ['user', 'login'], () = > import('../layouts/BasicLayout')
),
},
  '/home'
:
  {
    component: dynamicWrapper(app, [], () = > import('../routes/Home')
  ),
  }
,
  '/plug/hystrix'
:
  {
    component: dynamicWrapper(app, ['hystrix'], () = > import('../routes/Plugin/Hystrix')
  ),
  }
,
  '/plug/divide'
:
  {
    component: dynamicWrapper(app, ['divide'], () = > import('../routes/Plugin/Divide')
  ),
  }
,
  '/plug/:id'
:
  {
    component: dynamicWrapper(app, ['common'], () = > import('../routes/Plugin/Common')
  ),
  }
,
  '/system/manage'
:
  { // 用户管理
    component: dynamicWrapper(app, ['manage'], () = > import('../routes/System/User')
  ),
  }
,
  '/system/metadata'
:
  { // 元数据管理
    component: dynamicWrapper(app, ['metadata'], () = > import('../routes/System/Metadata')
  ),
  }
,
  '/system/plugin'
:
  { // 插件管理
    component: dynamicWrapper(app, ['plugin'], () = > import('../routes/System/Plugin')
  ),
  }
,
  '/system/auth'
:
  { // 认证管理
    component: dynamicWrapper(app, ['auth'], () = > import('../routes/System/AppAuth')
  ),
  }
,
  '/system/dict'
:
  { // 字典管理
    component: dynamicWrapper(app, ['soulDict'], () = > import('../routes/System/Dict')
  ),
  }
,
  '/exception/403'
:
  {
    component: dynamicWrapper(app, [], () = > import('../routes/Exception/403')
  ),
  }
,
  '/exception/404'
:
  {
    component: dynamicWrapper(app, [], () = > import('../routes/Exception/404')
  ),
  }
,
  '/exception/500'
:
  {
    component: dynamicWrapper(app, [], () = > import('../routes/Exception/500')
  ),
  }
,
  '/exception/trigger'
:
  {
    component: dynamicWrapper(app, ['error'], () = >
    import('../routes/Exception/triggerException')
  ),
  }
,
  '/user'
:
  {
    component: dynamicWrapper(app, [], () = > import('../layouts/UserLayout')
  ),
  }
,
  '/user/login'
:
  {
    component: dynamicWrapper(app, ['login'], () = > import('../routes/User/Login')
  ),
  }
,
  '/plugin/handle/:pluginId'
:
  {
    component: dynamicWrapper(app, ['pluginHandle'], () = > import('../routes/System/Plugin/PluginHandle')
  ),
  }
,
}
  ;
  // Get name from ./menu.js or just set it in the router data.
  const menuData = getFlatMenuData(getMenuData());
  // Route configuration data
  // eg. {name,authority ...routerConfig }
  const routerData = {};
  // The route matches the menu
  Object.keys(routerConfig).forEach(path = > {
    // Regular match item name
    // eg.  router /user/:id === /user/chen
    const pathRegexp = pathToRegexp(path);
  const menuKey = Object.keys(menuData).find(key = > pathRegexp.test(`${key}`)
)
  ;
  let menuItem = {};
  // If menuKey is not empty
  if (menuKey) {
    menuItem = menuData[menuKey];
  }
  let router = routerConfig[path];
  // If you need to configure complex parameter routing,
  // https://github.com/ant-design/ant-design-pro-site/blob/master/docs/router-and-nav.md#%E5%B8%A6%E5%8F%82%E6%95%B0%E7%9A%84%E8%B7%AF%E7%94%B1%E8%8F%9C%E5%8D%95
  // eg . /list/:type/user/info/:id
  router = {
    ...router,
    name: router.name || menuItem.name,
    authority: router.authority || menuItem.authority,
    hideInBreadcrumb: router.hideInBreadcrumb || menuItem.hideInBreadcrumb,
  };
  routerData[path] = router;
})
  ;
  return routerData;
}
;
