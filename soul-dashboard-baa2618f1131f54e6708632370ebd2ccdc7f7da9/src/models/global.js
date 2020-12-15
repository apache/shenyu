import {message} from "antd";
import {queryPlatform, getAllPlugins, asyncOnePlugin} from "../services/api";

export default {
  namespace: "global",

  state: {
    collapsed: false,
    platform: {},
    plugins: [],
    currentRouter: {}
  },

  effects: {
    * fetchPlatform(_, {call, put})
{
  const json = yield call(queryPlatform);
  if (json.code === 200) {
    yield put({
      type: "savePlatform",
      payload: json.data
    });
  }
}
,
*
fetchPlugins({payload}, {call, put})
{
  const {callback} = payload;
  const params = {
    currentPage: 1,
    pageSize: 50
  };
  const json = yield call(getAllPlugins, params);
  if (json.code === 200) {
    let {dataList} = json.data;

    callback(dataList)
    yield put({
      type: "savePlugins",
      payload: {
        dataList
      }
    });
  }
}
,
*
asyncPlugin(params, {call})
{
  const {payload} = params;
  const json = yield call(asyncOnePlugin, payload);
  if (json.code === 200) {
    message.success("同步成功");
  } else {
    message.warn(json.message);
  }
}
},

reducers: {
  changeLayoutCollapsed(state, {payload})
  {
    return {
      ...state,
      collapsed: payload
    };
  }
,
  savePlatform(state, {payload})
  {
    return {
      ...state,
      platform: payload
    };
  }
,
  savePlugins(state, {payload})
  {
    return {
      ...state,
      plugins: payload.dataList
    };
  }
,
  saveCurrentRoutr(state, {payload})
  {
    return {
      ...state,
      currentRouter: payload.currentRouter
    };
  }
}
,

subscriptions: {
  setup({history})
  {
    // Subscribe history(url) change, trigger `load` action if pathname is `/`
    return history.listen(({pathname, search}) = > {
      if(typeof window.ga !== "undefined"
  )
    {
      window.ga("send", "pageview", pathname + search);
    }
  })
    ;
  }
}
}
;
