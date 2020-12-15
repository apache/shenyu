import {message} from "antd";
import {
  getAllPlugins,
  findPlugin,
  updatePlugin,
  deletePlugin,
  addPlugin,
  asyncPlugin,
  updatepluginEnabled
} from "../services/api";

export default {
  namespace: "plugin",

  state: {
    pluginList: [],
    total: 0
  },

  effects: {
    * fetch(params, {call, put})
{
  const {payload} = params;
  const json = yield call(getAllPlugins, payload);
  if (json.code === 200) {
    let {page, dataList} = json.data;
    dataList = dataList.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    yield put({
      type: "savePlugins",
      payload: {
        total: page.totalCount,
        dataList
      }
    });
  }
}
,
*
fetchItem(params, {call})
{
  const {payload, callback} = params;
  const json = yield call(findPlugin, payload);
  if (json.code === 200) {
    const plugin = json.data;
    callback(plugin);
  }
}
,
*
add(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(addPlugin, payload);
  if (json.code === 200) {
    message.success("添加成功");
    callback();
    yield put({type: "reload", fetchValue});
  } else {
    message.warn(json.message);
  }
}
,
*
changeStatus({payload}, {call, put})
{
  const json = yield call(updatePlugin, payload);
  if (json.code === 200) {
    message.success("修改成功");
    yield put({
      type: "updataPlugins",
      payload,
    });
  } else {
    message.warn(json.message);
  }
}
,
*
delete (params, {call, put})
{
  const {payload, fetchValue, callback} = params;
  const {list} = payload;
  const json = yield call(deletePlugin, {list});
  if (json.code === 200) {
    message.success("删除成功");
    callback();
    yield put({type: "reload", fetchValue});
  } else {
    message.warn(json.message);
  }
}
,
*
update(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(updatePlugin, payload);
  if (json.code === 200) {
    message.success("修改成功");
    callback();
    yield put({type: "reload", fetchValue});
  } else {
    message.warn(json.message);
  }
}
,
*
updateEn(params, {call, put})
{
  const {payload, fetchValue, callback} = params;
  const json = yield call(updatepluginEnabled, payload);
  if (json.code === 200) {
    message.success("修改成功");
    callback();
    yield put({type: "reload", fetchValue});
  } else {
    message.warn(json.message)
  }
}
,
*
reload(params, {put})
{
  const {fetchValue} = params;
  const {name, currentPage, pageSize} = fetchValue;
  const payload = {name, currentPage, pageSize};
  yield put({type: "fetch", payload});
}
,
*
asyncAll(_, {call})
{
  const json = yield call(asyncPlugin);
  if (json.code === 200) {
    message.success("同步成功");
  } else {
    message.warn(json.message);
  }
}
},

reducers: {
  savePlugins(state, {payload})
  {
    return {
      ...state,
      pluginList: payload.dataList,
      total: payload.total
    };
  }
,
  updataPlugins(state, {payload})
  {
    let pluginList = state.pluginList;
    pluginList = pluginList.map((item) = > {
      if(item.id === payload.id
  )
    {
      item.enabled = payload.enabled;
    }
    return item;
  })
    return {
      ...state,
      pluginList,
    };
  }
}
}
;
