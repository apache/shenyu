import {message} from "antd";
import {
  addPluginHandle,
  batchDeletePluginHandle,
  fetchPluginHandles,
  findPluginHandle,
  updatePluginHandle,
  fetchPluginHandleByPluginId
} from "../services/api";

export default {
  namespace: "pluginHandle",

  state: {
    pluginHandleList: [],
    total: 0
  },

  effects: {
    * fetch(params, {call, put})
{
  const {payload} = params;
  const json = yield call(fetchPluginHandles, payload);
  if (json.code === 200) {
    let {page, dataList} = json.data;
    dataList = dataList.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    yield put({
      type: "savePluginHandles",
      payload: {
        total: page.totalCount,
        dataList
      }
    });
  }
}
,
*
add(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(addPluginHandle, payload);
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
fetchItem(params, {call})
{
  const {payload, callback} = params;
  const json = yield call(findPluginHandle, payload);
  if (json.code === 200) {
    const pluginHandle = json.data;
    callback(pluginHandle);
  }
}
,
*
update(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(updatePluginHandle, payload);
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
delete (params, {call, put})
{
  const {payload, fetchValue, callback} = params;
  const {list} = payload;
  const json = yield call(batchDeletePluginHandle, {list});
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
fetchByPluginId(params, {call})
{
  const {payload} = params;
  let handle = payload.handle;
  let callback = payload.callBack;
  let handleJson;
  if (handle != null && handle !== "" && typeof (handle) !== "undefined" && handle.indexOf("{") !== -1) {
    handleJson = JSON.parse(handle);
  }
  const json = yield call(fetchPluginHandleByPluginId, payload);
  if (json.code === 200) {
    let dataList = json.data.map(item = > {
      item.key = item.id;
    if (typeof (handleJson) === "undefined") {
      item.value = "";
    } else {
      item.value = handleJson[item.field];
    }
    return item;
  })
    ;
    callback(dataList);
  }
}
,
},
reducers: {
  savePluginHandles(state, {payload})
  {
    return {
      ...state,
      pluginHandleList: payload.dataList,
      total: payload.total
    };
  }
,

*
  reload(params, {put})
  {
    const {fetchValue} = params;
    const {pluginId, currentPage, pageSize} = fetchValue;
    const payload = {pluginId, currentPage, pageSize};
    yield put({type: "fetch", payload});
  }
,
}
}
;
