import {message} from "antd";
import {
  getAllMetadata,
  findMetadata,
  updateMetadata,
  deleteMetadata,
  addMetadata,
  updateEnabled,
  syncData
} from "../services/api";

export default {
  namespace: "metadata",

  state: {
    userList: [],
    total: 0
  },

  effects: {
    * fetch(params, {call, put})
{
  const {payload} = params;
  const json = yield call(getAllMetadata, payload);
  if (json.code === 200) {
    let {page, dataList} = json.data;

    dataList = dataList.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    yield put({
      type: "saveUsers",
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

  const json = yield call(findMetadata, payload);
  if (json.code === 200) {
    const user = json.data;
    callback(user);
  }
}
,
*
add(params, {call, put})
{
  const {payload, callback, fetchValue} = params;

  const json = yield call(addMetadata, payload);
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
delete (params, {call, put})
{
  const {payload, fetchValue, callback} = params;
  const {list} = payload;
  const json = yield call(deleteMetadata, {list});
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
  const json = yield call(updateMetadata, payload);
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

  const json = yield call(updateEnabled, payload);
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
syncDa(params, {call})
{
  const {payload} = params;
  yield call(syncData, payload);

}
,
*
reload(params, {put})
{
  const {fetchValue} = params;
  const {appName, currentPage, pageSize} = fetchValue;
  const payload = {appName, currentPage, pageSize};
  yield put({type: "fetch", payload});
}
,


},

reducers: {
  saveUsers(state, {payload})
  {
    return {
      ...state,
      userList: payload.dataList,
      total: payload.total
    };
  }
}
}
;
