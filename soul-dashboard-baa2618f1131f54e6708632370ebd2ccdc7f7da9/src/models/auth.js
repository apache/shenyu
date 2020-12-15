import {message} from 'antd';
import {
  getAllAuths,
  findAuthData,
  findAuthDataDel,
  updateAuthData,
  updateAuthDel,
  updateAuthEnabled,
  deleteAuths,
  addAuthData,
  syncAuthsData,
  getAllMetadata,
  getAllMetadatas,
  getfetchMetaGroup
} from '../services/api';

export default {
  namespace: "auth",

  state: {
    authList: [],
    total: 0
  },

  effects: {
    * fetch(params, {call, put})
{
  const {payload} = params;
  const json = yield call(getAllAuths, payload);
  if (json.code === 200) {
    let {page, dataList} = json.data;
    dataList = dataList.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    yield put({
      type: "saveAuths",
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
  const json = yield call(findAuthData, payload);
  if (json.code === 200) {
    const auth = json.data;
    callback(auth);
  }
}
,
*
fetchItemDel(params, {call})
{
  const {payload, callback} = params;
  const json = yield call(findAuthDataDel, payload);
  if (json.code === 200) {
    const auth = json.data;
    callback({auth});
  }
}
,
*
fetchMeta(params, {call})
{
  const {payload, callback} = params;
  const json = yield call(getAllMetadatas, payload);
  if (json.code === 200) {
    // let { page, dataList } = json.data;

    let dataList = json.data.map(item = > {
      // item.key = item.id;
      item = {id: item.id, path: item.path, appName: item.appName, enabled: item.enabled}
      return item;
  })
    ;
    callback({dataList});
    // yield put({
    //   type: "saveUsers",
    //   payload: {
    //     total: page.totalCount,
    //     dataList
    //   }
    // });
  }
}
,
*
fetchMetaGroup(params, {call})
{
  const {payload, callback} = params;
  const json = yield call(getfetchMetaGroup, payload);
  if (json.code === 200) {

    callback(json.data)
  }
}
,
*
add(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(addAuthData, payload);
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

  const json = yield call(deleteAuths, payload);
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

  const json = yield call(updateAuthData, payload);
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
updateDel(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(updateAuthDel, payload);
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
reload(params, {put})
{
  const {fetchValue} = params;
  const {name, currentPage, pageSize} = fetchValue;
  const payload = {name, currentPage, pageSize};
  yield put({type: "fetch", payload});
}
,
*
updateEn(params, {call, put})
{
  const {payload, fetchValue, callback} = params;
  const json = yield call(updateAuthEnabled, payload);
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
syncDa(params, {call})
{
  const {payload} = params;
  yield call(syncAuthsData, payload);
}
,
*
getDatas(params, {call})
{
  const {payload} = params;
  yield call(getAllMetadata, payload)
}
},

reducers: {
  saveAuths(state, {payload})
  {
    return {
      ...state,
      authList: payload.dataList,
      total: payload.total
    };
  }
,
  // saveUsers(state, { payload }) {
  //   return {
  //     ...state,
  //     userList: payload.dataList,
  //     total: payload.total
  //   };
  // }
}
}
;
