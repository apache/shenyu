import {message} from "antd";
import {
  addSoulDict,
  batchDeleteSoulDict,
  fetchSoulDicts,
  findSoulDict,
  updateSoulDict,
  fetchSoulDictByType,
  updateSoulDictEnabled,
} from "../services/api";


export default {
  namespace: "soulDict",

  state: {
    soulDictList: [],
    total: 0
  },

  effects: {
    * fetch(params, {call, put})
{
  const {payload} = params;
  const json = yield call(fetchSoulDicts, payload);
  if (json.code === 200) {
    let {page, dataList} = json.data;
    dataList = dataList.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    yield put({
      type: "saveSoulDicts",
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
  const json = yield call(addSoulDict, payload);
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
  const json = yield call(findSoulDict, payload);
  if (json.code === 200) {
    const soulDict = json.data;
    callback(soulDict);
  }
}
,
*
update(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(updateSoulDict, payload);
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
  const json = yield call(batchDeleteSoulDict, {list});
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
updateEn(params, {call, put})
{
  const {payload, fetchValue, callback} = params;
  const json = yield call(updateSoulDictEnabled, payload);
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
fetchByType(params, {call})
{
  const {payload} = params;
  let callback = payload.callBack;
  const json = yield call(fetchSoulDictByType, payload);
  if (json.code === 200) {
    let dataList = json.data.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    callback(dataList);
  }
}
,
},
reducers: {
  saveSoulDicts(state, {payload})
  {
    return {
      ...state,
      soulDictList: payload.dataList,
      total: payload.total
    };
  }
,

*
  reload(params, {put})
  {
    const {fetchValue} = params;
    const {type, dictCode, dictName, currentPage, pageSize} = fetchValue;
    const payload = {type, dictCode, dictName, currentPage, pageSize};
    yield put({type: "fetch", payload});
  }
,
}
}
;
