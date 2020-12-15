import {message} from "antd";
import {
  getAllSelectors,
  getAllRules,
  addSelector,
  findSelector,
  deleteSelector,
  updateSelector,
  addRule,
  deleteRule,
  findRule,
  updateRule,
} from "../services/api";

export default {
  namespace: "hystrix",

  state: {
    selectorList: [],
    ruleList: [],
    selectorTotal: 0,
    ruleTotal: 0,
    currentSelector: "",
  },

  effects: {
    * fetchSelector({payload}, {call, put})
{

  const json = yield call(getAllSelectors, {...payload});
  if (json.code === 200) {
    let {page, dataList} = json.data;
    dataList = dataList.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    yield put({
      type: "saveSelector",
      payload: {
        selectorTotal: page.totalCount,
        selectorList: dataList
      }
    });

    yield put({
      type: "saveCurrentSelector",
      payload: {
        currentSelector:
          dataList && dataList.length > 0 ? dataList[0] : ""
      }
    });
    if (dataList && dataList.length > 0) {
      yield put({
        type: "fetchRule",
        payload: {
          currentPage: 1,
          pageSize: 12,
          selectorId: dataList[0].id
        }
      });
    }
  }
}
,
*
fetchRule({payload}, {call, put})
{
  const json = yield call(getAllRules, payload);
  if (json.code === 200) {
    let {page, dataList} = json.data;
    dataList = dataList.map(item = > {
      item.key = item.id;
    return item;
  })
    ;
    yield put({
      type: "saveRule",
      payload: {
        ruleTotal: page.totalCount,
        ruleList: dataList
      }
    });
  }
}
,
*
addSelector(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(addSelector, payload);
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
addRule(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(addRule, payload);
  if (json.code === 200) {
    message.success("添加成功");
    callback();
    yield put({type: "reloadRule", fetchValue});
  } else {
    message.warn(json.message);
  }
}
,

*
fetchSeItem(params, {call})
{
  const {payload, callback} = params;
  const json = yield call(findSelector, payload);
  if (json.code === 200) {
    const selector = json.data;
    callback(selector);
  }
}
,
*
deleteSelector(params, {call, put})
{
  const {payload, fetchValue} = params;
  const {list} = payload;
  const json = yield call(deleteSelector, {list});
  if (json.code === 200) {
    message.success("删除成功");
    yield put({
      type: "saveRule",
      payload: {
        ruleTotal: 0,
        ruleList: []
      }
    });
    yield put({type: "reload", fetchValue});
  } else {
    message.warn(json.message);
  }
}
,
*
updateSelector(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(updateSelector, payload);
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
deleteRule(params, {call, put})
{
  const {payload, fetchValue} = params;
  const {list} = payload;
  const json = yield call(deleteRule, {list});
  if (json.code === 200) {
    message.success("删除成功");
    yield put({type: "reloadRule", fetchValue});
  } else {
    message.warn(json.message);
  }
}
,
*
fetchRuleItem(params, {call})
{
  const {payload, callback} = params;
  const json = yield call(findRule, payload);
  if (json.code === 200) {
    const rule = json.data;
    callback(rule);
  }
}
,
*
updateRule(params, {call, put})
{
  const {payload, callback, fetchValue} = params;
  const json = yield call(updateRule, payload);
  if (json.code === 200) {
    message.success("修改成功");
    callback();
    yield put({type: "reloadRule", fetchValue});
  } else {
    message.warn(json.message);
  }
}
,

*
reload(params, {put})
{
  const {fetchValue} = params;
  const {pluginId, currentPage, pageSize} = fetchValue;
  const payload = {pluginId, currentPage, pageSize};
  yield put({type: "fetchSelector", payload});
}
,

*
reloadRule(params, {put})
{
  const {fetchValue} = params;
  const {selectorId, currentPage, pageSize} = fetchValue;
  const payload = {selectorId, currentPage, pageSize};
  yield put({type: "fetchRule", payload});
}
},

reducers: {
  saveSelector(state, {payload})
  {
    return {
      ...state,
      selectorList: payload.selectorList,
      selectorTotal: payload.selectorTotal
    };
  }
,

  saveRule(state, {payload})
  {
    return {
      ...state,
      ruleList: payload.ruleList,
      ruleTotal: payload.ruleTotal
    };
  }
,
  saveCurrentSelector(state, {payload})
  {
    return {
      ...state,
      currentSelector: payload.currentSelector
    };
  }
}
}
;
