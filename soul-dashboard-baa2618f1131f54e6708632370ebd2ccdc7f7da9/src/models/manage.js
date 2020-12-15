import {message} from "antd";
import {
  getAllUsers,
  findUser,
  updateUser,
  deleteUser,
  addUser
} from "../services/api";

export default {
  namespace: "manage",

  state: {
    userList: [],
    total: 0
  },

  effects: {
    * fetch(params, {call, put})
{
  const {payload} = params;
  const json = yield call(getAllUsers, payload);
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
  const json = yield call(findUser, payload);
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
  const json = yield call(addUser, payload);
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
  const json = yield call(deleteUser, {list});
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
  const json = yield call(updateUser, payload);
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
  const {userName, currentPage, pageSize} = fetchValue;
  const payload = {userName, currentPage, pageSize};
  yield put({type: "fetch", payload});
}
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
