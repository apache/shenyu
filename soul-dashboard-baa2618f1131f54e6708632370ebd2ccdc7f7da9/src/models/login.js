import {routerRedux} from "dva/router";
import {stringify} from "qs";
import {message} from "antd";
import {queryLogin} from "../services/api";
import {setAuthority} from "../utils/authority";
import {reloadAuthorized} from "../utils/Authorized";
// import { getPageQuery } from "../utils/utils";

export default {
  namespace: "login",

  state: {
    status: undefined
  },

  effects: {
    * login({payload}, {call, put})
{
  const response = yield call(queryLogin, payload);

  // Login successfully

  if (response.data) {
    yield put({
      type: "changeLoginStatus",
      payload: {
        state: true,
        currentAuthority: "admin"
      }
    });

    reloadAuthorized();
    /* const urlParams = new URL(window.location.href);
     const params = getPageQuery();
     let { redirect } = params;
     if (redirect) {
       const redirectUrlParams = new URL(redirect);
       if (redirectUrlParams.origin === urlParams.origin) {
         redirect = redirect.substr(urlParams.origin.length);
         if (redirect.startsWith("/#")) {
           redirect = redirect.substr(2);
         }
       } else {
         window.location.href = redirect;
         return;
       }
     } */

    yield put(routerRedux.push("/home"));
  } else {
    message.destroy();
    message.error("用户名或密码不正确");
  }
}
,
*
logout(_, {put})
{
  yield put({
    type: "changeLoginStatus",
    payload: {
      status: false,
      currentAuthority: ""
    }
  });
  setAuthority("");
  reloadAuthorized();
  yield put(
    routerRedux.push({
      pathname: "/user/login",
      search: stringify({
        redirect: window.location.href
      })
    })
  );
}
},

reducers: {
  changeLoginStatus(state, {payload})
  {
    setAuthority("admin");
    return {
      ...state,
      status: payload.status
    };
  }
}
}
;
