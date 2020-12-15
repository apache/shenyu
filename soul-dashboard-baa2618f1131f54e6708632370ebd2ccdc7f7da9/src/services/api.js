import {stringify} from "qs";
import request from "../utils/request";

const baseUrl = document.getElementById("httpPath").innerHTML;
/* 添加用户 */
export
async

function addUser(params) {
  return request(`${baseUrl}/dashboardUser`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

/* 删除用户 */
export
async

function deleteUser(params) {
  return request(`${baseUrl}/dashboardUser/batch`, {
    method: `DELETE`,
    body: [...params.list
]
})
  ;
}

/* 修改用户 */
export
async

function updateUser(params) {
  return request(`${baseUrl}/dashboardUser/${params.id}`, {
    method: `PUT`,
    body: {
      userName: params.userName,
      password: params.password,
      role: params.role,
      enabled: params.enabled
    }
  });
}

/* 查询所有元数据 */
export
async

function getAllMetadata(params) {
  const {appName, currentPage, pageSize} = params;
  let myParams = params;
  if (appName) {
    myParams = params;
  } else {
    myParams = {currentPage, pageSize};
  }

  return request(`${baseUrl}/meta-data/queryList?${stringify(myParams)}`, {
    method: `GET`
  });
}

/* 查询单个元数据 */
// export async function findMetadata(params) {
//   return request(`${baseUrl}/meta-data/queryList/${params.id}`, {
//     method: `GET`
//   });
// }
export
async

function findMetadata(params) {
  // const { appName, currentPage, pageSize } = params;
  // let myParams = params;
  // if (appName) {
  //   myParams = params;
  // } else {
  //   myParams = { currentPage, pageSize };
  // }
  return request(`${baseUrl}/meta-data/${params.id}`, {
    method: `GET`
  });
}

/* 添加元数据 */
export
async

function addMetadata(params) {
  return request(`${baseUrl}/meta-data/createOrUpdate`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

/* 修改元数据 */
export
async

function updateMetadata(params) {
  return request(`${baseUrl}/meta-data/createOrUpdate`, {
    method: `POST`,
    body: {
      appName: params.appName,
      enabled: params.enabled,
      id: params.id,
      pathDesc: params.pathDesc,
      methodName: params.methodName,
      parameterTypes: params.parameterTypes,
      path: params.path,
      rpcExt: params.rpcExt,
      rpcType: params.rpcType,
      serviceName: params.serviceName
    }
  });
}

/* 同步元数据 */
export
async

function syncData() {
  return request(`${baseUrl}/meta-data/syncData`, {
    method: `POST`,
    body: {}
  });
}

/* 获取所有(按照应用名称分组) */
export
async

function getfetchMetaGroup() {
  return request(`${baseUrl}/meta-data/findAllGroup`, {
    method: `GET`
  })
}

/* 删除元数据 */
export
async

function deleteMetadata(params) {
  return request(`${baseUrl}/meta-data/batchDeleted`, {
    method: `POST`,
    body: [...params.list
]
})
  ;
}

/* 元数据中的批量启用或禁用 */
export
async

function updateEnabled(params) {

  return request(`${baseUrl}/meta-data/batchEnabled`, {
    method: `POST`,
    body: {
      ids: params.list,
      enabled: params.enabled
    }
  });
}

/* 查询所有用户 */
export
async

function getAllUsers(params) {
  const {userName, currentPage, pageSize} = params;
  let myParams = params;
  if (userName) {
    myParams = params;
  } else {
    myParams = {currentPage, pageSize};
  }
  return request(`${baseUrl}/dashboardUser?${stringify(myParams)}`, {
    method: `GET`
  });
}

/* 查询单个用户 */
export
async

function findUser(params) {
  return request(`${baseUrl}/dashboardUser/${params.id}`, {
    method: `GET`
  });
}

// 插件管理
/* 增加插件 */
export
async

function addPlugin(params) {
  return request(`${baseUrl}/plugin`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

/* 删除插件 */
export
async

function deletePlugin(params) {
  return request(`${baseUrl}/plugin/batch`, {
    method: `DELETE`,
    body: [...params.list
]
})
  ;
}

/* 修改插件 */
export
async

function updatePlugin(params) {
  return request(`${baseUrl}/plugin/${params.id}`, {
    method: `PUT`,
    body: {
      ids: [params.id],
      name: params.name,
      role: params.role,
      config: params.config,
      enabled: params.enabled
    }
  });
}

/* 查询所有插件 */
export
async

function getAllPlugins(params) {
  const {name, currentPage, pageSize} = params;
  let myParams = params;
  if (name) {
    myParams = params;
  } else {
    myParams = {currentPage, pageSize};
  }
  return request(`${baseUrl}/plugin?${stringify(myParams)}`, {
    method: `GET`
  });
}

/* 查询单个插件 */
export
async

function findPlugin(params) {
  return request(`${baseUrl}/plugin/${params.id}`, {
    method: `GET`
  });
}

/* 批量更改装态 */
export
async

function updatepluginEnabled(params) {
  return request(`${baseUrl}/plugin/enabled`, {
    method: `POST`,
    body: {
      ids: params.list,
      enabled: params.enabled
    }
  })
}

// 认证管理
/* 增加认证 */
export
async

function addAuth(params) {
  return request(`${baseUrl}/appAuth`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

/* 删除认证 */
export
async

function deleteAuth(params) {
  return request(`${baseUrl}/appAuth/batch`, {
    method: `DELETE`,
    body: [...params.list
]
})
  ;
}

/* 修改认证 */
export
async

function updateAuth(params) {
  return request(`${baseUrl}/appAuth/${params.id}`, {
    method: `PUT`,
    body: {
      appKey: params.appKey,
      appSecret: params.appSecret,
      enabled: params.enabled
    }
  });
}

/* 查询所有认证 */
export
async

function getAllAuth(params) {
  const {appKey, currentPage, pageSize} = params;
  let myParams = params;
  if (appKey) {
    myParams = params;
  } else {
    myParams = {currentPage, pageSize};
  }
  return request(`${baseUrl}/appAuth?${stringify(myParams)}`, {
    method: `GET`
  });
}

/* 同步Auth */
export
async

function syncAuthsData() {
  return request(`${baseUrl}/appAuth/syncData`, {
    method: `POST`,
    body: {}
  })
}

/* 查询所有Auth */
export
async

function getAllAuths(params) {
  const {appKey, phone, currentPage, pageSize} = params;
  let myParams = params;
  if (appKey || phone) {
    myParams = params;
  } else {
    myParams = {currentPage, pageSize};
  }
  return request(`${baseUrl}/appAuth/findPageByQuery?${stringify(myParams)}`, {
    method: `GET`
  });
}

/* 查询单个Auth */
export
async

function findAuthData(params) {
  return request(`${baseUrl}/appAuth/detail?id=${params.id}`, {
    method: `GET`
  });
}

/* 查询单个Auth详情 */
export
async

function findAuthDataDel(params) {
  return request(`${baseUrl}/appAuth/detailPath?id=${params.id}`, {
    method: `GET`
  });
}

/* 查询所有需用到的元数据 */
export
async

function getAllMetadatas() {
  return request(`${baseUrl}/meta-data/findAll`, {
    method: `GET`
  })
}

/* 修改Auth */
export
async

function updateAuthData(params) {

  return request(`${baseUrl}/appAuth/updateDetail`, {
    method: `POST`,
    body: {
      ...params
    }
  })
}

/* 修改AuthDel */
export
async

function updateAuthDel(params) {
  return request(`${baseUrl}/appAuth/updateDetailPath`, {
    method: `POST`,
    body: params
  })
}

/* 添加Auth */
export
async

function addAuthData(params) {
  return request(`${baseUrl}/appAuth/apply`, {
    method: `POST`,
    body: {
      ...params
    }
  })
}

/* Auth中的批量启用或禁用 */
export
async

function updateAuthEnabled(params) {
  return request(`${baseUrl}/appAuth/batchEnabled`, {
    method: `POST`,
    body: {
      ids: params.list,
      enabled: params.enabled
    }
  })
}

/* 批量删除Auth */
export
async

function deleteAuths(params) {
  return request(`${baseUrl}/appAuth/batchDelete`, {
    method: `POST`,
    body: [...params.list
]
})
}

/* 查询单个认证 */
export
async

function findAuth(params) {
  return request(`${baseUrl}/appAuth/${params.id}`, {
    method: `GET`
  });
}

// 选择器管理

/* 增加选择器 */
export
async

function addSelector(params) {
  return request(`${baseUrl}/selector`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

/* 删除选择器 */
export
async

function deleteSelector(params) {
  return request(`${baseUrl}/selector/batch`, {
    method: `DELETE`,
    body: [...params.list
]
})
  ;
}

/* 修改选择器 */
export
async

function updateSelector(params) {
  return request(`${baseUrl}/selector/${params.id}`, {
    method: `PUT`,
    body: {
      ...params
    }
  });
}

/* 查询所有选择器 */
export
async

function getAllSelectors(params) {
  return request(`${baseUrl}/selector?${stringify(params)}`, {
    method: `GET`
  });
}

/* 查询单个选择器 */
export
async

function findSelector(params) {
  return request(`${baseUrl}/selector/${params.id}`, {
    method: `GET`
  });
}

export
async

function getAllRules(params) {
  return request(`${baseUrl}/rule?${stringify(params)}`, {
    method: `GET`
  });
}

export
async

function addRule(params) {
  return request(`${baseUrl}/rule`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

export
async

function deleteRule(params) {
  return request(`${baseUrl}/rule/batch`, {
    method: `DELETE`,
    body: [...params.list
]
})
  ;
}

export
async

function findRule(params) {
  return request(`${baseUrl}/rule/${params.id}`, {
    method: `GET`
  });
}

export
async

function updateRule(params) {
  return request(`${baseUrl}/rule/${params.id}`, {
    method: `PUT`,
    body: {
      ...params
    }
  });
}

/* 查询所有常量 */
export
async

function queryPlatform() {
  return request(`${baseUrl}/platform/enum`, {
    method: `GET`
  });
}

/* 登录 */
export
async

function queryLogin(params) {
  return request(`${baseUrl}/platform/login?${stringify(params)}`, {
    method: `GET`
  });
}

// 同步所有插件
export
async

function asyncPlugin() {
  return request(`${baseUrl}/plugin/syncPluginAll`, {
    method: `POST`
  });
}

// 同步单个插件
export
async

function asyncOnePlugin(params) {
  return request(`${baseUrl}/plugin/syncPluginData/${params.id}`, {
    method: `PUT`
  });
}

// 根据插件id 获取插件处理字段列表
export
async

function fetchPluginHandles(params) {
  return request(`${baseUrl}/plugin-handle?${stringify(params)}`, {
    method: `GET`
  });
}

// 添加插件处理字段
export
async

function addPluginHandle(params) {
  return request(`${baseUrl}/plugin-handle`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

// 获取插件处理字段详情
export
async

function findPluginHandle(params) {
  return request(`${baseUrl}/plugin-handle/${params.id}`, {
    method: 'GET'
  })
}

// 更新插件处理字段
export
async

function updatePluginHandle(params) {
  return request(`${baseUrl}/plugin-handle/${params.id}`, {
    method: `PUT`,
    body: {
      ...params
    }
  })
}

// 批量删除插件处理字段

export
async

function batchDeletePluginHandle(params) {
  return request(`${baseUrl}/plugin-handle/batch`, {
    method: `DELETE`,
    body: [...params.list
]
})
  ;
}

export function fetchPluginHandleByPluginId(params) {
  return request(`${baseUrl}/plugin-handle/all/${params.pluginId}/${params.type}`, {
    method: `GET`
  });
}

// 获取字典列表
export
async

function fetchSoulDicts(params) {
  return request(`${baseUrl}/soul-dict?${stringify(params)}`, {
    method: `GET`
  });
}

// 添加字典
export
async

function addSoulDict(params) {
  return request(`${baseUrl}/soul-dict`, {
    method: `POST`,
    body: {
      ...params
    }
  });
}

// 获取字典详情
export
async

function findSoulDict(params) {
  return request(`${baseUrl}/soul-dict/${params.id}`, {
    method: 'GET'
  })
}

// 更新字典
export
async

function updateSoulDict(params) {
  return request(`${baseUrl}/soul-dict/${params.id}`, {
    method: `PUT`,
    body: {
      ...params
    }
  })
}

// 批量删除字典
export
async

function batchDeleteSoulDict(params) {
  return request(`${baseUrl}/soul-dict/batch`, {
    method: `DELETE`,
    body: [...params.list
]
})
  ;
}

export function fetchSoulDictByType(params) {
  return request(`${baseUrl}/soul-dict/all/${params.type}`, {
    method: `GET`
  });
}

export
async

function updateSoulDictEnabled(params) {
  return request(`${baseUrl}/soul-dict/batchEnabled`, {
    method: `POST`,
    body: {
      ids: params.list,
      enabled: params.enabled
    }
  })
}
