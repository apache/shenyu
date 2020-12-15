import mockjs from 'mockjs';
import {getNotices} from './mock/notices';

import {getUsers} from './mock/user';
import {getPlatform} from './mock/platform'
import {getPlugin} from './mock/plugin'
import {format, delay} from 'roadhog-api-doc';

// 是否禁用代理
const noProxy = process.env.NO_PROXY === 'true';

// 代码中会兼容本地 service mock 以及部署站点的静态数据
const proxy = {
  'GET /platform': {
    $body: getPlatform,
  },
  'GET /dashboardUser': {
    $params: {
      userName: 'ADMIN',
      currentPage: 1,
      pageSize: 10,
    },
    $body: getUsers(),
  },

  'GET /dashboardUser/1': {
    "code": 200,
    "message": "detail dashboard user success",
    "data": {
      "id": "1",
      "userName": "admin",
      "password": "123456",
      "role": 1,
      "enabled": false,
      "dateCreated": "2018-06-23 15:12:22",
      "dateUpdated": "2018-06-23 15:12:23"
    }
  },
  'POST /dashboardUser': {
    $body: {
      code: 200
    },
  },
  'PUT /dashboardUser/1': {
    $body: {
      code: 200
    },
  },
  'POST /dashboardUser/delete': {
    $body: {
      code: 200
    },
  },

  'GET /plugin': {
    $body: getPlugin,
  },

  'GET /plugin/1': {
    "code": 200,
    "message": "detail dashboard user success",
    "data": {
      "id": "1",
      "name": "admin",
      "enabled": false,
    }
  },
  'POST /plugin': {
    $body: {
      code: 200
    },
  },
  'PUT /plugin/1': {
    $body: {
      code: 200
    },
  },
  'POST /plugin/delete': {
    $body: {
      code: 200
    },
  },
  'GET /api/notices': getNotices,
  'GET /api/500': (req, res) = > {
  res.status(500).send({
    timestamp: 1513932555104,
    status: 500,
    error: 'error',
    message: 'error',
    path: '/base/category/list',
  });
},
'GET /api/404'
:
(req, res) =
>
{
  res.status(404).send({
    timestamp: 1513932643431,
    status: 404,
    error: 'Not Found',
    message: 'No message available',
    path: '/base/category/list/2121212',
  });
}
,
'GET /api/403'
:
(req, res) =
>
{
  res.status(403).send({
    timestamp: 1513932555104,
    status: 403,
    error: 'Unauthorized',
    message: 'Unauthorized',
    path: '/base/category/list',
  });
}
}
;

export default (noProxy ? {} : delay(proxy, 1000));
