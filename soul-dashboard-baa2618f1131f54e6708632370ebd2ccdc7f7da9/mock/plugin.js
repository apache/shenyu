export const getPlugin = (req, res) =
>
{
  res.json({
    "code": 200,
    "message": null,
    "data": {
      "page": {
        "currentPage": 1,
        "prePage": 1,
        "nextPage": 1,
        "pageSize": 10,
        "offset": 0,
        "totalPage": 1,
        "totalCount": 7,
      },
      "dataList": [
        {
          "id": "1",
          "code": 2,
          "name": "sign",
          "enabled": false,
          "dateCreated": "2018-06-14 10:17:35",
          "dateUpdated": "2018-06-14 10:17:35",
        },
        {
          "id": "2",
          "code": 10,
          "name": "waf",
          "enabled": false,
          "dateCreated": "2018-06-23 10:26:30",
          "dateUpdated": "2018-06-13 15:43:10",
        },
        {
          "id": "3",
          "code": 30,
          "name": "rewrite",
          "enabled": false,
          "dateCreated": "2018-06-23 10:26:34",
          "dateUpdated": "2018-06-25 13:59:31",
        },
        {
          "id": "4",
          "code": 20,
          "name": "rate_limiter",
          "enabled": false,
          "dateCreated": "2018-06-23 10:26:37",
          "dateUpdated": "2018-06-13 15:34:48",
        },
        {
          "id": "5",
          "code": 50,
          "name": "divide",
          "enabled": true,
          "dateCreated": "2018-06-25 10:19:10",
          "dateUpdated": "2018-06-13 13:56:04",
        },
        {
          "id": "6",
          "code": 60,
          "name": "dubbo",
          "enabled": false,
          "dateCreated": "2018-06-23 10:26:41",
          "dateUpdated": "2018-06-11 10:11:47",
        },
        {
          "id": "7",
          "code": 80,
          "name": "monitor",
          "enabled": false,
          "dateCreated": "2018-06-25 13:47:57",
          "dateUpdated": "2018-06-25 13:47:57",
        },
      ],
    },
  });
}
;
export default {
  getPlugin,
};
