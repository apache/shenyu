export const getUsers = () =
>
{
  return {
    code: 200,
    data: {
      page: {
        currentPage: 1,
        prePage: 1,
        nextPage: 1,
        pageSize: 10,
        offset: 0,
        totalPage: 1,
        totalCount: 1,
      },
      dataList: [
        {
          id: '1',
          userName: 'ADMIN',
          password: '123456',
          role: 1,
          enabled: true,
          dateCreated: '2018-07-28 13:38:05',
          dateUpdated: '2018-07-28 13:38:05',
        },
        {
          id: '2',
          userName: 'ADMIN2',
          password: '123456',
          role: 1,
          enabled: true,
          dateCreated: '2018-07-28 13:38:05',
          dateUpdated: '2018-07-28 13:38:05',
        },
        {
          id: '3',
          userName: 'User',
          password: '123456',
          role: 1,
          enabled: true,
          dateCreated: '2018-07-28 13:38:05',
          dateUpdated: '2018-07-28 13:38:05',
        },
      ],
    },
  }

}
;
export default {
  getUsers,
};
