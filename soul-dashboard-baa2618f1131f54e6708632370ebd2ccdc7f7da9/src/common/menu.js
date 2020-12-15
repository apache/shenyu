import {isUrl} from '../utils/utils';
import {getIntlContent} from '../utils/IntlUtils'

/** 菜单 移到 统一地方 处理 */
export const menuData = [
  {
    name: getIntlContent("SOUL.MENU.PLUGIN.LIST"),
    icon: 'dashboard',
    path: 'plug',
    children: [
      {
        name: 'divide',
        path: 'divide',
        id: 'divide6'
      },
      {
        name: 'hystrix',
        path: 'hystrix',
        id: 'hystrix9'
      }
    ],
  },
  {
    name: getIntlContent("SOUL.MENU.SYSTEM.MANAGMENT"),
    icon: 'setting',
    path: 'system',
    children: [
      {
        name: getIntlContent("SOUL.MENU.SYSTEM.MANAGMENT.USER"),
        path: 'manage',
      },
      {
        name: getIntlContent("SOUL.MENU.SYSTEM.MANAGMENT.PLUGIN"),
        path: 'plugin',
      },
      {
        name: getIntlContent("SOUL.MENU.SYSTEM.MANAGMENT.AUTHEN"),
        path: 'auth'
      },
      {
        name: getIntlContent("SOUL.MENU.SYSTEM.MANAGMENT.METADATA"),
        path: 'metadata'
      },
      {
        name: getIntlContent("SOUL.MENU.SYSTEM.MANAGMENT.DICTIONARY"),
        path: 'dict'
      }

    ],
  },
];

function formatter(data, parentPath = '/', parentAuthority) {
  return data.map(item = > {
    let {path} = item;
  if (!isUrl(path)) {
    path = parentPath + item.path;
  }
  const result = {
    ...item,
    path,
    authority: item.authority || parentAuthority,
  };
  if (item.children) {
    result.children = formatter(item.children, `${parentPath}${item.path}/`, item.authority);
  }

  return result;
})
  ;
}

export const getMenuData = () =
>
formatter(menuData);
