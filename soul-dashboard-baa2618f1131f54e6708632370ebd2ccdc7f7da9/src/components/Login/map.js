import React from 'react';
import {Input, Icon} from 'antd';
import styles from './index.less';
import {getIntlContent} from '../../utils/IntlUtils';

const map = {
  UserName: {
    component: Input,
    props: {
      size: 'large',
      prefix: < Icon type = "user" className = {styles.prefixIcon}
/>,
placeholder: 'admin',
},
rules: [
  {
    required: true,
    message: getIntlContent("SOUL.SYSTEM.USER.NAME"),
  },
],
},
Password: {
  component: Input,
    props
:
  {
    size: 'large',
      prefix
  : <
    Icon
    type = "lock"
    className = {styles.prefixIcon}
    />,
    type: 'password',
      placeholder
  :
    '888888',
  }
,
  rules: [
    {
      required: true,
      message: getIntlContent("SOUL.SYSTEM.USER.PASSWORD"),
    },
  ],
}
,
Mobile: {
  component: Input,
    props
:
  {
    size: 'large',
      prefix
  : <
    Icon
    type = "mobile"
    className = {styles.prefixIcon}
    />,
    placeholder: 'mobile number',
  }
,
  rules: [
    {
      required: true,
      message: 'Please enter mobile number!',
    },
    {
      pattern: /^1\d{10}$/,
      message: 'Wrong mobile number format!',
    },
  ],
}
,
Captcha: {
  component: Input,
    props
:
  {
    size: 'large',
      prefix
  : <
    Icon
    type = "mail"
    className = {styles.prefixIcon}
    />,
    placeholder: 'captcha',
  }
,
  rules: [
    {
      required: true,
      message: 'Please enter Captcha!',
    },
  ],
}
,
}
;

export default map;
