import React, {Component} from 'react';
import PropTypes from 'prop-types';
import {Tabs} from 'antd';

const {TabPane} = Tabs;

const generateId = (() = > {
  let i = 0;
return (prefix = '') =
>
{
  i += 1;
  return `${prefix}${i}`;
}
;
})
();

export default class LoginTab extends Component {

  static contextTypes = {
    tabUtil: PropTypes.object,
  };

  constructor(props) {
    super(props);
    this.uniqueId = generateId('login-tab-');
  }

  componentWillMount() {
    const {tabUtil} = this.context;
    if (tabUtil) {
      tabUtil.addTab(this.uniqueId);
    }
  }

  static __ANT_PRO_LOGIN_TAB = true;

  render() {
    return
  <
    TabPane
    {...
      this.props
    }
    />;
  }
}
