import React, {Component} from 'react';
import {connect} from 'dva';
import {Alert} from 'antd';
import Login from 'components/Login';
import styles from './Login.less';

const {UserName, Password, Submit} = Login;

@connect(({login, loading}) = > ({
  login,
  submitting: loading.effects['login/login'],
})
)
export default class LoginPage extends Component {

  handleSubmit = (err, values) =
> {

  const {
  dispatch
}
= this.props;
if (!err) {
  dispatch({
    type: 'login/login',
    payload: {
      ...values,
    },
  });
}
}
;

renderMessage = content =
>
{
  return
<
  Alert
  style = {
  {
    marginBottom: 24
  }
}
  message = {content}
  type = "error"
  showIcon / >;
}
;

render()
{
  const {submitting} = this.props;

  return (
    < div
  className = {styles.main} >
    < Login
  onSubmit = {this.handleSubmit} >
    < div >
    < UserName
  name = "userName"
  placeholder = "Account" / >
    < Password
  name = "password"
  placeholder = "Password" / >
    < /div>
    < Submit
  loading = {submitting} > Login < /Submit>
    < /Login>
    < /div>
)
  ;
}
}
