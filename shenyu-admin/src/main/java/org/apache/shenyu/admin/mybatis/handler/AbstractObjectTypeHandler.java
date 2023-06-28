package org.apache.shenyu.admin.mybatis.handler;

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.util.StringUtils;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * AbstractObjectTypeHandler
 */
public abstract class AbstractObjectTypeHandler<T> extends BaseTypeHandler<T> {
	
	@Override
	public void setNonNullParameter(PreparedStatement ps, int i, Object parameter,
	                                JdbcType jdbcType) throws SQLException {
		ps.setString(i, GsonUtils.getGson().toJson(parameter));
	}
	
	@Override
	public T getNullableResult(ResultSet rs, String columnName)
			throws SQLException {
		String data = rs.getString(columnName);
		return StringUtils.hasText(data) ? GsonUtils.getGson().fromJson(data, (Class<T>) getRawType()) : null;
	}
	
	@Override
	public T getNullableResult(ResultSet rs, int columnIndex) throws SQLException {
		String data = rs.getString(columnIndex);
		return StringUtils.hasText(data) ? GsonUtils.getGson().fromJson(data, (Class<T>) getRawType()) : null;
	}
	
	@Override
	public T getNullableResult(CallableStatement cs, int columnIndex)
			throws SQLException {
		String data = cs.getString(columnIndex);
		return StringUtils.hasText(data) ? GsonUtils.getGson().fromJson(data, (Class<T>) getRawType()) : null;
	}
}
