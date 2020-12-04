package org.dromara.soul.admin.utils;

import org.dromara.soul.common.exception.SoulException;
import org.junit.Test;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThat;


public class CipherUtilsTest {

	private static final String AES_KEY = "2095132720951327";

	@Test
	public void testEncryptHex() {
		assertThat(CipherUtils.encryptHex("123456", AES_KEY), is("jHcpKkiDbbQh7W7hh8yQSA=="));
	}

	@Test
	public void testEncryptHexForNull() {
		assertThat(CipherUtils.encryptHex("", AES_KEY), emptyString());
	}

	@Test
	public void testDecryptStr() {
		assertThat(CipherUtils.decryptStr("jHcpKkiDbbQh7W7hh8yQSA==", AES_KEY), is("123456"));
	}

	@Test(expected = SoulException.class)
	public void testDecryptStrForErrorStringThrowsException() {
		assertThat(CipherUtils.decryptStr("jHcpKkiDbbQh7W7hh8yQSA=", AES_KEY), notNullValue());
	}

	@Test(expected = SoulException.class)
	public void testDecryptStrForEmptyStringThrowsException() {
		assertThat(CipherUtils.decryptStr("", AES_KEY), notNullValue());
	}

	@Test(expected = AssertionError.class)
	public void testDecryptStrForNullThrowsException() {
		assertThat(CipherUtils.decryptStr(null, AES_KEY), notNullValue());
	}
}

