package com.polus.service;

import java.security.MessageDigest;

import org.apache.commons.codec.binary.Base64;
import org.springframework.security.crypto.password.PasswordEncoder;

public class FibiPasswordEncoder implements PasswordEncoder {

	private static final String HASH_ALGORITHM = "SHA";
	private static final String CHARSET = "UTF-8";

	@Override
	public String encode(CharSequence rawPassword) {
		try {
			MessageDigest md = MessageDigest.getInstance(HASH_ALGORITHM);
			byte[] digest = md.digest(rawPassword.toString().getBytes(CHARSET));
			return new String(Base64.encodeBase64(digest), CHARSET);
		} catch (Exception e) {
			throw new RuntimeException("Error encoding password", e);
		}
	}

	@Override
	public boolean matches(CharSequence rawPassword, String encodedPassword) {
		String encodedRawPassword = encode(rawPassword);
		return encodedRawPassword.equals(encodedPassword);
	}
}
