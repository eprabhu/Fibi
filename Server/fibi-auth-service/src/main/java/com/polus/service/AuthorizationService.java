package com.polus.service;

import org.springframework.http.ResponseEntity;

import com.polus.dto.AuthRequest;
import com.polus.dto.AuthResponse;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

public interface AuthorizationService {

	/**
	 * @param username
	 * @return
	 */
	String generateToken(String username);

	/**
	 * @param token
	 */
	void validateToken(String token);

	/**
	 * @param username
	 * @return
	 */
	ResponseEntity<AuthResponse> checkAuthorization(HttpServletRequest request, HttpServletResponse servletResponse);

	/**
	 * @param request
	 * @return
	 */
	ResponseEntity<AuthResponse> checkSSOAuthorization(HttpServletRequest request, HttpServletResponse response);

	ResponseEntity<?> verifyCode(AuthRequest vo);

	/**
	 *
	 * @param request
	 * @param response
	 */
	void logout(HttpServletRequest request, HttpServletResponse response);
}
