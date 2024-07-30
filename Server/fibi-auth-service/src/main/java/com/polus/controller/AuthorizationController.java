package com.polus.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.polus.dto.AuthRequest;
import com.polus.dto.AuthResponse;
import com.polus.service.AuthorizationService;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@RestController
@RequestMapping("/auth")
public class AuthorizationController {

	@Autowired
	private AuthorizationService authorizationService;

	private static final Logger LOGGER = LoggerFactory.getLogger(AuthorizationController.class);

	@PostMapping("/login")
	public ResponseEntity<AuthResponse> login(HttpServletRequest request, HttpServletResponse response) {
		LOGGER.info("checkAuthorization");
		return authorizationService.checkAuthorization(request, response);
	}

	@GetMapping("/logout")
	public void logout(HttpServletRequest request, HttpServletResponse response) {
		LOGGER.info("logout....!");
		authorizationService.logout(request, response);
	}

	@GetMapping("/validate")
	public String validateToken(@RequestParam("token") String token) {
		authorizationService.validateToken(token);
		return "Token is valid";
	}

	@PostMapping("/verify")
	public ResponseEntity<?> verifyCode(@RequestBody AuthRequest vo) {
		return authorizationService.verifyCode(vo);
	}
}
