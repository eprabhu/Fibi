package com.polus.controller;

import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.polus.dto.AuthRequest;
import com.polus.dto.AuthResponse;
import com.polus.entity.Person;
import com.polus.service.AuthService;
import com.polus.service.PersonService;

@RestController
@RequestMapping("/auth")
public class AuthController {
    @Autowired
    private AuthService service;
    
    @Autowired
    private PersonService personService;

    @Autowired
    private AuthenticationManager authenticationManager;

    
    private static final Logger LOGGER
    = LoggerFactory.getLogger(AuthController.class);
    
    @PostMapping("/login")
    public ResponseEntity<AuthResponse> getToken(@RequestBody AuthRequest authRequest) {
        
    	LOGGER.info("Inside Login Controller.");
    	Authentication authenticate = authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(authRequest.getUsername(), authRequest.getPassword()));
        String token;
        AuthResponse response = new AuthResponse();
        if (authenticate.isAuthenticated()) {
        	token = service.generateToken(authRequest.getUsername());
        	Optional<Person> optionalPerson = personService.loadUserByUsername(authRequest.getUsername());
        	response =   AuthResponse.builder()
	        			.personId(optionalPerson.get().getPersonId())
	        			.userName(optionalPerson.get().getPrincipalName())
	        			.fullName(optionalPerson.get().getFullName())
	        			.homeUnit(optionalPerson.get().getHomeUnit())
	        			.gender(optionalPerson.get().getGender())
	        			.primaryTitle(optionalPerson.get().getPrimaryTitle())
                        .isFaculty(optionalPerson.get().getIsFaculty())
                        .homeUnitName(optionalPerson.get().getUnit() != null ? optionalPerson.get().getUnit().getUnitName() : null)
	        			.build();
        	LOGGER.info("Token generated succefully.");
        } else {
            throw new RuntimeException("invalid access");
        }
        HttpHeaders headers = new HttpHeaders();
	    headers.setBearerAuth(token);
	    
	    return ResponseEntity.ok()
	            .headers(headers)
	            .body((response));
	    
    }
        
    @GetMapping("/validate")
    public String validateToken(@RequestParam("token") String token) {
        service.validateToken(token);
        return "Token is valid";
    }
    
    }
