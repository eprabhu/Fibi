package com.polus.fibicomp.security;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.env.Environment;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.login.dao.LoginDao;
import com.polus.fibicomp.login.service.LoginService;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.PersonDTO;
import com.polus.fibicomp.pojo.UnitAdministrator;
import com.polus.fibicomp.roles.dao.RolesManagementDao;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

public class JWTAuthenticationFilter extends UsernamePasswordAuthenticationFilter {

	protected static Logger log = LogManager.getLogger(JWTAuthenticationFilter.class.getName());

	private AuthenticationManager authenticationManager;
	private LoginDao loginDao;
	private CommonService commonService;
	private RolesManagementDao rolesManagementDao;
	private LoginService loginService;
	private Environment environment;

	public JWTAuthenticationFilter(AuthenticationManager authenticationManager, LoginDao loginDao, CommonService commonService,
			RolesManagementDao rolesManagementDao, LoginService loginService, Environment environment) {
		this.authenticationManager = authenticationManager;
		this.loginDao = loginDao;
		this.commonService = commonService;
		this.rolesManagementDao = rolesManagementDao;
		this.loginService = loginService;
		this.environment = environment;
	}

	@Override
	public Authentication attemptAuthentication(HttpServletRequest req, HttpServletResponse res) {
		try {
			if (Constants.EXTERNAL_AUTHORIZATION.equals(environment.getProperty("LOGIN_MODE"))) {
				String remoteUser = req.getRemoteUser();
				if (remoteUser == null) {
					remoteUser = (req.getUserPrincipal() != null ? req.getUserPrincipal().getName() : null);
				}
				log.info("IdP provided username: {} ", remoteUser);
				Authentication authentication = new UsernamePasswordAuthenticationToken(remoteUser, null, new ArrayList<>());
				SecurityContextHolder.getContext().setAuthentication(authentication);
				return authentication;
			} else {
				Person creds = new ObjectMapper().readValue(req.getInputStream(), Person.class);
				String encryptedPWD = getEncryptedPassword(creds.getPassword());				
				return authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(creds.getPrincipalName(), encryptedPWD, new ArrayList<>()));
			}
		} catch (IOException e) {
			log.error(String.format("Exception in attemptAuthentication: %s", e.getMessage()));	
		}
		return null;
	}

	private String getEncryptedPassword(String password) {
		try {
			return commonService.hash(password);
		} catch (GeneralSecurityException e) {
			log.error("Error occured while hashing the password: {} ", e);
		}
		return "";
	}

	@Override
	protected void successfulAuthentication(HttpServletRequest req, HttpServletResponse res, FilterChain chain, Authentication auth) throws IOException, ServletException {
		log.info("-------- successfulAuthentication --------");
		try {
			String userName = "";
			if (Constants.EXTERNAL_AUTHORIZATION.equals(environment.getProperty("LOGIN_MODE"))) {
				userName = (String) auth.getPrincipal();
			} else {
				userName = ((User) auth.getPrincipal()).getUsername();
			}
			PersonDTO personDTO = loginDao.readPersonData(userName);
			personDTO.setJwtRoles(auth.getAuthorities());
			if (personDTO.isLogin()) {
				List<UnitAdministrator> unitAdministrators = loginDao.isUnitAdmin(personDTO.getPersonID());
				if (unitAdministrators != null && !unitAdministrators.isEmpty()) {
					personDTO.setUnitAdmin(true);
				}
				personDTO.setSuperUser(rolesManagementDao.isPersonHasRole(personDTO.getPersonID(), Constants.SUPERUSER_ROLE_ID));
				personDTO.setExternalUser(loginDao.isExternalUser(personDTO.getPersonID()));
				loginService.savePersonLoginDetails(personDTO.getPersonID(), personDTO.getFullName(), Constants.LOGIN_FLAG, userName);
				String token = generateToken(userName, personDTO.getPersonID(), personDTO.getUnitNumber(), personDTO.getFullName(), personDTO.isExternalUser());
				String response = new ObjectMapper().writeValueAsString(personDTO);
				res.getWriter().write(response);
				log.info("Token:   {}", token);
				res.addHeader(Constants.HEADER_STRING, Constants.TOKEN_PREFIX + token);
			}
		} catch (Exception e) {
			log.error("Exception in successfulAuthentication {}", e.getMessage());
		}
	}

	@Override
	protected void unsuccessfulAuthentication(HttpServletRequest request, HttpServletResponse res, AuthenticationException failed) throws IOException, ServletException {
		log.info("-------- unsuccessfulAuthentication --------");
		PersonDTO personDTO = new PersonDTO();
		personDTO.setLogin(false);
		String response = new ObjectMapper().writeValueAsString(personDTO);
		res.getWriter().write(response);
	}
	
	public String generateToken(String username, String personId, String unitNumber, String fullName,
			boolean isExternalUser) {
		Claims claims = Jwts.claims().setSubject(username);
		claims.put(Constants.LOGIN_PERSON_ID, personId);
		claims.put(Constants.LOGIN_PERSON_UNIT, unitNumber);
		claims.put(Constants.LOGIN_USER_FULL_NAME, fullName);
		claims.put(Constants.IS_EXTERNAL_USER, isExternalUser);
		Date now = new Date();
		Date expiryDate = new Date(now.getTime() + Constants.EXPIRATION_TIME);
		return Jwts.builder().setClaims(claims).setIssuedAt(now).setExpiration(expiryDate)
				.signWith(getSignKey(), SignatureAlgorithm.HS256).compact();
	} 
	
	private Key getSignKey() {
		byte[] keyBytes = Decoders.BASE64.decode(Constants.SECRET);
		return Keys.hmacShaKeyFor(keyBytes);
	}

}
