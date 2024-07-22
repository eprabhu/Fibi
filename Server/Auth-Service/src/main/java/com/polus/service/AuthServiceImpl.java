package com.polus.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.common.constants.Constants;
import com.polus.common.utils.CommonUtils;
import com.polus.dto.AuthRequest;
import com.polus.dto.AuthResponse;
import com.polus.entity.Person;
import com.polus.entity.PersonLoginDetail;
import com.polus.repository.PersonLoginDetailRepository;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@Service
public class AuthServiceImpl implements AuthService {


	private static final Logger LOGGER = LoggerFactory.getLogger(AuthServiceImpl.class);

    @Value("${fibi.token.cookie.name}")
    private String COOKIE_FIBI_ACCESS_TOKEN;

    @Value("${fibi.token.cookie.expiration}")
    private Integer COOKIE_EXPIRATION;

    @Autowired
    private JwtService jwtService;

    @Autowired
    private PersonService personService;

    @Autowired
    private AuthenticationManager authenticationManager;

    @Autowired
    private TwoFactorAuthenticationService tfaService;

    @Autowired
    private PersonLoginDetailRepository personLoginDetailRepository;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private Environment environment;

    @Override
    public ResponseEntity<AuthResponse> checkAuthorization(HttpServletRequest request, HttpServletResponse response) {
        Authentication authentication;
        try {
            AuthRequest authRequest = new ObjectMapper().readValue(request.getInputStream(), AuthRequest.class);
            String cookieToken = getTokenFromCookie(request);
            if (Constants.EXTERNAL_AUTHORIZATION.equals(environment.getProperty("LOGIN_MODE"))) {
                if (cookieToken != null) {
                    try {
                        validateToken(cookieToken);
                        String username = jwtService.getUsernameFromToken(cookieToken);
                        setCookie(response, generateToken(username));
                        return getUserDetails(username);
                    } catch (Exception e) {
                        LOGGER.info("SSO enabled & token expired attempting authorization");
                        return checkSSOAuthorization(request, response);
                    }
                }
                return checkSSOAuthorization(request, response);
            }
            if (authRequest.getUsername() == null && cookieToken != null) {
                validateToken(cookieToken);
                String username = jwtService.getUsernameFromToken(cookieToken);
                setCookie(response, generateToken(username));
                return getUserDetails(username);
            }
            authentication = authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(authRequest.getUsername(), authRequest.getPassword()));
            return isAuthenticated(authentication, authRequest.getUsername(), response);
        } catch (Exception e) {
            LOGGER.error(String.format("Exception in attemptAuthentication: %s", e.getMessage()));
            logout(request, response);
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body((new AuthResponse()));
        }
    }

    private ResponseEntity<AuthResponse> isAuthenticated(Authentication authentication, String userName, HttpServletResponse serverResponse) {
        AuthResponse response = new AuthResponse();
        String token = null;
        if (!authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body((new AuthResponse()));
        }
        Optional<Person> optionalPerson = personService.loadUserByUsername(userName);
        if (optionalPerson == null || !optionalPerson.isPresent() || optionalPerson.isEmpty()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Person person = optionalPerson.get();
        if (!person.getStatus().equals("A")) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body((new AuthResponse()));
        }
        if (person.getIsMfaEnabled() != null && person.getIsMfaEnabled() && person.getSecret() == null) {
            String secret = tfaService.generateNewSecret();
            personService.updateSecret(person.getPersonId(), secret);
            response.setSecretImageUri(tfaService.generateQrCodeImageUri(secret));
        }
        token = generateToken(userName);
        response = getAuthResponseDetails(optionalPerson);

        saveLoginDetails(userName, person);

        setCookie(serverResponse, token);
        LOGGER.info("Token generated successfully.");
        HttpHeaders headers = new HttpHeaders();
        return ResponseEntity.ok().headers(headers).body((response));
    }

    private void saveLoginDetails(String userName, Person person) {
        PersonLoginDetail personLoginDetail = new PersonLoginDetail();
        personLoginDetail.setPersonId(person.getPersonId());
        personLoginDetail.setLoginStatus(Constants.LOGIN_FLAG);
        personLoginDetail.setFullName(person.getFullName());
        personLoginDetail.setUpdateTimestamp(commonUtils.getCurrentTimestamp());
        personLoginDetail.setUpdateUser(userName);
        personLoginDetailRepository.save(personLoginDetail);
    }

    public void setCookie(HttpServletResponse serverResponse, String token) {
        Cookie cookie = new Cookie(COOKIE_FIBI_ACCESS_TOKEN, token);
        cookie.setMaxAge(COOKIE_EXPIRATION); //43200
        cookie.setPath("/");
        cookie.setHttpOnly(true);
        String cookieHeader = String.format(
                "%s=%s; Max-Age=%d; Path=%s; HttpOnly; SameSite=Strict",
                cookie.getName(),
                cookie.getValue(),
                cookie.getMaxAge(),
                cookie.getPath()
        );
        serverResponse.addHeader("Set-Cookie", cookieHeader);
    }

    private String getRemoteUserDetails(String remoteUser) {
        return remoteUser.substring(0, remoteUser.lastIndexOf('@'));
    }

    @Override
    public String generateToken(String username) {
        return jwtService.generateToken(username);
    }

    @Override
    public void validateToken(String token) {
        jwtService.validateToken(token);
    }

    @Override
    public ResponseEntity<AuthResponse> checkSSOAuthorization(HttpServletRequest request, HttpServletResponse serverResponse) {
        String userName = request.getRemoteUser();
        Authentication authentication = null;
        userName = getRemoteUserDetails(userName);
        authentication = new UsernamePasswordAuthenticationToken(userName, null, new ArrayList<>());
        SecurityContextHolder.getContext().setAuthentication(authentication);
        return isAuthenticated(authentication, userName, serverResponse);
    }

    @Override
    public ResponseEntity<Object> verifyCode(AuthRequest vo) {
        Optional<Person> optionalPerson = personService.loadUserByUsername(vo.getUsername());
        AuthResponse response = new AuthResponse();
        Person person = optionalPerson.get();
        if (tfaService.isOtpNotValid(person.getSecret(), vo.getCode())) {
            throw new BadCredentialsException("Code is not correct");
        }
        var jwtToken = generateToken(person.getPrincipalName());
        try {
            response = getAuthResponseDetails(optionalPerson);
            HttpHeaders headers = new HttpHeaders();
            headers.setBearerAuth(jwtToken);
            return ResponseEntity.ok().headers(headers).body((response));
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error processing");
        }
    }

    private String getTokenFromCookie(HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();
        if (cookies == null || cookies.length == 0) {
            return null;
        }
        List<Cookie> cookieList = Arrays.stream(cookies).filter(cookie -> cookie.getName().equals(COOKIE_FIBI_ACCESS_TOKEN)).toList();
        if (!cookieList.isEmpty()) {
            Cookie httpCookie = cookieList.get(0);
            return httpCookie.getValue();
        }
        return null;
    }

    private ResponseEntity<AuthResponse> getUserDetails(String userName) {
        if (Optional.ofNullable(userName).isEmpty()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body((new AuthResponse()));
        }
        Optional<Person> optionalPerson = personService.loadUserByUsername(userName);
        AuthResponse response = getAuthResponseDetails(optionalPerson);
        return ResponseEntity.ok().body((response));
    }

    private static AuthResponse getAuthResponseDetails(Optional<Person> optionalPerson) {
        AuthResponse response = AuthResponse.builder()
                .personId(optionalPerson.map(Person::getPersonId).orElse(null))
                .userName(optionalPerson.map(Person::getPrincipalName).orElse(null))
                .firstName(optionalPerson.map(Person::getFirstName).orElse(null))
                .lastName(optionalPerson.map(Person::getLastName).orElse(null))
                .fullName(optionalPerson.map(Person::getFullName).orElse(null))
                .unitNumber(optionalPerson.map(Person::getHomeUnit).orElse(null))
                .login(Boolean.TRUE)
                .gender(optionalPerson.map(Person::getGender).orElse(null))
//                .userType(optionalPerson.isPresent() ? optionalPerson.get().getUserType() : null)
                .isExternalUser(optionalPerson.map(Person::getIsExternalUser).orElse(null))
                .build();
        return response;
    }

    @Override
    public void logout(HttpServletRequest request, HttpServletResponse response) {
        try {
            String username = jwtService.getUsernameFromToken(getTokenFromCookie(request));
            Optional<Person> optionalPerson = personService.loadUserByUsername(username);
            Person person = optionalPerson.get();
            PersonLoginDetail personLoginDetail = new PersonLoginDetail();
            personLoginDetail.setPersonId(person.getPersonId());
            personLoginDetail.setLoginStatus(Constants.LOGOUT_FLAG);
            personLoginDetail.setFullName(person.getFullName());
            personLoginDetail.setUpdateTimestamp(commonUtils.getCurrentTimestamp());
            personLoginDetail.setUpdateUser(username);
            personLoginDetailRepository.save(personLoginDetail);
        } catch(Exception e) {
            LOGGER.error("Exception logout : {} ", e.getMessage());
        }
        // Find the cookie by name
        Cookie cookie = new Cookie(COOKIE_FIBI_ACCESS_TOKEN, null);
        cookie.setValue(null);
        cookie.setMaxAge(0);
        cookie.setPath("/");
        response.addCookie(cookie);
    }
   
}
