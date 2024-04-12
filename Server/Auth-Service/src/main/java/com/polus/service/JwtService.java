package com.polus.service;

import java.security.Key;
import java.util.Date;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.polus.entity.Person;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

@Component
public class JwtService {

	@Autowired
	PersonService service;
	
	private static final String LOGIN_PERSON_ID = "personId";
	private static final String LOGIN_PERSON_UNIT = "unitNumber";
	private static final String LOGIN_USER_FULL_NAME = "fullName";

	public static final String SECRET = "5367566B59703373367639792F423F4528482B4D6251655468576D5A71347437";
	
	private static final long EXPIRATION_TIME = 43_200_000; // 12 hour


    public void validateToken(final String token) {
        Jwts.parserBuilder().setSigningKey(getSignKey()).build().parseClaimsJws(token);
    }


    public String generateToken(String userName) {       
    	Optional<Person> optionalPerson = service.loadUserByUsername(userName);
    	Person person = optionalPerson.orElseThrow(); 
        Claims claims = Jwts.claims().setSubject(userName);
 	    claims.put(LOGIN_PERSON_ID, person.getPersonId());
 	    claims.put(LOGIN_PERSON_UNIT, person.getHomeUnit());
 	    claims.put(LOGIN_USER_FULL_NAME, person.getFullName());
        return createToken(claims, userName);
    }

    private String createToken(Claims claims, String userName) {
    	
        return Jwts.builder()
                .setClaims(claims)                
                .setIssuedAt(new Date(System.currentTimeMillis()))
                .setExpiration(new Date(System.currentTimeMillis() + EXPIRATION_TIME))
                .signWith(getSignKey(), SignatureAlgorithm.HS256).compact();
    }

    private Key getSignKey() {
        byte[] keyBytes = Decoders.BASE64.decode(SECRET);
        return Keys.hmacShaKeyFor(keyBytes);
    }    
}
