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
	PersonService personService;
	
	private static final String LOGIN_PERSON_ID = "personId";
	private static final String LOGIN_PERSON_UNIT = "unitNumber";
	private static final String LOGIN_USER_FULL_NAME = "fullName";

	public static final String SECRET = "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAJt6Aa8aOs06KyiBZ3pwiKdfgmdbosVuNPpxO9hUd6gsKk3lxTx+pBpkaONP/WHWkv5g/GvgOt9R+aItSxf3LEcCAwEAAQ==";
	
	private static final long EXPIRATION_TIME = 43_200_000; // 12 hour


    public void validateToken(final String token) {
        Jwts.parserBuilder().setSigningKey(getSignKey()).build().parseClaimsJws(token);
    }


    public String generateToken(String userName) {       
    	Optional<Person> optionalPerson = personService.loadUserByUsername(userName);
    	Person person = optionalPerson.orElseThrow(); 
        Claims claims = Jwts.claims().setSubject(userName);
 	    claims.put(LOGIN_PERSON_ID, person.getPersonId());
 	    claims.put(LOGIN_PERSON_UNIT, person.getHomeUnit());
 	    claims.put(LOGIN_USER_FULL_NAME, person.getFullName());
 	   claims.put("isExternalUser", false);
        return createToken(claims, userName);
    }

    private String createToken(Claims claims, String userName) {
    	Date now = new Date();
		Date expiryDate = new Date(now.getTime() 
				+ EXPIRATION_TIME
				);
        return Jwts.builder()
                .setClaims(claims)                
                .setIssuedAt(now)
                .setExpiration(expiryDate)
                .signWith(getSignKey(), SignatureAlgorithm.HS512).compact();
    }

    private Key getSignKey() {
        byte[] keyBytes = Decoders.BASE64.decode(SECRET);
        return Keys.hmacShaKeyFor(keyBytes);
    }

	public String getUsernameFromToken(String token) {
		if (!Optional.ofNullable(token).isPresent()) {
			return null;
		}
		String username;
		try {
			final Claims claims = this.getAllClaimsFromToken(token);
			username = claims.getSubject();
		} catch (Exception e) {
			username = null;
		}
		return username;
	}

	private Claims getAllClaimsFromToken(String token) {
		Claims claims;
		try {
			claims = Jwts.parser()
					.setSigningKey(SECRET)
					.parseClaimsJws(token)
					.getBody();
		} catch (Exception e) {
			claims = null;
		}
		return claims;
	}

}
