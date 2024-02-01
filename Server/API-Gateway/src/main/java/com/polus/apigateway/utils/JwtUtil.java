package com.polus.apigateway.utils;

import java.security.Key;

import org.springframework.stereotype.Component;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

@Component
public class JwtUtil {


    public static final String SECRET = "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAJt6Aa8aOs06KyiBZ3pwiKdfgmdbosVuNPpxO9hUd6gsKk3lxTx+pBpkaONP/WHWkv5g/GvgOt9R+aItSxf3LEcCAwEAAQ==";


    public void validateToken(final String token) {
        Jwts.parserBuilder().setSigningKey(getSignKey()).build().parseClaimsJws(token);
    }


    private Key getSignKey() {
        byte[] keyBytes = Decoders.BASE64.decode(SECRET);
        return Keys.hmacShaKeyFor(keyBytes);
    }
}
