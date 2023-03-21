package com.polus.fibicomp.security;

import javax.servlet.http.HttpServletRequest;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.polus.fibicomp.constants.Constants;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
/*
 * This class is used for fetch logged in User Details from the Request Object
 * This method is not Thread safe*/
public final class AuthenticatedUser {

	protected static Logger logger = LogManager.getLogger(AuthenticatedUser.class.getName());

	private AuthenticatedUser() {
	    throw new IllegalStateException("AuthenticatedUser Class Exception");
	  }

	/*
	 * used for fetch Claims Object*/
	public static Claims getLoginPersonDetailFromJWT() {
		return Jwts.parser().setSigningKey(Constants.SECRET).parseClaimsJws(getJwtFromRequest()).getBody();
	}

	private static String getJwtFromRequest() {
		HttpServletRequest request = ((ServletRequestAttributes)RequestContextHolder.getRequestAttributes()).getRequest();
        String bearerToken = request.getHeader(Constants.HEADER_STRING);
        if (org.springframework.util.StringUtils.hasText(bearerToken) && bearerToken.startsWith(Constants.TOKEN_PREFIX)) {
            return bearerToken.substring(7, bearerToken.length());
        }
        return null;
	}

	/*
	 * used for fetch Logged In User Full Name */
	public static String getLoginUserFullName() {
		try {
			return getLoginPersonDetailFromJWT().get(Constants.LOGIN_USER_FULL_NAME).toString();
		} catch (Exception e) {
			logger.error("Exception while fetching user full name from token {}", e.getMessage());
			return null;
		}
	}

	/*
	 * used for fetch Logged In UserName */
	public static String getLoginUserName() {
		try {
			return getLoginPersonDetailFromJWT().getSubject();
		} catch (Exception e) {
			logger.error("Exception while fetching userName from token {}", e.getMessage());
			return null;
		}
	}

	/*
	 * used for fetch Logged In User Person Id */
	public static String getLoginPersonId() {
		try {
			return getLoginPersonDetailFromJWT().get(Constants.LOGIN_PERSON_ID).toString();
		} catch (Exception e) {
			logger.error("Exception while fetching personId from token {}", e.getMessage());
			return null;
		}
	}

	/*
	 * used for fetch Logged In User Person Unit*/
	public static String getLoginPersonUnit() {
		try {
			return getLoginPersonDetailFromJWT().get(Constants.LOGIN_PERSON_UNIT).toString();
		} catch (Exception e) {
			logger.error("Exception while fetching person unit from token {}", e.getMessage());
			return null;
		}
	}

	/*
	 * used for fetch person is external user*/
	public static Boolean getLoginPersonIsExternal() {
		try {
			return getLoginPersonDetailFromJWT().get(Constants.IS_EXTERNAL_USER) != null ? (Boolean) getLoginPersonDetailFromJWT().get(Constants.IS_EXTERNAL_USER) : Boolean.FALSE ;
		} catch (Exception e) {
			logger.error("Exception while fetching person is external user {}", e.getMessage());
			return Boolean.FALSE;
		}
	}

}
