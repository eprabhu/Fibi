package com.polus.fibicomp.pojo;

import java.math.BigDecimal;
import java.util.Collection;

import org.springframework.security.core.GrantedAuthority;

/**
 * Person DTO with basic details about a person
 */
public class PersonDTO {

	private String personID = "";

	private String firstName = "";

	private String lastName = "";

	private String fullName = "";

	private String email = "";

	private BigDecimal roleNumber = null;

	private String userName = "";

	private String unitNumber = "";

	private boolean isLogin = false;

	private Collection<? extends GrantedAuthority> jwtRoles;

	private boolean isUnitAdmin = false;

	private boolean isSuperUser = false;
	
	private boolean isExternalUser = false;

	public boolean isExternalUser() {
		return isExternalUser;
	}

	public void setExternalUser(boolean isExternalUser) {
		this.isExternalUser = isExternalUser;
	}

	public boolean isLogin() {
		return isLogin;
	}

	public void setLogin(boolean isLogin) {
		this.isLogin = isLogin;
	}

	public String getPersonID() {
		return personID;
	}

	public void setPersonID(String personID) {
		this.personID = personID;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public BigDecimal getRoleNumber() {
		return roleNumber;
	}

	public void setRoleNumber(BigDecimal roleNumber) {
		this.roleNumber = roleNumber;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Collection<? extends GrantedAuthority> getJwtRoles() {
		return jwtRoles;
	}

	public void setJwtRoles(Collection<? extends GrantedAuthority> jwtRoles) {
		this.jwtRoles = jwtRoles;
	}

	public boolean isUnitAdmin() {
		return isUnitAdmin;
	}

	public void setUnitAdmin(boolean isUnitAdmin) {
		this.isUnitAdmin = isUnitAdmin;
	}

	public boolean isSuperUser() {
		return isSuperUser;
	}

	public void setSuperUser(boolean isSuperUser) {
		this.isSuperUser = isSuperUser;
	}

}
