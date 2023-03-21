package com.polus.fibicomp.login.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.Unit;

@Entity
@Table(name = "PERSON_LOGIN_DETAILS")
public class PersonLoginDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "LOGIN_DETAIL_ID_GENERATOR")
	@SequenceGenerator(name = "LOGIN_DETAIL_ID_GENERATOR", sequenceName = "LOGIN_DETAIL_ID_GENERATOR", allocationSize = 1)
	@Column(name = "LOGIN_DETAIL_ID")
	private Integer loginDetailId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "LOGIN_STATUS")
	private String loginStatus;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String userFullName;

	@Transient
	private Unit unit;
	
	public Integer getLoginDetailId() {
		return loginDetailId;
	}

	public void setLoginDetailId(Integer loginDetailId) {
		this.loginDetailId = loginDetailId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getLoginStatus() {
		return loginStatus;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public void setLoginStatus(String loginStatus) {
		this.loginStatus = loginStatus;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getUserFullName() {
		return userFullName;
	}

	public void setUserFullName(String userFullName) {
		this.userFullName = userFullName;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

}
