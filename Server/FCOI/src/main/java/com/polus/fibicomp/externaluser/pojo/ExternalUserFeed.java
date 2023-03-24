package com.polus.fibicomp.externaluser.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "EXTERNAL_USER_FEED")
public class ExternalUserFeed implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "EXTERNAL_USER_FEED_ID")
	private Integer externalUserFeedId;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@Column(name = "ACTION")
	private String action;
	
	@Column(name = "USER_NAME")
	private String userName;
	
	@Column(name = "FULL_NAME")
	private String fullName;
	
	@Column(name = "PASSWORD")
	private String password;
	
	@Column(name = "EMAIL_ADDRESS")
	private String emailAddress;
	
	@Column(name = "ORGANIZATION_NAME")
	private String organizationName;
	
	@Column(name = "TYPE")
	private String type;
	
	@Column(name = "IS_SENT")
	private String isSent;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getExternalUserFeedId() {
		return externalUserFeedId;
	}

	public void setExternalUserFeedId(Integer externalUserFeedId) {
		this.externalUserFeedId = externalUserFeedId;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getIsSent() {
		return isSent;
	}

	public void setIsSent(String isSent) {
		this.isSent = isSent;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public String getOrganizationName() {
		return organizationName;
	}

	public void setOrganizationName(String organizationName) {
		this.organizationName = organizationName;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

}
