package com.polus.fibicomp.manpower.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "MANPOWER_LOG_USER")
@EntityListeners(AuditingEntityListener.class)
public class ManpowerLogUser {

	@Id
	@Column(name = "MANPOWER_LOG_USER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_MNPWR_LG_USER_GNTR")
	@SequenceGenerator(name = "SEQ_MNPWR_LG_USER_GNTR", sequenceName = "SEQ_MNPWR_LG_USER_GNTR", allocationSize = 1)
	private Integer manpowerLogUserId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "LOGIN_PERSON_ID")
	private String LoginPersonId;

	@Column(name = "ACCESS_STATUS")
	private Boolean accessStatus;

	public Integer getManpowerLogUserId() {
		return manpowerLogUserId;
	}

	public void setManpowerLogUserId(Integer manpowerLogUserId) {
		this.manpowerLogUserId = manpowerLogUserId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
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

	public String getLoginPersonId() {
		return LoginPersonId;
	}

	public void setLoginPersonId(String loginPersonId) {
		LoginPersonId = loginPersonId;
	}

	public Boolean getAccessStatus() {
		return accessStatus;
	}

	public void setAccessStatus(Boolean accessStatus) {
		this.accessStatus = accessStatus;
	}
}
