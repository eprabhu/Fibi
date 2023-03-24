package com.polus.fibicomp.externaluser.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "EXTERNAL_USER")
public class ExternalUser implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "PERSON_ID")
	private Integer personId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "USER_NAME", unique = true)
	private String userName;

	@Column(name = "EMAIL_ADDRESS")
	private String emailAddress;

	@Column(name = "FUNDING_OFFICE")
	private String fundingOffice;

	@Column(name = "PASSWORD")
	private String password;

	@Column(name = "ORGANIZATION_ID")
	private String organizationId;

	@Column(name = "VERIFIED_FLAG")
	private String verifiedFlag;

	@Column(name = "CREATED_TIMESTAMP")
	private Timestamp createdTimestamp;

	@Column(name = "UPDATED_TIMESTAMP")
	private Timestamp updatedTimestamp;

	@Column(name = "VERIFIED_AT")
	private Timestamp verifiedAt;

	@Column(name = "VERIFIED_BY")
	private String verifiedBy;
	
	@Column(name = "ADMIN_COMMENT")
	private String adminComment;
	
	@Column(name = "IS_SGAF_USER")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSGAFUser = false;
	
	@Column(name = "IS_MAIL_SENT")
	private String isMailSent;

	@Transient
	private String fundingOfficeName;
	
	@Transient
	private String orgnanizationName;
	
	public String getIsMailSent() {
		return isMailSent;
	}

	public void setIsMailSent(String isMailSent) {
		this.isMailSent = isMailSent;
	}

	public String getFundingOfficeName() {
		return fundingOfficeName;
	}

	public void setFundingOfficeName(String fundingOfficeName) {
		this.fundingOfficeName = fundingOfficeName;
	}

	public String getOrgnanizationName() {
		return orgnanizationName;
	}

	public void setOrgnanizationName(String orgnanizationName) {
		this.orgnanizationName = orgnanizationName;
	}

	public String getAdminComment() {
		return adminComment;
	}

	public void setAdminComment(String adminComment) {
		this.adminComment = adminComment;
	}

	public Boolean getIsSGAFUser() {
		return isSGAFUser;
	}

	public void setIsSGAFUser(Boolean isSGAFUser) {
		this.isSGAFUser = isSGAFUser;
	}

	public String getVerifiedFlag() {
		return verifiedFlag;
	}

	public void setVerifiedFlag(String verifiedFlag) {
		this.verifiedFlag = verifiedFlag;
	}

	public Integer getPersonId() {
		return personId;
	}

	public void setPersonId(Integer personId) {
		this.personId = personId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public String getFundingOffice() {
		return fundingOffice;
	}

	public void setFundingOffice(String fundingOffice) {
		this.fundingOffice = fundingOffice;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getOrganizationId() {
		return organizationId;
	}

	public void setOrganizationId(String organizationId) {
		this.organizationId = organizationId;
	}

	public Timestamp getCreatedTimestamp() {
		return createdTimestamp;
	}

	public void setCreatedTimestamp(Timestamp createdTimestamp) {
		this.createdTimestamp = createdTimestamp;
	}

	public Timestamp getUpdatedTimestamp() {
		return updatedTimestamp;
	}

	public void setUpdatedTimestamp(Timestamp updatedTimestamp) {
		this.updatedTimestamp = updatedTimestamp;
	}

	public Timestamp getVerifiedAt() {
		return verifiedAt;
	}

	public void setVerifiedAt(Timestamp verifiedAt) {
		this.verifiedAt = verifiedAt;
	}

	public String getVerifiedBy() {
		return verifiedBy;
	}

	public void setVerifiedBy(String verifiedBy) {
		this.verifiedBy = verifiedBy;
	}
	

}
