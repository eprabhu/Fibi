package com.polus.fibicomp.externaluser.vo;

import java.sql.Timestamp;

import com.polus.fibicomp.claims.claimsIntegration.excelity.vo.ExcelityReportVo.EmailContent;

public class HomeVo {

	private Integer personId;
	
	private String fullName;

	private String userName;

	private String emailAddress;

	private String fundingOffice;

	private String middleName;

	private String password;

	private String organizationName;

	private String verifiedFlag;

	private Timestamp createdTimestamp;

	private Timestamp updatedTimestamp;

	private Timestamp verifiedAt;

	private String verifiedBy;
	
	private String reverse;
	
	private String sortBy;
	
	private Boolean isSGAFUser;
	
	private String searchString;
	
	private String adminComments;
	
	private StringBuilder emailBody;
	
	private Boolean errorOccured = Boolean.FALSE;
	
	private EmailContent emailContent;
	
	public class EmailContent {

		public EmailContent() {
			this.error = new StringBuilder();
			this.success = new StringBuilder();
		}

		private StringBuilder error;

		private StringBuilder success;

		public StringBuilder getError() {
			return error;
		}

		public void setError(StringBuilder error) {
			this.error = error;
		}

		public StringBuilder getSuccess() {
			return success;
		}

		public void setSuccess(StringBuilder success) {
			this.success = success;
		}
	}

	public HomeVo() {
		this.emailContent = new EmailContent();
	}

	public StringBuilder getEmailBody() {
		return emailBody;
	}

	public void setEmailBody(StringBuilder emailBody) {
		this.emailBody = emailBody;
	}

	public Boolean getErrorOccured() {
		return errorOccured;
	}

	public void setErrorOccured(Boolean errorOccured) {
		this.errorOccured = errorOccured;
	}

	public EmailContent getEmailContent() {
		return emailContent;
	}

	public void setEmailContent(EmailContent emailContent) {
		this.emailContent = emailContent;
	}

	public String getAdminComments() {
		return adminComments;
	}

	public void setAdminComments(String adminComments) {
		this.adminComments = adminComments;
	}

	public String getSearchString() {
		return searchString;
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
	}

	public String getVerifiedFlag() {
		return verifiedFlag;
	}

	public void setVerifiedFlag(String verifiedFlag) {
		this.verifiedFlag = verifiedFlag;
	}

	public Boolean getIsSGAFUser() {
		return isSGAFUser;
	}

	public void setIsSGAFUser(Boolean isSGAFUser) {
		this.isSGAFUser = isSGAFUser;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
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

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getOrganizationName() {
		return organizationName;
	}

	public void setOrganizationName(String organizationName) {
		this.organizationName = organizationName;
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
