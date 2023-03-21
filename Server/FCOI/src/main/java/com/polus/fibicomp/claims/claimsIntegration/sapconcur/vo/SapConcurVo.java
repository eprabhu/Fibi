package com.polus.fibicomp.claims.claimsIntegration.sapconcur.vo;

import java.util.Set;

public class SapConcurVo {
	private Integer sapConcurFileCount;
	private Boolean isResponseMail = false;
	private StringBuilder emailBody;
	private Integer batchId;
	private Integer totalFileRowCount;
	private Integer fileId;
	private Integer fileRowIndex;
	private String fileName;
	private Set<Integer> batchIds;
	private Boolean errorOccured = Boolean.FALSE;

	public Boolean getErrorOccured() {
		return errorOccured;
	}

	public void setErrorOccured(Boolean errorOccured) {
		this.errorOccured = errorOccured;
	}

	public Integer getSapConcurFileCount() {
		return sapConcurFileCount;
	}

	public void setSapConcurFileCount(Integer sapConcurFileCount) {
		this.sapConcurFileCount = sapConcurFileCount;
	}

	public Boolean getIsResponseMail() {
		return isResponseMail;
	}

	public void setIsResponseMail(Boolean isResponseMail) {
		this.isResponseMail = isResponseMail;
	}

	public StringBuilder getEmailBody() {
		return emailBody;
	}

	public void setEmailBody(StringBuilder emailBody) {
		this.emailBody = emailBody;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public Integer getTotalFileRowCount() {
		return totalFileRowCount;
	}

	public void setTotalFileRowCount(Integer totalFileRowCount) {
		this.totalFileRowCount = totalFileRowCount;
	}

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
	}

	public Integer getFileRowIndex() {
		return fileRowIndex;
	}

	public void setFileRowIndex(Integer fileRowIndex) {
		this.fileRowIndex = fileRowIndex;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public Set<Integer> getBatchIds() {
		return batchIds;
	}

	public void setBatchIds(Set<Integer> batchIds) {
		this.batchIds = batchIds;
	}

}
