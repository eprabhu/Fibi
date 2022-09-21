package com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.vo;

import java.util.Set;

public class BaseSalaryVo {

	private Integer manpowerSftpFileCount;
	private Boolean isResponseMail = false;
	private StringBuilder emailBody;
	private Integer batchId;
	private Integer totalFileRowCount;
	private Integer fileId;
	private Integer fileRowIndex;
	private String fileName;
	private Set<Integer> batchIds;
	private String passphrase;
	private String keyFile;
	private String inputFile;
	private String outputFile;
	private String personId;
	private Boolean errorOccured = Boolean.FALSE;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Boolean getErrorOccured() {
		return errorOccured;
	}

	public void setErrorOccured(Boolean errorOccured) {
		this.errorOccured = errorOccured;
	}

	public String getPassphrase() {
		return passphrase;
	}

	public void setPassphrase(String passphrase) {
		this.passphrase = passphrase;
	}

	public String getKeyFile() {
		return keyFile;
	}

	public void setKeyFile(String keyFile) {
		this.keyFile = keyFile;
	}

	public String getInputFile() {
		return inputFile;
	}

	public void setInputFile(String inputFile) {
		this.inputFile = inputFile;
	}

	public String getOutputFile() {
		return outputFile;
	}

	public void setOutputFile(String outputFile) {
		this.outputFile = outputFile;
	}

	public StringBuilder getEmailBody() {
		return emailBody;
	}

	public void setEmailBody(StringBuilder emailBody) {
		this.emailBody = emailBody;
	}

	public Integer getManpowerSftpFileCount() {
		return manpowerSftpFileCount;
	}

	public void setManpowerSftpFileCount(Integer manpowerSftpFileCount) {
		this.manpowerSftpFileCount = manpowerSftpFileCount;
	}

	public Boolean getIsResponseMail() {
		return isResponseMail;
	}

	public void setIsResponseMail(Boolean isResponseMail) {
		this.isResponseMail = isResponseMail;
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
