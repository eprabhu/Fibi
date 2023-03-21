package com.polus.fibicomp.claims.claimsIntegration.excelity.vo;

import java.util.Set;

import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;

public class ExcelityReportVo {

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
	
	public ExcelityReportVo() {
		this.emailContent = new EmailContent();
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

	public EmailContent getEmailContent() {
		return emailContent;
	}

	public void setEmailContent(EmailContent emailContent) {
		this.emailContent = emailContent;
	}

}
