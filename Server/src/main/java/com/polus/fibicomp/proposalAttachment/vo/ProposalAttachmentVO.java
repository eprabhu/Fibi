package com.polus.fibicomp.proposalAttachment.vo;

public class ProposalAttachmentVO {

	private String fileContent;

	private String fileName;

	private Integer remaining;

	private Integer length;

	private Integer moduleCode;

	private Long moduleItemKey;

	private Long userId;

	private String contentType;

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Long getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(Long moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Long getUserId() {
		return userId;
	}

	public void setUserId(Long userId) {
		this.userId = userId;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}
}
