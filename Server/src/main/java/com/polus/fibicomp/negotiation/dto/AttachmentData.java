package com.polus.fibicomp.negotiation.dto;

import java.util.List;

public class AttachmentData {
	
	private Integer negotiationAttachmentId;
	private String fileName;
	
	List<AttachmentData> attachmentDataList;

	public Integer getNegotiationAttachmentId() {
		return negotiationAttachmentId;
	}

	public void setNegotiationAttachmentId(Integer negotiationAttachmentId) {
		this.negotiationAttachmentId = negotiationAttachmentId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public List<AttachmentData> getAttachmentDataList() {
		return attachmentDataList;
	}

	public void setAttachmentDataList(List<AttachmentData> attachmentDataList) {
		this.attachmentDataList = attachmentDataList;
	}

}
