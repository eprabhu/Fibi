package com.polus.fibicomp.award.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.award.pojo.AwardAttachment;
import com.polus.fibicomp.award.pojo.AwardAttachmentType;
import com.polus.fibicomp.proposal.pojo.NarrativeStatus;

public class AwardAttachmentsVO {
	
	public AwardAttachment awardAttachment;

	private String acType;
    
	private String message;
	
	private String updateUser;
	
	private List<AwardAttachmentType> awardAttachmentTypes;

	private List<NarrativeStatus> narrativeStatus;

	private List<AwardAttachment> newAttachments;

	private Integer awardId;

	private String awardNumber;

	private Integer awardAttachmentId;

	private String fileDataId;

	private String sortBy;

	private String reverse;

	private Integer documentId;

	private Integer awardSequenceNumber;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String fileTimestamp;

	private String personId;

	private String fileName;

	private String contentType;

	private Boolean isReplaceAttachmentEnabled = Boolean.FALSE;
	
	private String awardLeadUnitNumber;

	public AwardAttachmentsVO() {
		newAttachments = new ArrayList<>();
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<AwardAttachmentType> getAwardAttachmentTypes() {
		return awardAttachmentTypes;
	}

	public void setAwardAttachmentTypes(List<AwardAttachmentType> awardAttachmentTypes) {
		this.awardAttachmentTypes = awardAttachmentTypes;
	}

	public AwardAttachment getAwardAttachment() {
		return awardAttachment;
	}

	public void setAwardAttachment(AwardAttachment awardAttachment) {
		this.awardAttachment = awardAttachment;
	}

	public List<NarrativeStatus> getNarrativeStatus() {
		return narrativeStatus;
	}

	public void setNarrativeStatus(List<NarrativeStatus> narrativeStatus) {
		this.narrativeStatus = narrativeStatus;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public List<AwardAttachment> getNewAttachments() {
		return newAttachments;
	}

	public void setNewAttachments(List<AwardAttachment> newAttachments) {
		this.newAttachments = newAttachments;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getAwardAttachmentId() {
		return awardAttachmentId;
	}

	public void setAwardAttachmentId(Integer awardAttachmentId) {
		this.awardAttachmentId = awardAttachmentId;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
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

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public Integer getAwardSequenceNumber() {
		return awardSequenceNumber;
	}

	public void setAwardSequenceNumber(Integer awardSequenceNumber) {
		this.awardSequenceNumber = awardSequenceNumber;
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

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public Boolean getIsReplaceAttachmentEnabled() {
		return isReplaceAttachmentEnabled;
	}

	public void setIsReplaceAttachmentEnabled(Boolean isReplaceAttachmentEnabled) {
		this.isReplaceAttachmentEnabled = isReplaceAttachmentEnabled;
	}

	public String getAwardLeadUnitNumber() {
		return awardLeadUnitNumber;
	}

	public void setAwardLeadUnitNumber(String awardLeadUnitNumber) {
		this.awardLeadUnitNumber = awardLeadUnitNumber;
	}

}
