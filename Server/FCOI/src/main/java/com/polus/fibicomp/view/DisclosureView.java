package com.polus.fibicomp.view;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.Immutable;

@Entity
@Immutable
@Table(name = "DISCLOSURE_MV")
public class DisclosureView implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_DISCLOSURE_ID")
	private Integer coiDisclosureId;

	@Column(name = "DOCUMENT_NUMBER")
	private String documentNumber;

	@Column(name = "COI_DISCLOSURE_NUMBER")
	private String coiDisclosureNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "DISCLOSURE_DISPOSITION_CODE")
	private Integer dispositionStatusCode;

	@Column(name = "DISCLOSURE_DISPOSITION")
	private String dispositionStatus;

	@Column(name = "DISCLOSURE_STATUS_CODE")
	private Integer conflictStatusCode;

	@Column(name = "DISCLOSURE_STATUS")
	private String conflictStatus;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "DISC_ACTIVE_STATUS")
	private Integer discActiveStatus;

	@Column(name = "EXPIRATION_DATE")
	private Timestamp expirationDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String versionStatus;

	@Transient
	private String reviewStatus;
	
	@Transient
	private Timestamp submittedDate;

	@Transient
	private Integer lastApprovedVersion;

	@Transient
	private Integer noOfSfiInActive;

	@Transient
	private Integer noOfSfiInPending;

	@Transient
	private Integer noOfAwardInPending;

	@Transient
	private Integer noOfProposalInPending;

	@Transient
	private Integer noOfAwardInActive;

	@Transient
	private Integer noOfProposalInActive;

	@Transient
	private Timestamp createTimestamp;

	@Transient
	private Integer disclosureVersionNumber;

	@Transient
	private String disclosurePersonFullName;

	@Transient
	private String fcoiTypeCode;

	@Transient
	private String fcoiType;

	@Transient
	private Timestamp lastApprovedVersionDate;

	@Transient
	private String reviseComment;

	@Transient
	private String reviewStatusCode;

	@Transient
	private Integer reviewId;

	@Transient
	private String reviewDescription;

	@Transient
	private String reviewerStatusCode;

	@Transient
	private String reviewerStatus;

	@Transient
	private String reviewerFullName;

	@Transient
	private String proposalId;

	@Transient
	private String proposalTitle;
	
	@Transient
	private String awardlId;

	@Transient
	private String awardTitle;
	
	@Transient
	private Integer noOfSfi;

	@Transient
	private Integer noOfAward;

	@Transient
	private Integer noOfProposal;

	public Integer getCoiDisclosureId() {
		return coiDisclosureId;
	}

	public void setCoiDisclosureId(Integer coiDisclosureId) {
		this.coiDisclosureId = coiDisclosureId;
	}

	public String getDocumentNumber() {
		return documentNumber;
	}

	public void setDocumentNumber(String documentNumber) {
		this.documentNumber = documentNumber;
	}

	public String getCoiDisclosureNumber() {
		return coiDisclosureNumber;
	}

	public void setCoiDisclosureNumber(String coiDisclosureNumber) {
		this.coiDisclosureNumber = coiDisclosureNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public Integer getDispositionStatusCode() {
		return dispositionStatusCode;
	}

	public void setDispositionStatusCode(Integer dispositionStatusCode) {
		this.dispositionStatusCode = dispositionStatusCode;
	}

	public Integer getConflictStatusCode() {
		return conflictStatusCode;
	}

	public void setConflictStatusCode(Integer conflictStatusCode) {
		this.conflictStatusCode = conflictStatusCode;
	}

	public String getConflictStatus() {
		return conflictStatus;
	}

	public void setConflictStatus(String conflictStatus) {
		this.conflictStatus = conflictStatus;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getDiscActiveStatus() {
		return discActiveStatus;
	}

	public void setDiscActiveStatus(Integer discActiveStatus) {
		this.discActiveStatus = discActiveStatus;
	}

	public Timestamp getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Timestamp expirationDate) {
		this.expirationDate = expirationDate;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public String getDispositionStatus() {
		return dispositionStatus;
	}

	public void setDispositionStatus(String dispositionStatus) {
		this.dispositionStatus = dispositionStatus;
	}

	public String getReviewStatus() {
		return reviewStatus;
	}

	public void setReviewStatus(String reviewStatus) {
		this.reviewStatus = reviewStatus;
	}

	public Timestamp getSubmittedDate() {
		return submittedDate;
	}

	public void setSubmittedDate(Timestamp submittedDate) {
		this.submittedDate = submittedDate;
	}

	public Integer getLastApprovedVersion() {
		return lastApprovedVersion;
	}

	public void setLastApprovedVersion(Integer lastApprovedVersion) {
		this.lastApprovedVersion = lastApprovedVersion;
	}

	public Integer getNoOfSfiInActive() {
		return noOfSfiInActive;
	}

	public void setNoOfSfiInActive(Integer noOfSfiInActive) {
		this.noOfSfiInActive = noOfSfiInActive;
	}

	public Integer getNoOfSfiInPending() {
		return noOfSfiInPending;
	}

	public void setNoOfSfiInPending(Integer noOfSfiInPending) {
		this.noOfSfiInPending = noOfSfiInPending;
	}

	public Integer getNoOfAwardInPending() {
		return noOfAwardInPending;
	}

	public void setNoOfAwardInPending(Integer noOfAwardInPending) {
		this.noOfAwardInPending = noOfAwardInPending;
	}

	public Integer getNoOfProposalInPending() {
		return noOfProposalInPending;
	}

	public void setNoOfProposalInPending(Integer noOfProposalInPending) {
		this.noOfProposalInPending = noOfProposalInPending;
	}

	public Integer getNoOfAwardInActive() {
		return noOfAwardInActive;
	}

	public void setNoOfAwardInActive(Integer noOfAwardInActive) {
		this.noOfAwardInActive = noOfAwardInActive;
	}

	public Integer getNoOfProposalInActive() {
		return noOfProposalInActive;
	}

	public void setNoOfProposalInActive(Integer noOfProposalInActive) {
		this.noOfProposalInActive = noOfProposalInActive;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Integer getDisclosureVersionNumber() {
		return disclosureVersionNumber;
	}

	public void setDisclosureVersionNumber(Integer disclosureVersionNumber) {
		this.disclosureVersionNumber = disclosureVersionNumber;
	}

	public String getDisclosurePersonFullName() {
		return disclosurePersonFullName;
	}

	public void setDisclosurePersonFullName(String disclosurePersonFullName) {
		this.disclosurePersonFullName = disclosurePersonFullName;
	}

	public String getFcoiTypeCode() {
		return fcoiTypeCode;
	}

	public void setFcoiTypeCode(String fcoiTypeCode) {
		this.fcoiTypeCode = fcoiTypeCode;
	}

	public String getFcoiType() {
		return fcoiType;
	}

	public void setFcoiType(String fcoiType) {
		this.fcoiType = fcoiType;
	}

	public Timestamp getLastApprovedVersionDate() {
		return lastApprovedVersionDate;
	}

	public void setLastApprovedVersionDate(Timestamp lastApprovedVersionDate) {
		this.lastApprovedVersionDate = lastApprovedVersionDate;
	}

	public String getReviseComment() {
		return reviseComment;
	}

	public void setReviseComment(String reviseComment) {
		this.reviseComment = reviseComment;
	}

	public String getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(String reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public Integer getReviewId() {
		return reviewId;
	}

	public void setReviewId(Integer reviewId) {
		this.reviewId = reviewId;
	}

	public String getReviewDescription() {
		return reviewDescription;
	}

	public void setReviewDescription(String reviewDescription) {
		this.reviewDescription = reviewDescription;
	}

	public String getReviewerStatusCode() {
		return reviewerStatusCode;
	}

	public void setReviewerStatusCode(String reviewerStatusCode) {
		this.reviewerStatusCode = reviewerStatusCode;
	}

	public String getReviewerStatus() {
		return reviewerStatus;
	}

	public void setReviewerStatus(String reviewerStatus) {
		this.reviewerStatus = reviewerStatus;
	}

	public String getReviewerFullName() {
		return reviewerFullName;
	}

	public void setReviewerFullName(String reviewerFullName) {
		this.reviewerFullName = reviewerFullName;
	}

	public String getProposalId() {
		return proposalId;
	}

	public void setProposalId(String proposalId) {
		this.proposalId = proposalId;
	}

	public String getProposalTitle() {
		return proposalTitle;
	}

	public void setProposalTitle(String proposalTitle) {
		this.proposalTitle = proposalTitle;
	}

	public String getAwardlId() {
		return awardlId;
	}

	public void setAwardlId(String awardlId) {
		this.awardlId = awardlId;
	}

	public String getAwardTitle() {
		return awardTitle;
	}

	public void setAwardTitle(String awardTitle) {
		this.awardTitle = awardTitle;
	}

	public Integer getNoOfSfi() {
		return noOfSfi;
	}

	public void setNoOfSfi(Integer noOfSfi) {
		this.noOfSfi = noOfSfi;
	}

	public Integer getNoOfAward() {
		return noOfAward;
	}

	public void setNoOfAward(Integer noOfAward) {
		this.noOfAward = noOfAward;
	}

	public Integer getNoOfProposal() {
		return noOfProposal;
	}

	public void setNoOfProposal(Integer noOfProposal) {
		this.noOfProposal = noOfProposal;
	}

}
