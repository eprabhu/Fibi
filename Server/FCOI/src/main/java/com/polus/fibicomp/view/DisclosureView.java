package com.polus.fibicomp.view;

import java.sql.Timestamp;
import java.util.Date;

import com.polus.fibicomp.pojo.Unit;

public class DisclosureView {

	private Integer coiDisclosureId;
	private String documentNumber;
	private String coiDisclosureNumber;
	private Integer sequenceNumber;
	private String personId;
	private String fullName;
	private String dispositionStatusCode;
	private String dispositionStatus;
	private String conflictStatusCode;
	private String conflictStatus;
	private String moduleItemKey;
	private Integer discActiveStatus;
	private Timestamp expirationDate;
	private Timestamp updateTimeStamp;
	private String updateUser;
	private String updateUserFullName;
	private String createUser;
	private String versionStatus;
	private String reviewStatus;
	private Timestamp submittedDate;
	private Integer lastApprovedVersion;
	private Integer noOfSfiInActive;
	private Integer noOfSfiInPending;
	private Integer noOfAwardInPending;
	private Integer noOfProposalInPending;
	private Integer noOfAwardInActive;
	private Integer noOfProposalInActive;
	private Timestamp createTimestamp;
	private Integer disclosureVersionNumber;
	private String disclosurePersonFullName;
	private String fcoiTypeCode;
	private String fcoiType;
	private Timestamp lastApprovedVersionDate;
	private String reviseComment;
	private String reviewStatusCode;
	private Integer reviewId;
	private String reviewDescription;
	private String reviewerStatusCode;
	private String reviewerStatus;
	private String reviewerFullName;
	private String proposalId;
	private String proposalTitle;
	private String awardId;
	private String awardTitle;
	private Integer noOfSfi;
	private Integer noOfAward;
	private Integer noOfProposal;
	private Timestamp certifiedAt;
	private Unit unit;
	private Integer travelDisclosureId;
	private String travelVersionStatus;
	private Date travelStartDate;
	private Date travelEndDate;
	private Date travelSubmissionDate;
	private String lastUpdatedBy;
	private String acknowledgeBy;
	private String travelEntityName;
	private String travellerName;
	private String travelDisclosurestatus;
	private String destination;
	private String purpose;
	private Date certificationDate;
	private Date acknowledgeDate;
	private String travelDisclosureNumber;
	private String description;

	public String getAcknowledgeBy() {
		return acknowledgeBy;
	}

	public void setAcknowledgeBy(String acknowledgeBy) {
		this.acknowledgeBy = acknowledgeBy;
	}

	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public String getTravelVersionStatus() {
		return travelVersionStatus;
	}

	public void setTravelVersionStatus(String travelVersionStatus) {
		this.travelVersionStatus = travelVersionStatus;
	}

	public String getTravelEntityName() {
		return travelEntityName;
	}

	public void setTravelEntityName(String travelEntityName) {
		this.travelEntityName = travelEntityName;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public String getTravellerName() {
		return travellerName;
	}

	public void setTravellerName(String travellerName) {
		this.travellerName = travellerName;
	}

	public String getTravelDisclosurestatus() {
		return travelDisclosurestatus;
	}

	public void setTravelDisclosurestatus(String travelDisclosurestatus) {
		this.travelDisclosurestatus = travelDisclosurestatus;
	}

	public Date getTravelStartDate() {
		return travelStartDate;
	}

	public void setTravelStartDate(Date travelStartDate) {
		this.travelStartDate = travelStartDate;
	}

	public Date getTravelEndDate() {
		return travelEndDate;
	}

	public void setTravelEndDate(Date travelEndDate) {
		this.travelEndDate = travelEndDate;
	}

	public Date getTravelSubmissionDate() {
		return travelSubmissionDate;
	}

	public void setTravelSubmissionDate(Date travelSubmissionDate) {
		this.travelSubmissionDate = travelSubmissionDate;
	}

	public String getLastUpdatedBy() {
		return lastUpdatedBy;
	}

	public void setLastUpdatedBy(String lastUpdatedBy) {
		this.lastUpdatedBy = lastUpdatedBy;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public Timestamp getCertifiedAt() {
		return certifiedAt;
	}

	public void setCertifiedAt(Timestamp certifiedAt) {
		this.certifiedAt = certifiedAt;
	}

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

	public String getDispositionStatusCode() {
		return dispositionStatusCode;
	}

	public void setDispositionStatusCode(String dispositionStatusCode) {
		this.dispositionStatusCode = dispositionStatusCode;
	}

	public String getConflictStatusCode() {
		return conflictStatusCode;
	}

	public void setConflictStatusCode(String conflictStatusCode) {
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

	public String getAwardId() {
		return awardId;
	}

	public void setAwardId(String awardId) {
		this.awardId = awardId;
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

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getDestination() {
		return destination;
	}

	public void setDestination(String destination) {
		this.destination = destination;
	}

	public String getPurpose() {
		return purpose;
	}

	public void setPurpose(String purpose) {
		this.purpose = purpose;
	}

	public Date getCertificationDate() {
		return certificationDate;
	}

	public void setCertificationDate(Date certificationDate) {
		this.certificationDate = certificationDate;
	}

	public Date getAcknowledgeDate() {
		return acknowledgeDate;
	}

	public void setAcknowledgeDate(Date acknowledgeDate) {
		this.acknowledgeDate = acknowledgeDate;
	}

	public String getTravelDisclosureNumber() {
		return travelDisclosureNumber;
	}

	public void setTravelDisclosureNumber(String travelDisclosureNumber) {
		this.travelDisclosureNumber = travelDisclosureNumber;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
	
}
