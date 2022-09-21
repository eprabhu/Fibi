package com.polus.fibicomp.compilance.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "IRB_PROTOCOL")
public class IrbProtocol implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "IRB_PROTOCOL_ID_GENERATOR")
	@SequenceGenerator(name = "IRB_PROTOCOL_ID_GENERATOR", sequenceName = "IRB_PROTOCOL_ID_GENERATOR", allocationSize = 1)
	@Column(name = "PROTOCOL_ID")
	private Integer protocolId;

	@Column(name = "PROTOCOL_NUMBER")
	private String protocolNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "PROTOCOL_TYPE_CODE")
	private String protocolTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "IRB_PROTOCOL_FK1"), name = "PROTOCOL_TYPE_CODE", referencedColumnName = "PROTOCOL_TYPE_CODE", insertable = false, updatable = false)
	private IrbProtocolType irbProtocolType;

	@Column(name = "PROTOCOL_STATUS_CODE")
	private String protocolStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "IRB_PROTOCOL_FK2"), name = "PROTOCOL_STATUS_CODE", referencedColumnName = "PROTOCOL_STATUS_CODE", insertable = false, updatable = false)
	private IrbProtocolStatus irbProtocolStatus;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "APPROVAL_DATE")
	private Timestamp approvalDate;

	@Column(name = "EXPIRATION_DATE")
	private Timestamp expirationDate;

	@Column(name = "FDA_APPLICATION_NUMBER")
	private String fdaApplicationNumber;

	@Column(name = "REFERENCE_NUMBER_1")
	private String referenceNumberOne;

	@Column(name = "REFERENCE_NUMBER_2")
	private String referenceNumberTwo;

	@Column(name = "INITIAL_SUBMISSION_DATE")
	private Timestamp initialSubmissionDate;

	@Column(name = "ACTIVE")
	private String active;

	@Column(name = "IS_LATEST")
	private String isLatest;

	@Column(name = "ASSIGNEE_PERSON_ID")
	private String personId;

	@Column(name = "ORIGINAL_ASSIGNEE_PERSON_ID")
	private String originalAssigneePersonId;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "LEAD_UNIT_NUMBER")
	private String leadUnitNumber;

	@Column(name = "IS_NEW_RULE")
	private String isNewRule;

	@Column(name = "IS_CANCELLED")
	private String isCancelled;

	@Column(name = "RISK_LEVEL_CODE")
	private String riskLevelCode;

	@Column(name = "RISK_LEVEL_COMMENTS")
	private String riskLevelComments;

	@Column(name = "RISK_LVL_DATE_ASSIGNED")
	private Timestamp riskLevelDateAssigned;

	@Column(name = "FDA_RISK_LEVEL_CODE")
	private String fdaRiskLevelCode;

	@Column(name = "FDA_RISK_LVL_COMMENTS")
	private String fdaRiskLevelComments;

	@Column(name = "FDA_RISK_LVL_DATE_ASSIGNED")
	private Timestamp fdaRiskLevelDateAssigned;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_HOLD")
	private String isHold;

	@Column(name = "WORKFLOW_PROTOCOL_STATUS_CODE")
	private String workflowProtocolStatusCode;

	@Column(name = "DS_LEVEL_ID")
	private Integer dsLevelId;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "NON_EMPLOYEE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean nonEmployeeFlag;

	@Column(name = "FUNDING_SOURCE")
	private String fundingSource;

	@Column(name = "FUNDING_SOURCE_TYPE_CODE")
	private String fundingSourceTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "IRB_PROTOCOL_FK3"), name = "FUNDING_SOURCE_TYPE_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Transient
	private String fullName;

	public Integer getProtocolId() {
		return protocolId;
	}

	public void setProtocolId(Integer protocolId) {
		this.protocolId = protocolId;
	}

	public String getProtocolNumber() {
		return protocolNumber;
	}

	public void setProtocolNumber(String protocolNumber) {
		this.protocolNumber = protocolNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getProtocolTypeCode() {
		return protocolTypeCode;
	}

	public void setProtocolTypeCode(String protocolTypeCode) {
		this.protocolTypeCode = protocolTypeCode;
	}

	public String getProtocolStatusCode() {
		return protocolStatusCode;
	}

	public void setProtocolStatusCode(String protocolStatusCode) {
		this.protocolStatusCode = protocolStatusCode;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Timestamp getApprovalDate() {
		return approvalDate;
	}

	public void setApprovalDate(Timestamp approvalDate) {
		this.approvalDate = approvalDate;
	}

	public Timestamp getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Timestamp expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getFdaApplicationNumber() {
		return fdaApplicationNumber;
	}

	public void setFdaApplicationNumber(String fdaApplicationNumber) {
		this.fdaApplicationNumber = fdaApplicationNumber;
	}

	public String getReferenceNumberOne() {
		return referenceNumberOne;
	}

	public void setReferenceNumberOne(String referenceNumberOne) {
		this.referenceNumberOne = referenceNumberOne;
	}

	public String getReferenceNumberTwo() {
		return referenceNumberTwo;
	}

	public void setReferenceNumberTwo(String referenceNumberTwo) {
		this.referenceNumberTwo = referenceNumberTwo;
	}

	public Timestamp getInitialSubmissionDate() {
		return initialSubmissionDate;
	}

	public void setInitialSubmissionDate(Timestamp initialSubmissionDate) {
		this.initialSubmissionDate = initialSubmissionDate;
	}

	public String getActive() {
		return active;
	}

	public void setActive(String active) {
		this.active = active;
	}

	public String getIsLatest() {
		return isLatest;
	}

	public void setIsLatest(String isLatest) {
		this.isLatest = isLatest;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getOriginalAssigneePersonId() {
		return originalAssigneePersonId;
	}

	public void setOriginalAssigneePersonId(String originalAssigneePersonId) {
		this.originalAssigneePersonId = originalAssigneePersonId;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public String getIsNewRule() {
		return isNewRule;
	}

	public void setIsNewRule(String isNewRule) {
		this.isNewRule = isNewRule;
	}

	public String getIsCancelled() {
		return isCancelled;
	}

	public void setIsCancelled(String isCancelled) {
		this.isCancelled = isCancelled;
	}

	public String getRiskLevelCode() {
		return riskLevelCode;
	}

	public void setRiskLevelCode(String riskLevelCode) {
		this.riskLevelCode = riskLevelCode;
	}

	public String getRiskLevelComments() {
		return riskLevelComments;
	}

	public void setRiskLevelComments(String riskLevelComments) {
		this.riskLevelComments = riskLevelComments;
	}

	public Timestamp getRiskLevelDateAssigned() {
		return riskLevelDateAssigned;
	}

	public void setRiskLevelDateAssigned(Timestamp riskLevelDateAssigned) {
		this.riskLevelDateAssigned = riskLevelDateAssigned;
	}

	public String getFdaRiskLevelCode() {
		return fdaRiskLevelCode;
	}

	public void setFdaRiskLevelCode(String fdaRiskLevelCode) {
		this.fdaRiskLevelCode = fdaRiskLevelCode;
	}

	public String getFdaRiskLevelComments() {
		return fdaRiskLevelComments;
	}

	public void setFdaRiskLevelComments(String fdaRiskLevelComments) {
		this.fdaRiskLevelComments = fdaRiskLevelComments;
	}

	public Timestamp getFdaRiskLevelDateAssigned() {
		return fdaRiskLevelDateAssigned;
	}

	public void setFdaRiskLevelDateAssigned(Timestamp fdaRiskLevelDateAssigned) {
		this.fdaRiskLevelDateAssigned = fdaRiskLevelDateAssigned;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getIsHold() {
		return isHold;
	}

	public void setIsHold(String isHold) {
		this.isHold = isHold;
	}

	public String getWorkflowProtocolStatusCode() {
		return workflowProtocolStatusCode;
	}

	public void setWorkflowProtocolStatusCode(String workflowProtocolStatusCode) {
		this.workflowProtocolStatusCode = workflowProtocolStatusCode;
	}

	public Integer getDsLevelId() {
		return dsLevelId;
	}

	public void setDsLevelId(Integer dsLevelId) {
		this.dsLevelId = dsLevelId;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
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

	public Boolean getNonEmployeeFlag() {
		return nonEmployeeFlag;
	}

	public void setNonEmployeeFlag(Boolean nonEmployeeFlag) {
		this.nonEmployeeFlag = nonEmployeeFlag;
	}

	public String getFundingSource() {
		return fundingSource;
	}

	public void setFundingSource(String fundingSource) {
		this.fundingSource = fundingSource;
	}

	public String getFundingSourceTypeCode() {
		return fundingSourceTypeCode;
	}

	public void setFundingSourceTypeCode(String fundingSourceTypeCode) {
		this.fundingSourceTypeCode = fundingSourceTypeCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public IrbProtocolType getIrbProtocolType() {
		return irbProtocolType;
	}

	public void setIrbProtocolType(IrbProtocolType irbProtocolType) {
		this.irbProtocolType = irbProtocolType;
	}

	public IrbProtocolStatus getIrbProtocolStatus() {
		return irbProtocolStatus;
	}

	public void setIrbProtocolStatus(IrbProtocolStatus irbProtocolStatus) {
		this.irbProtocolStatus = irbProtocolStatus;
	}

}
