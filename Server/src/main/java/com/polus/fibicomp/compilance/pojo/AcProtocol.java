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
@Table(name = "AC_PROTOCOL")
public class AcProtocol implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PROTOCOL_ID_GENERATOR")
	@SequenceGenerator(name = "PROTOCOL_ID_GENERATOR", sequenceName = "PROTOCOL_ID_GENERATOR", allocationSize = 1)
	@Column(name = "PROTOCOL_ID")
	private Integer protocolId;

	@Column(name = "PROTOCOL_NUMBER")
	private String protocolNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "PROTOCOL_TYPE_CODE")
	private String protocolTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AC_PROTOCOL_FK1"), name = "PROTOCOL_TYPE_CODE", referencedColumnName = "PROTOCOL_TYPE_CODE", insertable = false, updatable = false)
	private AcProtocolType acProtocolType;

	@Column(name = "PROTOCOL_STATUS_CODE")
	private String protocolStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AC_PROTOCOL_FK2"), name = "PROTOCOL_STATUS_CODE", referencedColumnName = "PROTOCOL_STATUS_CODE", insertable = false, updatable = false)
	private AcProtocolStatus acProtocolStatus;

	@Column(name = "FUNDING_SOURCE")
	private String fundingSource;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "PURPOSE_OF_STUDY")
	private String purposeOfStudy;

	@Column(name = "LEAD_UNIT_NUMBER")
	private String leadUnitNumber;

	@Column(name = "APPROVAL_DATE")
	private Timestamp approvalDate;

	@Column(name = "EXPIRATION_DATE")
	private Timestamp expirationDate;

	@Column(name = "LAST_APPROVAL_DATE")
	private Timestamp lastApprovalDate;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "INITIAL_SUBMISSION_DATE")
	private Timestamp initialSubmissionDate;

	@Column(name = "FDA_APPLICATION_NUMBER")
	private String fdaApplicationNumber;

	@Column(name = "REFERENCE_NUMBER_1")
	private String referenceNumberOne;

	@Column(name = "REFERENCE_NUMBER_2")
	private String referenceNumberTwo;

	@Column(name = "LAY_STATEMENT_1")
	private String layStatementOne;

	@Column(name = "LAY_STATEMENT_2")
	private String layStatementTwo;

	@Column(name = "ACTIVE")
	private String active;

	@Column(name = "ASSIGNEE_PERSON_ID")
	private String personId;

	@Column(name = "IS_CANCELLED")
	private String isCancelled;

	@Column(name = "IS_HOLD")
	private String isHold;

	@Column(name = "PROJECT_TYPE_CODE")
	private Integer projectTypeCode;

	@Column(name = "DS_LEVEL_ID")
	private Integer dsLevelId;

	@Column(name = "RENEWAL_DUE_DATE")
	private Timestamp renewalDueDate;

	@Column(name = "YEAR")
	private Integer year;

	@Column(name = "SUBMISSION_DUE_DATE")
	private Timestamp submissionDueDate;

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

	@Column(name = "FUNDING_SOURCE_TYPE_CODE")
	private String fundingSourceTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AC_PROTOCOL_FK3"), name = "FUNDING_SOURCE_TYPE_CODE", insertable = false, updatable = false)
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

	public String getFundingSource() {
		return fundingSource;
	}

	public void setFundingSource(String fundingSource) {
		this.fundingSource = fundingSource;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getPurposeOfStudy() {
		return purposeOfStudy;
	}

	public void setPurposeOfStudy(String purposeOfStudy) {
		this.purposeOfStudy = purposeOfStudy;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
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

	public Timestamp getLastApprovalDate() {
		return lastApprovalDate;
	}

	public void setLastApprovalDate(Timestamp lastApprovalDate) {
		this.lastApprovalDate = lastApprovalDate;
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

	public Timestamp getInitialSubmissionDate() {
		return initialSubmissionDate;
	}

	public void setInitialSubmissionDate(Timestamp initialSubmissionDate) {
		this.initialSubmissionDate = initialSubmissionDate;
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

	public String getLayStatementOne() {
		return layStatementOne;
	}

	public void setLayStatementOne(String layStatementOne) {
		this.layStatementOne = layStatementOne;
	}

	public String getLayStatementTwo() {
		return layStatementTwo;
	}

	public void setLayStatementTwo(String layStatementTwo) {
		this.layStatementTwo = layStatementTwo;
	}

	public String getActive() {
		return active;
	}

	public void setActive(String active) {
		this.active = active;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getIsCancelled() {
		return isCancelled;
	}

	public void setIsCancelled(String isCancelled) {
		this.isCancelled = isCancelled;
	}

	public String getIsHold() {
		return isHold;
	}

	public void setIsHold(String isHold) {
		this.isHold = isHold;
	}

	public Integer getProjectTypeCode() {
		return projectTypeCode;
	}

	public void setProjectTypeCode(Integer projectTypeCode) {
		this.projectTypeCode = projectTypeCode;
	}

	public Integer getDsLevelId() {
		return dsLevelId;
	}

	public void setDsLevelId(Integer dsLevelId) {
		this.dsLevelId = dsLevelId;
	}

	public Timestamp getRenewalDueDate() {
		return renewalDueDate;
	}

	public void setRenewalDueDate(Timestamp renewalDueDate) {
		this.renewalDueDate = renewalDueDate;
	}

	public Integer getYear() {
		return year;
	}

	public void setYear(Integer year) {
		this.year = year;
	}

	public Timestamp getSubmissionDueDate() {
		return submissionDueDate;
	}

	public void setSubmissionDueDate(Timestamp submissionDueDate) {
		this.submissionDueDate = submissionDueDate;
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

	public AcProtocolType getAcProtocolType() {
		return acProtocolType;
	}

	public void setAcProtocolType(AcProtocolType acProtocolType) {
		this.acProtocolType = acProtocolType;
	}

	public AcProtocolStatus getAcProtocolStatus() {
		return acProtocolStatus;
	}

	public void setAcProtocolStatus(AcProtocolStatus acProtocolStatus) {
		this.acProtocolStatus = acProtocolStatus;
	}

}
