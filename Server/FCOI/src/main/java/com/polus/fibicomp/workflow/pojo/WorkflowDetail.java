package com.polus.fibicomp.workflow.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.person.pojo.PersonRoleType;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "WORKFLOW_DETAIL")
public class WorkflowDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKFLOW_DETAIL_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer workflowDetailId;

	@Column(name = "WORKFLOW_ID")
	private Integer workflowId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_DETAIL_FK1"), name = "WORKFLOW_ID", referencedColumnName = "WORKFLOW_ID", insertable = false, updatable = false)
	private Workflow workflow;

	@Column(name = "MAP_ID")
	private Integer mapId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_DETAIL_FK2"), name = "MAP_ID", referencedColumnName = "MAP_ID", insertable = false, updatable = false)
	private WorkflowMap workflowMap;

	@Column(name = "MAP_NUMBER")
	private Integer mapNumber;

	@Column(name = "APPROVAL_STOP_NUMBER")
	private Integer approvalStopNumber;

	@Column(name = "APPROVER_NUMBER")
	private Integer approverNumber;

	@Column(name = "PRIMARY_APPROVER_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean primaryApproverFlag;

	@Column(name = "APPROVER_PERSON_ID")
	private String approverPersonId;

	@Column(name = "APPROVER_PERSON_NAME")
	private String approverPersonName;

	@Column(name = "APPROVAL_STATUS")
	private String approvalStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_DETAIL_FK3"), name = "APPROVAL_STATUS", referencedColumnName = "APPROVAL_STATUS", insertable = false, updatable = false)
	private WorkflowStatus workflowStatus;

	@Column(name = "APPROVAL_COMMENT")
	private String approvalComment;

	@Column(name = "APPROVAL_DATE")
	private Timestamp approvalDate;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonManagedReference
	@OneToMany(mappedBy = "workflowDetail", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<WorkflowAttachment> workflowAttachments;

	@Column(name = "ROLE_TYPE_CODE")
	private Integer roleTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_DETAIL_FK4"), name = "ROLE_TYPE_CODE", referencedColumnName = "ROLE_TYPE_CODE", insertable = false, updatable = false)
	private PersonRoleType personRoleType;

	@JsonManagedReference
	@OneToMany(mappedBy = "workflowDetail", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<WorkflowReviewerDetail> workflowReviewerDetails;

	@Column(name = "EMAIL_ADDRESS")
	private String emailAddress;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "DELEGATED_BY_PERSON_ID")
	private String delegatedByPersonId;

	@Column(name = "STOP_NAME")
	private String stopName;

	@Column(name = "MAP_NAME")
	private String mapName;

	@Column(name = "MAP_DESCRIPTION")
	private String mapDescription;

	public String getMapDescription() {
		return mapDescription;
	}

	public void setMapDescription(String mapDescription) {
		this.mapDescription = mapDescription;
	}

	public String getStopName() {
		return stopName;
	}

	public void setStopName(String stopName) {
		this.stopName = stopName;
	}

	public String getMapName() {
		return mapName;
	}

	public void setMapName(String mapName) {
		this.mapName = mapName;
	}

	@Transient
	private Boolean isReviewerCanScore = false;


	@Transient
	private String updateUserFullName;

	@Transient
	private WorkflowDetailExt workflowDetailExt;

	@Transient
	private String delegatedPersonName;

	public WorkflowDetail() {
		workflowAttachments = new ArrayList<WorkflowAttachment>();
		workflowReviewerDetails = new ArrayList<WorkflowReviewerDetail>();
	}

	public WorkflowDetail(Integer workflowDetailId, String approverPersonId, String approverPersonName, Boolean isReviewerCanScore, WorkflowMap workflowMap) {
		super();
		this.workflowDetailId = workflowDetailId;
		this.approverPersonId = approverPersonId;
		this.approverPersonName = approverPersonName;
		this.isReviewerCanScore = isReviewerCanScore;
		this.workflowMap = workflowMap;
	}

	public Integer getWorkflowDetailId() {
		return workflowDetailId;
	}

	public void setWorkflowDetailId(Integer workflowDetailId) {
		this.workflowDetailId = workflowDetailId;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public WorkflowMap getWorkflowMap() {
		return workflowMap;
	}

	public void setWorkflowMap(WorkflowMap workflowMap) {
		this.workflowMap = workflowMap;
	}

	public Integer getMapNumber() {
		return mapNumber;
	}

	public void setMapNumber(Integer mapNumber) {
		this.mapNumber = mapNumber;
	}

	public Integer getApprovalStopNumber() {
		return approvalStopNumber;
	}

	public void setApprovalStopNumber(Integer approvalStopNumber) {
		this.approvalStopNumber = approvalStopNumber;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public Boolean getPrimaryApproverFlag() {
		return primaryApproverFlag;
	}

	public void setPrimaryApproverFlag(Boolean primaryApproverFlag) {
		this.primaryApproverFlag = primaryApproverFlag;
	}

	public String getApproverPersonId() {
		return approverPersonId;
	}

	public void setApproverPersonId(String approverPersonId) {
		this.approverPersonId = approverPersonId;
	}

	public WorkflowStatus getWorkflowStatus() {
		return workflowStatus;
	}

	public void setWorkflowStatus(WorkflowStatus workflowStatus) {
		this.workflowStatus = workflowStatus;
	}

	public String getApprovalComment() {
		return approvalComment;
	}

	public void setApprovalComment(String approvalComment) {
		this.approvalComment = approvalComment;
	}

	public Timestamp getApprovalDate() {
		return approvalDate;
	}

	public void setApprovalDate(Timestamp approvalDate) {
		this.approvalDate = approvalDate;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getApprovalStatusCode() {
		return approvalStatusCode;
	}

	public void setApprovalStatusCode(String approvalStatusCode) {
		this.approvalStatusCode = approvalStatusCode;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public List<WorkflowAttachment> getWorkflowAttachments() {
		return workflowAttachments;
	}

	public void setWorkflowAttachments(List<WorkflowAttachment> workflowAttachments) {
		this.workflowAttachments = workflowAttachments;
	}

	public String getApproverPersonName() {
		return approverPersonName;
	}

	public void setApproverPersonName(String approverPersonName) {
		this.approverPersonName = approverPersonName;
	}

	public Integer getRoleTypeCode() {
		return roleTypeCode;
	}

	public void setRoleTypeCode(Integer roleTypeCode) {
		this.roleTypeCode = roleTypeCode;
	}

	public List<WorkflowReviewerDetail> getWorkflowReviewerDetails() {
		return workflowReviewerDetails;
	}

	public void setWorkflowReviewerDetails(List<WorkflowReviewerDetail> workflowReviewerDetails) {
		this.workflowReviewerDetails = workflowReviewerDetails;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public PersonRoleType getPersonRoleType() {
		return personRoleType;
	}

	public void setPersonRoleType(PersonRoleType personRoleType) {
		this.personRoleType = personRoleType;
	}

	public Integer getWorkflowId() {
		return workflowId;
	}

	public void setWorkflowId(Integer workflowId) {
		this.workflowId = workflowId;
	}

	public Boolean getIsReviewerCanScore() {
		return isReviewerCanScore;
	}

	public void setIsReviewerCanScore(Boolean isReviewerCanScore) {
		this.isReviewerCanScore = isReviewerCanScore;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public WorkflowDetailExt getWorkflowDetailExt() {
		return workflowDetailExt;
	}

	public void setWorkflowDetailExt(WorkflowDetailExt workflowDetailExt) {
		this.workflowDetailExt = workflowDetailExt;
	}

	public String getDelegatedByPersonId() {
		return delegatedByPersonId;
	}

	public void setDelegatedByPersonId(String delegatedByPersonId) {
		this.delegatedByPersonId = delegatedByPersonId;
	}

	public String getDelegatedPersonName() {
		return delegatedPersonName;
	}

	public void setDelegatedPersonName(String delegatedPersonName) {
		this.delegatedPersonName = delegatedPersonName;
	}

}
