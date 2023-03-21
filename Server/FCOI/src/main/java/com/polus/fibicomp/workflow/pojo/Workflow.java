package com.polus.fibicomp.workflow.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "WORKFLOW")
public class Workflow implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKFLOW_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer workflowId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "MODULE_ITEM_ID")
	private String moduleItemId;

	@Column(name = "IS_WORKFLOW_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isWorkflowActive;

	@Column(name = "COMMENTS")
	private String comments;

	@JsonManagedReference
	@OneToMany(mappedBy = "workflow", orphanRemoval = true, cascade = { CascadeType.ALL })
	private List<WorkflowDetail> workflowDetails;

	@Column(name = "WORKFLOW_SEQUENCE")
	private Integer workflowSequence;

	@Column(name = "WORKFLOW_START_DATE")
	private Timestamp workflowStartDate;

	@Column(name = "WORKFLOW_END_DATE")
	private Timestamp workflowEndDate;

	@Column(name = "WORKFLOW_START_PERSON")
	private String workflowStartPerson;

	@Column(name = "WORKFLOW_END_PERSON")
	private String workflowEndPerson;
	
	@Column(name ="MAP_TYPE")
	private String mapType;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "SUB_MODULE_ITEM_ID")
	private String subModuleItemId;

	@Transient
	private Map<Integer, List<WorkflowDetail>> workflowDetailMap;

	@Transient
	private String currentStopName;

	@Transient
	private String startPersonName;

	@Transient
	private String endPersonName;

	@Transient
	private String workflowCreatedBy;

	public String getWorkflowCreatedBy() {
		return workflowCreatedBy;
	}

	public void setWorkflowCreatedBy(String workflowCreatedBy) {
		this.workflowCreatedBy = workflowCreatedBy;
	}

	public Workflow() {
		workflowDetails = new ArrayList<WorkflowDetail>();
	}

	public Integer getWorkflowId() {
		return workflowId;
	}

	public void setWorkflowId(Integer workflowId) {
		this.workflowId = workflowId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Boolean getIsWorkflowActive() {
		return isWorkflowActive;
	}

	public void setIsWorkflowActive(Boolean isWorkflowActive) {
		this.isWorkflowActive = isWorkflowActive;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public List<WorkflowDetail> getWorkflowDetails() {
		return workflowDetails;
	}

	public void setWorkflowDetails(List<WorkflowDetail> workflowDetails) {
		this.workflowDetails = workflowDetails;
	}

	public Integer getWorkflowSequence() {
		return workflowSequence;
	}

	public void setWorkflowSequence(Integer workflowSequence) {
		this.workflowSequence = workflowSequence;
	}

	public Timestamp getWorkflowStartDate() {
		return workflowStartDate;
	}

	public void setWorkflowStartDate(Timestamp workflowStartDate) {
		this.workflowStartDate = workflowStartDate;
	}

	public Timestamp getWorkflowEndDate() {
		return workflowEndDate;
	}

	public void setWorkflowEndDate(Timestamp workflowEndDate) {
		this.workflowEndDate = workflowEndDate;
	}

	public String getWorkflowStartPerson() {
		return workflowStartPerson;
	}

	public void setWorkflowStartPerson(String workflowStartPerson) {
		this.workflowStartPerson = workflowStartPerson;
	}

	public String getWorkflowEndPerson() {
		return workflowEndPerson;
	}

	public void setWorkflowEndPerson(String workflowEndPerson) {
		this.workflowEndPerson = workflowEndPerson;
	}

	public Map<Integer, List<WorkflowDetail>> getWorkflowDetailMap() {
		return workflowDetailMap;
	}

	public void setWorkflowDetailMap(Map<Integer, List<WorkflowDetail>> workflowDetailMap) {
		this.workflowDetailMap = workflowDetailMap;
	}

	public String getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(String moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public String getMapType() {
		return mapType;
	}

	public void setMapType(String mapType) {
		this.mapType = mapType;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getSubModuleItemId() {
		return subModuleItemId;
	}

	public void setSubModuleItemId(String subModuleItemId) {
		this.subModuleItemId = subModuleItemId;
	}

	public String getCurrentStopName() {
		return currentStopName;
	}

	public void setCurrentStopName(String currentStopName) {
		this.currentStopName = currentStopName;
	}

	public String getStartPersonName() {
		return startPersonName;
	}

	public void setStartPersonName(String startPersonName) {
		this.startPersonName = startPersonName;
	}

	public String getEndPersonName() {
		return endPersonName;
	}

	public void setEndPersonName(String endPersonName) {
		this.endPersonName = endPersonName;
	}

}
