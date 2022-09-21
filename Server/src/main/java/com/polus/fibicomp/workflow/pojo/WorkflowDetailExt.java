package com.polus.fibicomp.workflow.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "WORKFLOW_DETAIL_EXT")
@EntityListeners(AuditingEntityListener.class)
public class WorkflowDetailExt implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKFLOW_DETAIL_EXT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "WORKFLOW_DETAIL_EXT_ID_GENERATOR")
	@SequenceGenerator(name = "WORKFLOW_DETAIL_EXT_ID_GENERATOR", sequenceName = "WORKFLOW_DETAIL_EXT_ID_GENERATOR", allocationSize = 1)
	private Integer workflowDetailExtId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@Column(name = "SUB_MODULE_ITEM_KEY")
	private String subModuleItemKey;

	@Column(name = "WORKFLOW_DETAIL_ID")
	private Integer workflowDetailId;

	@Column(name = "FEEDBACK_TYPE_CODE")
	private String feedbackTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "WORKFLOW_DETAIL_EXT_FK2"), name = "FEEDBACK_TYPE_CODE", referencedColumnName = "FEEDBACK_TYPE_CODE", insertable = false, updatable = false)
	private WorkflowFeedbackType workflowFeedbackType;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public Integer getWorkflowDetailExtId() {
		return workflowDetailExtId;
	}

	public void setWorkflowDetailExtId(Integer workflowDetailExtId) {
		this.workflowDetailExtId = workflowDetailExtId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public WorkflowFeedbackType getWorkflowFeedbackType() {
		return workflowFeedbackType;
	}

	public void setWorkflowFeedbackType(WorkflowFeedbackType workflowFeedbackType) {
		this.workflowFeedbackType = workflowFeedbackType;
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

	public Integer getWorkflowDetailId() {
		return workflowDetailId;
	}

	public void setWorkflowDetailId(Integer workflowDetailId) {
		this.workflowDetailId = workflowDetailId;
	}

	public String getFeedbackTypeCode() {
		return feedbackTypeCode;
	}

	public void setFeedbackTypeCode(String feedbackTypeCode) {
		this.feedbackTypeCode = feedbackTypeCode;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}
}
