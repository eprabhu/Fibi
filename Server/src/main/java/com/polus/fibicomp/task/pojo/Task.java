package com.polus.fibicomp.task.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;

@Entity
@Table(name = "TASK")
@EntityListeners(AuditingEntityListener.class)
public class Task implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TASK_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TASK_ID_GENERATOR")
	@SequenceGenerator(name = "TASK_ID_GENERATOR", sequenceName = "TASK_ID_GENERATOR", allocationSize = 1)
	private Integer taskId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "MODULE_ITEM_ID")
	private String moduleItemId;

	@Column(name = "START_MODULE_SUB_ITEM_KEY")
	private Integer startModuleSubItemKey;

	@Column(name = "END_MODULE_SUB_ITEM_KEY")
	private Integer endModuleSubItemKey;

	@Column(name = "TASK_TYPE_CODE")
	private String taskTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_FK1"), name = "TASK_TYPE_CODE", referencedColumnName = "TASK_TYPE_CODE", insertable = false, updatable = false)
	private TaskType taskType;

	@Column(name = "TASK_STATUS_CODE")
	private String taskStatusCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_FK2"), name = "TASK_STATUS_CODE", referencedColumnName = "TASK_STATUS_CODE", insertable = false, updatable = false)
	private TaskStatus taskStatus;

	@Column(name = "ASSIGNEE_PERSON_ID")
	private String assigneePersonId;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "DUE_DATE")
	private Timestamp dueDate;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "task", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<TaskAttachment> taskAttachments;

	@Transient
	private String assigneeFullName;

	@Transient
	private String lastUpdateUserFullName;

	@Transient
	private String createUserFullName;

	@Transient
	private String workFlowStatusName;

	@Transient
	private String submitUserFullName;

	@Transient
	private String serviceRequestSubject;

	@Transient
	private List<ModuleVariableSection> sectionTypeCodes;

	@Transient
	private String canApproveRouting;

	public Task() {
		taskAttachments = new ArrayList<TaskAttachment>();
	}

	public Integer getTaskId() {
		return taskId;
	}

	public void setTaskId(Integer taskId) {
		this.taskId = taskId;
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

	public String getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(String moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public Integer getStartModuleSubItemKey() {
		return startModuleSubItemKey;
	}

	public void setStartModuleSubItemKey(Integer startModuleSubItemKey) {
		this.startModuleSubItemKey = startModuleSubItemKey;
	}

	public Integer getEndModuleSubItemKey() {
		return endModuleSubItemKey;
	}

	public void setEndModuleSubItemKey(Integer endModuleSubItemKey) {
		this.endModuleSubItemKey = endModuleSubItemKey;
	}

	public String getTaskTypeCode() {
		return taskTypeCode;
	}

	public void setTaskTypeCode(String taskTypeCode) {
		this.taskTypeCode = taskTypeCode;
	}

	public TaskType getTaskType() {
		return taskType;
	}

	public void setTaskType(TaskType taskType) {
		this.taskType = taskType;
	}

	public String getTaskStatusCode() {
		return taskStatusCode;
	}

	public void setTaskStatusCode(String taskStatusCode) {
		this.taskStatusCode = taskStatusCode;
	}

	public TaskStatus getTaskStatus() {
		return taskStatus;
	}

	public void setTaskStatus(TaskStatus taskStatus) {
		this.taskStatus = taskStatus;
	}

	public String getAssigneePersonId() {
		return assigneePersonId;
	}

	public void setAssigneePersonId(String assigneePersonId) {
		this.assigneePersonId = assigneePersonId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Timestamp getDueDate() {
		return dueDate;
	}

	public void setDueDate(Timestamp dueDate) {
		this.dueDate = dueDate;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
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

	public List<TaskAttachment> getTaskAttachments() {
		return taskAttachments;
	}

	public void setTaskAttachments(List<TaskAttachment> taskAttachments) {
		this.taskAttachments = taskAttachments;
	}

	public String getAssigneeFullName() {
		return assigneeFullName;
	}

	public void setAssigneeFullName(String assigneeFullName) {
		this.assigneeFullName = assigneeFullName;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public String getWorkFlowStatusName() {
		return workFlowStatusName;
	}

	public void setWorkFlowStatusName(String workFlowStatusName) {
		this.workFlowStatusName = workFlowStatusName;
	}

	public String getSubmitUserFullName() {
		return submitUserFullName;
	}

	public void setSubmitUserFullName(String submitUserFullName) {
		this.submitUserFullName = submitUserFullName;
	}

	public String getServiceRequestSubject() {
		return serviceRequestSubject;
	}

	public void setServiceRequestSubject(String serviceRequestSubject) {
		this.serviceRequestSubject = serviceRequestSubject;
	}

	public List<ModuleVariableSection> getSectionTypeCodes() {
		return sectionTypeCodes;
	}

	public void setSectionTypeCodes(List<ModuleVariableSection> sectionTypeCodes) {
		this.sectionTypeCodes = sectionTypeCodes;
	}

	public String getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(String canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

}
