package com.polus.fibicomp.task.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
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

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "TASK_ACTION_LOG")
public class TaskActionLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ACTION_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TASK_ACTION_LOG_ID_GENERATOR")
	@SequenceGenerator(name = "TASK_ACTION_LOG_ID_GENERATOR", sequenceName = "TASK_ACTION_LOG_ID_GENERATOR", allocationSize = 1)
	private Integer actionLogId;

	@Column(name = "TASK_ID")
	private Integer taskId;

	@JsonIgnore
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_ACTION_LOG_FK1"), name = "TASK_ID", referencedColumnName = "TASK_ID", insertable = false, updatable = false)
	private Task task;

	@Column(name = "ACTION_TYPE_CODE")
	private String actionTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_ACTION_LOG_FK2"), name = "ACTION_TYPE_CODE", referencedColumnName = "ACTION_TYPE_CODE", insertable = false, updatable = false)
	private TaskActionType taskActionType;

	@Column(name = "SYSTEM_COMMENT")
	private String systemComment;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String updateUserFullName;

	@Transient
	private String taskTypeDescription;

	@Transient
	private String newAssigneeFullName;

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public Integer getTaskId() {
		return taskId;
	}

	public void setTaskId(Integer taskId) {
		this.taskId = taskId;
	}

	public Task getTask() {
		return task;
	}

	public void setTask(Task task) {
		this.task = task;
	}

	public String getActionTypeCode() {
		return actionTypeCode;
	}

	public void setActionTypeCode(String actionTypeCode) {
		this.actionTypeCode = actionTypeCode;
	}

	public TaskActionType getTaskActionType() {
		return taskActionType;
	}

	public void setTaskActionType(TaskActionType taskActionType) {
		this.taskActionType = taskActionType;
	}

	public String getSystemComment() {
		return systemComment;
	}

	public void setSystemComment(String systemComment) {
		this.systemComment = systemComment;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getTaskTypeDescription() {
		return taskTypeDescription;
	}

	public void setTaskTypeDescription(String taskTypeDescription) {
		this.taskTypeDescription = taskTypeDescription;
	}

	public String getNewAssigneeFullName() {
		return newAssigneeFullName;
	}

	public void setNewAssigneeFullName(String newAssigneeFullName) {
		this.newAssigneeFullName = newAssigneeFullName;
	}

}
