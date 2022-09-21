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

@Entity
@Table(name = "TASK_STATUS_HISTORY")
public class TaskStatusHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "STATUS_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TASK_STATUS_HISTORY_ID_GENERATOR")
	@SequenceGenerator(name="TASK_STATUS_HISTORY_ID_GENERATOR", sequenceName = "TASK_STATUS_HISTORY_ID_GENERATOR", allocationSize=1)
	private Integer statusHistoryId;

	@Column(name = "TASK_STATUS_CODE")
	private String taskStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_STATUS_HISTORY_FK3"), name = "TASK_STATUS_CODE", referencedColumnName = "TASK_STATUS_CODE", insertable = false, updatable = false)
	private TaskStatus taskStatus;

	@Column(name = "TASK_ID")
	private Integer taskId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_STATUS_HISTORY_FK1"), name = "TASK_ID", referencedColumnName = "TASK_ID", insertable = false, updatable = false)
	private Task task;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_STATUS_HISTORY_FK2"), name = "ACTION_LOG_ID", referencedColumnName = "ACTION_LOG_ID", insertable = false, updatable = false)
	private TaskActionLog taskActionLog;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getStatusHistoryId() {
		return statusHistoryId;
	}

	public void setStatusHistoryId(Integer statusHistoryId) {
		this.statusHistoryId = statusHistoryId;
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

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public TaskActionLog getTaskActionLog() {
		return taskActionLog;
	}

	public void setTaskActionLog(TaskActionLog taskActionLog) {
		this.taskActionLog = taskActionLog;
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

}
