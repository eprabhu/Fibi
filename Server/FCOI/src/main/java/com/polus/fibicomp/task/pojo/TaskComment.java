package com.polus.fibicomp.task.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
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

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "TASK_COMMENTS")
public class TaskComment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COMMENT_ID", length = 10)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TASK_COMMENT_ID_GENERATOR")
	@SequenceGenerator(name = "TASK_COMMENT_ID_GENERATOR", sequenceName = "TASK_COMMENT_ID_GENERATOR", allocationSize = 1)
	private Integer commentId;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "TASK_ID")
	private Integer taskId;

	@JsonIgnore
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_COMMENTS_FK1"), name = "TASK_ID", referencedColumnName = "TASK_ID", insertable = false, updatable = false)
	private Task task;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "TASK_COMMENTS_FK2"), name = "ACTION_LOG_ID", referencedColumnName = "ACTION_LOG_ID", insertable = false, updatable = false)
	private TaskActionLog taskActionLog;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "taskComment", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<TaskCommentAttachment> taskCommentAttachments;

	@Transient
	private String lastUpdateUserFullName;

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
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

	public List<TaskCommentAttachment> getTaskCommentAttachments() {
		return taskCommentAttachments;
	}

	public void setTaskCommentAttachments(List<TaskCommentAttachment> taskCommentAttachments) {
		this.taskCommentAttachments = taskCommentAttachments;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

}
