package com.polus.fibicomp.task.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.award.pojo.AwardTaskTypeMapping;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "TASK_TYPE")
public class TaskType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TASK_TYPE_CODE")
	private String taskTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_REVIEW_TASK")
	private String isReviewTask;

	@Column(name = "INSTRUCTION")
	private String instruction;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "taskType", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<AwardTaskTypeMapping> awardTaskTypeMappings;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public String getTaskTypeCode() {
		return taskTypeCode;
	}

	public void setTaskTypeCode(String taskTypeCode) {
		this.taskTypeCode = taskTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getIsReviewTask() {
		return isReviewTask;
	}

	public void setIsReviewTask(String isReviewTask) {
		this.isReviewTask = isReviewTask;
	}

	public String getInstruction() {
		return instruction;
	}

	public void setInstruction(String instruction) {
		this.instruction = instruction;
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

	public List<AwardTaskTypeMapping> getAwardTaskTypeMappings() {
		return awardTaskTypeMappings;
	}

	public void setAwardTaskTypeMappings(List<AwardTaskTypeMapping> awardTaskTypeMappings) {
		this.awardTaskTypeMappings = awardTaskTypeMappings;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
