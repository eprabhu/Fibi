package com.polus.fibicomp.persontraining.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "TRAINING")
public class Training implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "TRAINING_CODE")
	private Integer trainingCode;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getTrainingCode() {
		return trainingCode;
	}

	public void setTrainingCode(Integer trainingCode) {
		this.trainingCode = trainingCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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
	
}
