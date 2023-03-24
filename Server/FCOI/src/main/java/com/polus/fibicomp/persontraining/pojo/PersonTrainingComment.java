package com.polus.fibicomp.persontraining.pojo;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "PERSON_TRAINING_COMMENT")
@EntityListeners(AuditingEntityListener.class)
public class PersonTrainingComment implements Serializable {

	private static final long serialVersionUID = 1L;
		
	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_PERSON_TRAINING_COMMENT")
	@SequenceGenerator(name = "SEQ_PERSON_TRAINING_COMMENT", sequenceName = "SEQ_PERSON_TRAINING_COMMENT", allocationSize = 1)
	@Column(name = "TRAINING_COMMENT_ID")
	private Integer trainingCommentId;
	
	@Column(name = "COMMENT")
	private String comment;
		
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "PERSON_TRAINING_ID")
	private Integer personTrainingId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_TRAINING_COMMENT_FK1"), name = "PERSON_TRAINING_ID", referencedColumnName = "PERSON_TRAINING_ID" , insertable = false, updatable = false)
	private PersonTraining personTraining;
	
	@Transient
	private String updateUserName;
	
	public Integer getTrainingCommentId() {
		return trainingCommentId;
	}

	public void setTrainingCommentId(Integer trainingCommentId) {
		this.trainingCommentId = trainingCommentId;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
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

	public PersonTraining getPersonTraining() {
		return personTraining;
	}

	public void setPersonTraining(PersonTraining personTraining) {
		this.personTraining = personTraining;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}

	public Integer getPersonTrainingId() {
		return personTrainingId;
	}

	public void setPersonTrainingId(Integer personTrainingId) {
		this.personTrainingId = personTrainingId;
	}

}
