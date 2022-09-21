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
@Table(name = "PERSON_TRAINING_ATTACHMENT")
@EntityListeners(AuditingEntityListener.class)
public class PersonTrainingAttachment implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_PERSON_TRAINING_ATTACHMENT")
	@SequenceGenerator(name = "SEQ_PERSON_TRAINING_ATTACHMENT", sequenceName = "SEQ_PERSON_TRAINING_ATTACHMENT", allocationSize = 1)
	@Column(name = "TRAINING_ATTACHMENT_ID")
	private Integer trainingAttachmentId;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "FILE_NAME")
	private String fileName;
	
	@Column(name = "MIME_TYPE")
	private String mimeType;
	
	@Column(name = "FILE_DATA_ID")
	private String fileDataId;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_TRAING_ATTACHMNT_FK1"), name = "PERSON_TRAINING_ID", referencedColumnName = "PERSON_TRAINING_ID")
	private PersonTraining personTraining;
	
	@Transient
	private String updateUserName;
	
	public Integer getTrainingAttachmentId() {
		return trainingAttachmentId;
	}

	public void setTrainingAttachmentId(Integer trainingAttachmentId) {
		this.trainingAttachmentId = trainingAttachmentId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void setMimeType(String mimeType) {
		this.mimeType = mimeType;
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

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}
}
