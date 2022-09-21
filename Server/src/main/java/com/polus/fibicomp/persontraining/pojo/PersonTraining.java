package com.polus.fibicomp.persontraining.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PERSON_TRAINING")
@EntityListeners(AuditingEntityListener.class)
public class PersonTraining implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_PERSON_TRAINING")
	@SequenceGenerator(name = "SEQ_PERSON_TRAINING", sequenceName = "SEQ_PERSON_TRAINING", allocationSize = 1)
	@Column(name = "PERSON_TRAINING_ID")
	private Integer personTrainingId;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@Column(name = "TRAINING_CODE")
	private Integer trainingCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_TRAINING_FK1"), name = "TRAINING_CODE", referencedColumnName = "TRAINING_CODE", insertable = false, updatable = false)
	private Training training;
	
	@Column(name = "DATE_REQUESTED")
	private Timestamp dateRequested;
	
	@Column(name = "DATE_SUBMITTED")
	private Timestamp dateSubmitted;
	
	@Column(name = "DATE_ACKNOWLEDGED")
	private Timestamp dateAcknowledged;
	
	@Column(name = "FOLLOWUP_DATE")
	private Timestamp followupDate;
	
	@Column(name = "SCORE")
	private String score;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@Column(name = "IS_NON_EMPLOYEE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean nonEmployee;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "personTraining", orphanRemoval = true, cascade = { CascadeType.ALL })
	@OrderBy("trainingAttachmentId ASC")
	private List<PersonTrainingAttachment> personTrainingAttachments;

	@JsonManagedReference
	@OneToMany(mappedBy = "personTraining", orphanRemoval = true, cascade = { CascadeType.ALL })
	@OrderBy("trainingCommentId ASC")
	private List<PersonTrainingComment> personTrainingComments;
	
	@Transient
	private String trainingDescription;
	
	@Transient
	private String personName;
	
	@Transient
	private String updateUserName;

	@Transient
	private String expirationDate;

	@Transient
	private String completionDate;

	public Integer getPersonTrainingId() {
		return personTrainingId;
	}

	public void setPersonTrainingId(Integer personTrainingId) {
		this.personTrainingId = personTrainingId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getTrainingCode() {
		return trainingCode;
	}

	public void setTrainingCode(Integer trainingCode) {
		this.trainingCode = trainingCode;
	}

	public Timestamp getDateRequested() {
		return dateRequested;
	}

	public void setDateRequested(Timestamp dateRequested) {
		this.dateRequested = dateRequested;
	}

	public Timestamp getDateSubmitted() {
		return dateSubmitted;
	}

	public void setDateSubmitted(Timestamp dateSubmitted) {
		this.dateSubmitted = dateSubmitted;
	}

	public Timestamp getDateAcknowledged() {
		return dateAcknowledged;
	}

	public void setDateAcknowledged(Timestamp dateAcknowledged) {
		this.dateAcknowledged = dateAcknowledged;
	}

	public Timestamp getFollowupDate() {
		return followupDate;
	}

	public void setFollowupDate(Timestamp followupDate) {
		this.followupDate = followupDate;
	}

	public String getScore() {
		return score;
	}

	public void setScore(String score) {
		this.score = score;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
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

	public Training getTraining() {
		return training;
	}

	public void setTraining(Training training) {
		this.training = training;
	}

	public List<PersonTrainingAttachment> getPersonTrainingAttachments() {
		return personTrainingAttachments;
	}

	public void setPersonTrainingAttachments(List<PersonTrainingAttachment> personTrainingAttachments) {
		this.personTrainingAttachments = personTrainingAttachments;
	}

	public List<PersonTrainingComment> getPersonTrainingComments() {
		return personTrainingComments;
	}

	public void setPersonTrainingComments(List<PersonTrainingComment> personTrainingComments) {
		this.personTrainingComments = personTrainingComments;
	}

	public String getTrainingDescription() {
		return trainingDescription;
	}

	public void setTrainingDescription(String trainingDescription) {
		this.trainingDescription = trainingDescription;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public Boolean getNonEmployee() {
		return nonEmployee;
	}

	public void setNonEmployee(Boolean nonEmployee) {
		this.nonEmployee = nonEmployee;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}

	public String getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(String expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getCompletionDate() {
		return completionDate;
	}

	public void setCompletionDate(String completionDate) {
		this.completionDate = completionDate;
	}
}
