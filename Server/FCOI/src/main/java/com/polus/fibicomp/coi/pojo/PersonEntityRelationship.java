package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

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
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PERSON_ENTITY_RELATIONSHIP")
@EntityListeners(AuditingEntityListener.class)
public class PersonEntityRelationship implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_ENTITY_REL_TYPE")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer personEntityRelType;
	
	@Column(name = "PERSON_ENTITY_ID")
	private Integer personEntityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_ENTITY_RELATIONSHIP_FK1"), name = "PERSON_ENTITY_ID", referencedColumnName = "PERSON_ENTITY_ID", insertable = false, updatable = false)
	private PersonEntity personEntity;
	
	@Column(name = "VALID_PERSON_ENTITY_REL_TYPE_CODE")
	private Integer validPersonEntityRelTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_ENTITY_RELATIONSHIP_FK2"), name = "VALID_PERSON_ENTITY_REL_TYPE_CODE", referencedColumnName = "VALID_PERSON_ENTITY_REL_TYPE_CODE", insertable = false, updatable = false)
	private ValidPersonEntityRelType validPersonEntityRelType;
	
	@Column(name = "QUESTIONNAIRE_ANS_HEADER_ID")
	private Integer questionnaireAnsHeaderId;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "START_DATE")
	private Date startDate;

	@Column(name = "END_DATE")
	private Date endDate;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getPersonEntityRelType() {
		return personEntityRelType;
	}

	public void setPersonEntityRelType(Integer personEntityRelType) {
		this.personEntityRelType = personEntityRelType;
	}

	public Integer getPersonEntityId() {
		return personEntityId;
	}

	public void setPersonEntityId(Integer personEntityId) {
		this.personEntityId = personEntityId;
	}

	public PersonEntity getPersonEntity() {
		return personEntity;
	}

	public void setPersonEntity(PersonEntity personEntity) {
		this.personEntity = personEntity;
	}

	public Integer getValidPersonEntityRelTypeCode() {
		return validPersonEntityRelTypeCode;
	}

	public void setValidPersonEntityRelTypeCode(Integer validPersonEntityRelTypeCode) {
		this.validPersonEntityRelTypeCode = validPersonEntityRelTypeCode;
	}

	public ValidPersonEntityRelType getValidPersonEntityRelType() {
		return validPersonEntityRelType;
	}

	public void setValidPersonEntityRelType(ValidPersonEntityRelType validPersonEntityRelType) {
		this.validPersonEntityRelType = validPersonEntityRelType;
	}

	public Integer getQuestionnaireAnsHeaderId() {
		return questionnaireAnsHeaderId;
	}

	public void setQuestionnaireAnsHeaderId(Integer questionnaireAnsHeaderId) {
		this.questionnaireAnsHeaderId = questionnaireAnsHeaderId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public Date getEndDate() {
		return endDate;
	}

	public void setEndDate(Date endDate) {
		this.endDate = endDate;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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
