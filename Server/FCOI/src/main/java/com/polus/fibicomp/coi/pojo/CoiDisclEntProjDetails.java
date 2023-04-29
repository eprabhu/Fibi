package com.polus.fibicomp.coi.pojo;

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
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "COI_DISCL_ENT_PROJ_DETAILS")
@EntityListeners(AuditingEntityListener.class)
public class CoiDisclEntProjDetails implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "DISCLOSURE_DETAILS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer disclosureDetailsId;
	
	@Column(name = "DISCLOSURE_ID")
	private Integer disclosureId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCL_ENT_PROJ_DETAILS_FK1"), name = "DISCLOSURE_ID", referencedColumnName = "DISCLOSURE_ID", insertable = false, updatable = false)
	private CoiDisclosure coiDisclosure;
	
	@Column(name = "DISCLOSURE_NUMBER")
	private Integer disclosureNumber;
	
	@Column(name = "PERSON_ENTITY_ID")
	private Integer personEntityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCL_ENT_PROJ_DETAILS_FK2"), name = "PERSON_ENTITY_ID", referencedColumnName = "PERSON_ENTITY_ID", insertable = false, updatable = false)
	private PersonEntity personEntity;
	
	@Column(name = "ENTITY_ID")
	private Integer entityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCL_ENT_PROJ_DETAILS_FK3"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
	private CoiEntity coiEntity;
	
	@Column(name = "ENTITY_NUMBER")
	private Integer entityNumber;
	
	@Column(name = "MODULE_CODE")
	private Integer moduleCode;
	
	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;
	
	@Column(name = "PROJECT_CONFLICT_STATUS_CODE")
	private String projectConflictStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCL_ENT_PROJ_DETAILS_FK4"), name = "PROJECT_CONFLICT_STATUS_CODE", referencedColumnName = "PROJECT_CONFLICT_STATUS_CODE", insertable = false, updatable = false)
	private CoiProjConflictStatusType coiProjConflictStatusType;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getDisclosureDetailsId() {
		return disclosureDetailsId;
	}

	public void setDisclosureDetailsId(Integer disclosureDetailsId) {
		this.disclosureDetailsId = disclosureDetailsId;
	}

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
	}

	public CoiDisclosure getCoiDisclosure() {
		return coiDisclosure;
	}

	public void setCoiDisclosure(CoiDisclosure coiDisclosure) {
		this.coiDisclosure = coiDisclosure;
	}

	public Integer getDisclosureNumber() {
		return disclosureNumber;
	}

	public void setDisclosureNumber(Integer disclosureNumber) {
		this.disclosureNumber = disclosureNumber;
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

	public Integer getEntityId() {
		return entityId;
	}

	public void setEntityId(Integer entityId) {
		this.entityId = entityId;
	}

	public CoiEntity getCoiEntity() {
		return coiEntity;
	}

	public void setCoiEntity(CoiEntity coiEntity) {
		this.coiEntity = coiEntity;
	}

	public Integer getEntityNumber() {
		return entityNumber;
	}

	public void setEntityNumber(Integer entityNumber) {
		this.entityNumber = entityNumber;
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

	public String getProjectConflictStatusCode() {
		return projectConflictStatusCode;
	}

	public void setProjectConflictStatusCode(String projectConflictStatusCode) {
		this.projectConflictStatusCode = projectConflictStatusCode;
	}

	public CoiProjConflictStatusType getCoiProjConflictStatusType() {
		return coiProjConflictStatusType;
	}

	public void setCoiProjConflictStatusType(CoiProjConflictStatusType coiProjConflictStatusType) {
		this.coiProjConflictStatusType = coiProjConflictStatusType;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}
	
}
