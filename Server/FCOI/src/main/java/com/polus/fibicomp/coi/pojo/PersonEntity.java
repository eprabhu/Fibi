package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

import javax.persistence.*;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PERSON_ENTITY")
@EntityListeners(AuditingEntityListener.class)
public class PersonEntity implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "PERSON_ENTITY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer personEntityId;

	@Column(name = "PERSON_ENTITY_NUMBER")
	private Integer personEntityNumber;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_ENTITY_FK1"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;
	
	@Column(name = "ENTITY_ID")
	private Integer entityId;
	
	@ManyToOne(optional = true, cascade = CascadeType.REFRESH)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_ENTITY_FK2"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
	private CoiEntity coiEntity;
	
	@Column(name = "ENTITY_NUMBER")
	private Integer entityNumber;
	
	@Column(name = "IS_RELATIONSHIP_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isRelationshipActive;
	
	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;
	
	@Column(name = "VERSION_STATUS")
	private String versionStatus;
	
	@Column(name = "SPONSORS_RESEARCH")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean sponsorsResearch;
	
	@Column(name = "INVOLVEMENT_START_DATE")
	private Date involvementStartDate;

	@Column(name = "INVOLVEMENT_END_DATE")
	private Date involvementEndDate;
	
	@Column(name = "STUDENT_INVOLVEMENT")
	private String studentInvolvement;

	@Column(name = "STAFF_INVOLVEMENT")
	private String staffInvolvement;
	
	@Column(name = "INSTITUTE_RESOURCE_INVOLVEMENT")
	private String instituteResourceInvolvement;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "REVISION_REASON")
	private String revisionReason;

	@Transient
	private List<PersonEntityRelationship> personEntityRelationships;
	
	@Transient
	private List<ValidPersonEntityRelType> validPersonEntityRelTypes;

	@Transient
	private  String personFullName;
	
	@Transient
	private  Unit unit;
	
	@Transient
	private  String relationshipTypes;
	
	@Transient
	private  String designation;

	public Integer getPersonEntityId() {
		return personEntityId;
	}

	public void setPersonEntityId(Integer personEntityId) {
		this.personEntityId = personEntityId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
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

	public Boolean getIsRelationshipActive() {
		return isRelationshipActive;
	}

	public void setIsRelationshipActive(Boolean isRelationshipActive) {
		this.isRelationshipActive = isRelationshipActive;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public Date getInvolvementStartDate() {
		return involvementStartDate;
	}

	public void setInvolvementStartDate(Date involvementStartDate) {
		this.involvementStartDate = involvementStartDate;
	}

	public Date getInvolvementEndDate() {
		return involvementEndDate;
	}

	public void setInvolvementEndDate(Date involvementEndDate) {
		this.involvementEndDate = involvementEndDate;
	}

	public String getStudentInvolvement() {
		return studentInvolvement;
	}

	public void setStudentInvolvement(String studentInvolvement) {
		this.studentInvolvement = studentInvolvement;
	}

	public String getStaffInvolvement() {
		return staffInvolvement;
	}

	public void setStaffInvolvement(String staffInvolvement) {
		this.staffInvolvement = staffInvolvement;
	}

	public String getInstituteResourceInvolvement() {
		return instituteResourceInvolvement;
	}

	public void setInstituteResourceInvolvement(String instituteResourceInvolvement) {
		this.instituteResourceInvolvement = instituteResourceInvolvement;
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

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Boolean getSponsorsResearch() {
		return sponsorsResearch;
	}

	public void setSponsorsResearch(Boolean sponsorsResearch) {
		this.sponsorsResearch = sponsorsResearch;
	}

	public List<PersonEntityRelationship> getPersonEntityRelationships() {
		return personEntityRelationships;
	}

	public void setPersonEntityRelationships(List<PersonEntityRelationship> personEntityRelationships) {
		this.personEntityRelationships = personEntityRelationships;
	}

	public String getPersonFullName() {
		return personFullName;
	}

	public void setPersonFullName(String personFullName) {
		this.personFullName = personFullName;
	}

	public List<ValidPersonEntityRelType> getValidPersonEntityRelTypes() {
		return validPersonEntityRelTypes;
	}

	public void setValidPersonEntityRelTypes(List<ValidPersonEntityRelType> validPersonEntityRelTypes) {
		this.validPersonEntityRelTypes = validPersonEntityRelTypes;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public String getRelationshipTypes() {
		return relationshipTypes;
	}

	public void setRelationshipTypes(String relationshipTypes) {
		this.relationshipTypes = relationshipTypes;
	}

	public String getDesignation() {
		return designation;
	}

	public void setDesignation(String designation) {
		this.designation = designation;
	}

	public Integer getPersonEntityNumber() {
		return personEntityNumber;
	}

	public void setPersonEntityNumber(Integer personEntityNumber) {
		this.personEntityNumber = personEntityNumber;
	}

	public String getRevisionReason() {
		return revisionReason;
	}

	public void setRevisionReason(String revisionReason) {
		this.revisionReason = revisionReason;
	}
}
