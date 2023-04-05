package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COI_FINANCIAL_ENTITY")
@EntityListeners(AuditingEntityListener.class)
public class COIFinancialEntity implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_FINANCIAL_ENTITY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer coiFinancialEntityId;

	@Column(name = "ENTITY_ID")
	private Integer coiEntityId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ENTITY_VERSION_NUMBER")
	private Integer entityVersionNumber;

	@Column(name = "INVOLVEMENT_START_DATE")
	private Date involvementStartDate;

	@Column(name = "INVOLVEMENT_END_DATE")
	private Date involvementEndDate;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

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

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Transient
	private List<COIFinancialEntityDetails> coiFinancialEntityDetails;
	
	@Transient
	private COIEntity coiEntity;

	public Integer getCoiFinancialEntityId() {
		return coiFinancialEntityId;
	}

	public void setCoiFinancialEntityId(Integer coiFinancialEntityId) {
		this.coiFinancialEntityId = coiFinancialEntityId;
	}

	public Integer getCoiEntityId() {
		return coiEntityId;
	}

	public void setCoiEntityId(Integer coiEntityId) {
		this.coiEntityId = coiEntityId;
	}

	public COIEntity getCoiEntity() {
		return coiEntity;
	}

	public void setCoiEntity(COIEntity coiEntity) {
		this.coiEntity = coiEntity;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getEntityVersionNumber() {
		return entityVersionNumber;
	}

	public void setEntityVersionNumber(Integer entityVersionNumber) {
		this.entityVersionNumber = entityVersionNumber;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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

	public List<COIFinancialEntityDetails> getCoiFinancialEntityDetails() {
		return coiFinancialEntityDetails;
	}

	public void setCoiFinancialEntityDetails(List<COIFinancialEntityDetails> coiFinancialEntityDetails) {
		this.coiFinancialEntityDetails = coiFinancialEntityDetails;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

}
