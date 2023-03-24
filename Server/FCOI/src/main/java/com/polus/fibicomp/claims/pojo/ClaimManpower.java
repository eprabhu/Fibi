package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CLAIM_MANPOWER")
@EntityListeners(AuditingEntityListener.class)
public final class ClaimManpower implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "CLAIM_MANPOWER_ID_GENERATOR")
	@SequenceGenerator(name = "CLAIM_MANPOWER_ID_GENERATOR", sequenceName = "CLAIM_MANPOWER_ID_GENERATOR", allocationSize = 1)
	@Column(name = "CLAIM_MANPOWER_ID")
	private Integer claimManpowerId;
	
	@Column(name = "CLAIM_NUMBER")
	private String claimNumber;
	
	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "AWARD_MANPOWER_ID")
	private Integer awardManpowerId;
	
	@Column(name = "AWARD_MANPOWER_RESOURCE_ID")
	private Integer awardManpowerResourceId;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@Column(name = "IS_GRANT_USED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isGrantUsed;
	
	@Column(name = "SOURCE_OF_FUNDING")
	private String sourceOfFunding;
	
	@Column(name = "IS_APPROVED_FOR_FOREIGN_STAFF")
	@Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isApprovedForForeignStaff;
	
	@Column(name = "DATE_OF_APPROVAL")
	private Date dateOfApproval;
	
	@Column(name = "IS_JOB_REQ_PHD_QUALIFICATION")
	@Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isJobReqPHDQualification;
	
	@Column(name = "PERCNTGE_TIME_SPENT_ON_PROG")
	private BigDecimal percntgeTimeSpentOnProg;
	
	@Column(name = "INSTITUTION")
	private String institution;
	
	@Column(name = "NATIONALITY_WAIVER_DESC")
	private String nationalityWaiverDesc;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Transient
	private String jobReqPHDQualificationValue;

	@Transient
	private String grantUsedValue;

	@Transient
	private String approvedForForeignStaff;
	
	@Transient
	private String fullName;

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public Integer getClaimManpowerId() {
		return claimManpowerId;
	}

	public void setClaimManpowerId(Integer claimManpowerId) {
		this.claimManpowerId = claimManpowerId;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public Integer getAwardManpowerId() {
		return awardManpowerId;
	}

	public void setAwardManpowerId(Integer awardManpowerId) {
		this.awardManpowerId = awardManpowerId;
	}

	public Integer getAwardManpowerResourceId() {
		return awardManpowerResourceId;
	}

	public void setAwardManpowerResourceId(Integer awardManpowerResourceId) {
		this.awardManpowerResourceId = awardManpowerResourceId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Boolean getIsGrantUsed() {
		return isGrantUsed;
	}

	public void setIsGrantUsed(Boolean isGrantUsed) {
		this.isGrantUsed = isGrantUsed;
	}

	public String getSourceOfFunding() {
		return sourceOfFunding;
	}

	public void setSourceOfFunding(String sourceOfFunding) {
		this.sourceOfFunding = sourceOfFunding;
	}

	public Boolean getIsApprovedForForeignStaff() {
		return isApprovedForForeignStaff;
	}

	public void setIsApprovedForForeignStaff(Boolean isApprovedForForeignStaff) {
		this.isApprovedForForeignStaff = isApprovedForForeignStaff;
	}

	public Date getDateOfApproval() {
		return dateOfApproval;
	}

	public void setDateOfApproval(Date dateOfApproval) {
		this.dateOfApproval = dateOfApproval;
	}

	public Boolean getIsJobReqPHDQualification() {
		return isJobReqPHDQualification;
	}

	public void setIsJobReqPHDQualification(Boolean isJobReqPHDQualification) {
		this.isJobReqPHDQualification = isJobReqPHDQualification;
	}

	public BigDecimal getPercntgeTimeSpentOnProg() {
		return percntgeTimeSpentOnProg;
	}

	public void setPercntgeTimeSpentOnProg(BigDecimal percntgeTimeSpentOnProg) {
		this.percntgeTimeSpentOnProg = percntgeTimeSpentOnProg;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getInstitution() {
		return institution;
	}

	public void setInstitution(String institution) {
		this.institution = institution;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public String getNationalityWaiverDesc() {
		return nationalityWaiverDesc;
	}

	public void setNationalityWaiverDesc(String nationalityWaiverDesc) {
		this.nationalityWaiverDesc = nationalityWaiverDesc;
	}

	public String getJobReqPHDQualificationValue() {
		return jobReqPHDQualificationValue;
	}

	public void setJobReqPHDQualificationValue(String jobReqPHDQualificationValue) {
		this.jobReqPHDQualificationValue = jobReqPHDQualificationValue;
	}

	public String getGrantUsedValue() {
		return grantUsedValue;
	}

	public void setGrantUsedValue(String grantUsedValue) {
		this.grantUsedValue = grantUsedValue;
	}

	public String getApprovedForForeignStaff() {
		return approvedForForeignStaff;
	}

	public void setApprovedForForeignStaff(String approvedForForeignStaff) {
		this.approvedForForeignStaff = approvedForForeignStaff;
	}
}
