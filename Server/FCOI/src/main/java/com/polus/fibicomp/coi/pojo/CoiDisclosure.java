package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

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
import javax.persistence.Transient;
import javax.persistence.FetchType;

import com.polus.fibicomp.pojo.Unit;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.person.pojo.Person;

@Entity
@Table(name = "COI_DISCLOSURE")
@EntityListeners(AuditingEntityListener.class)
public class CoiDisclosure implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "DISCLOSURE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer disclosureId;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE1_FK1"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;

	@Column(name = "HOME_UNIT")
	private String homeUnit;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DIS_HOME_UNIT_FK7"), name = "HOME_UNIT", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "DISCLOSURE_NUMBER")
	private Integer disclosureNumber;
	
	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;
	
	@Column(name = "VERSION_STATUS")
	private String versionStatus;
	
	@Column(name = "FCOI_TYPE_CODE")
	private String fcoiTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE1_FK2"), name = "FCOI_TYPE_CODE", referencedColumnName = "FCOI_TYPE_CODE", insertable = false, updatable = false)
	private CoiDisclosureFcoiType coiDisclosureFcoiType;
	
	@Column(name = "CONFLICT_STATUS_CODE")
	private String conflictStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE1_FK3"), name = "CONFLICT_STATUS_CODE", referencedColumnName = "CONFLICT_STATUS_CODE", insertable = false, updatable = false)
	private CoiConflictStatusType coiConflictStatusType;
	
	@Column(name = "DISPOSITION_STATUS_CODE")
	private String dispositionStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE1_FK4"), name = "DISPOSITION_STATUS_CODE", referencedColumnName = "DISPOSITION_STATUS_CODE", insertable = false, updatable = false)
	private CoiDispositionStatusType coiDispositionStatusType;
	
	@Column(name = "REVIEW_STATUS_CODE")
	private String reviewStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE1_FK5"), name = "REVIEW_STATUS_CODE", referencedColumnName = "REVIEW_STATUS_CODE", insertable = false, updatable = false)
	private CoiReviewStatusType coiReviewStatusType;
	
	@Column(name = "RISK_CATEGORY_CODE")
	private String riskCategoryCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE1_FK6"), name = "RISK_CATEGORY_CODE", referencedColumnName = "RISK_CATEGORY_CODE", insertable = false, updatable = false)
	private CoiRiskCategory coiRiskCategory;
	
	@Column(name = "MODULE_CODE")
	private Integer moduleCode;
	
	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;
	
	@Column(name = "EXPIRATION_DATE")
	private Date expirationDate;
	
	@Column(name = "CERTIFICATION_TEXT")
	private String certificationText;
	
	@Column(name = "CERTIFIED_BY")
	private String certifiedBy;
	
	@Column(name = "CERTIFIED_AT")
	private Timestamp certifiedAt;
	
	@Column(name = "REVISION_COMMENT")
	private String revisionComment;

	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;

	@Column(name = "ADMIN_PERSON_ID")
	private String adminPersonId;
	
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

	@Transient
	private String updateUserFullName;

	@Transient
	private String createUserFullName;

	@Transient
	private Integer numberOfSFI;

	@Transient
	private Integer numberOfProposals;
	
	@Transient
	private Integer numberOfAwards;
	
	@Transient
	private String coiProjectTypeCode;
	
	@Transient
	private String adminGroupName;

	@Transient
	private String adminPersonName;

	public Integer getNumberOfProposals() {
		return numberOfProposals;
	}

	public void setNumberOfProposals(Integer numberOfProposals) {
		this.numberOfProposals = numberOfProposals;
	}

	public Integer getNumberOfAwards() {
		return numberOfAwards;
	}

	public void setNumberOfAwards(Integer numberOfAwards) {
		this.numberOfAwards = numberOfAwards;
	}

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
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

	public String getFcoiTypeCode() {
		return fcoiTypeCode;
	}

	public void setFcoiTypeCode(String fcoiTypeCode) {
		this.fcoiTypeCode = fcoiTypeCode;
	}

	public CoiDisclosureFcoiType getCoiDisclosureFcoiType() {
		return coiDisclosureFcoiType;
	}

	public void setCoiDisclosureFcoiType(CoiDisclosureFcoiType coiDisclosureFcoiType) {
		this.coiDisclosureFcoiType = coiDisclosureFcoiType;
	}

	public String getConflictStatusCode() {
		return conflictStatusCode;
	}

	public void setConflictStatusCode(String conflictStatusCode) {
		this.conflictStatusCode = conflictStatusCode;
	}

	public CoiConflictStatusType getCoiConflictStatusType() {
		return coiConflictStatusType;
	}

	public void setCoiConflictStatusType(CoiConflictStatusType coiConflictStatusType) {
		this.coiConflictStatusType = coiConflictStatusType;
	}

	public String getDispositionStatusCode() {
		return dispositionStatusCode;
	}

	public void setDispositionStatusCode(String dispositionStatusCode) {
		this.dispositionStatusCode = dispositionStatusCode;
	}

	public CoiDispositionStatusType getCoiDispositionStatusType() {
		return coiDispositionStatusType;
	}

	public void setCoiDispositionStatusType(CoiDispositionStatusType coiDispositionStatusType) {
		this.coiDispositionStatusType = coiDispositionStatusType;
	}

	public String getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(String reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public CoiReviewStatusType getCoiReviewStatusType() {
		return coiReviewStatusType;
	}

	public void setCoiReviewStatusType(CoiReviewStatusType coiReviewStatusType) {
		this.coiReviewStatusType = coiReviewStatusType;
	}

	public String getRiskCategoryCode() {
		return riskCategoryCode;
	}

	public void setRiskCategoryCode(String riskCategoryCode) {
		this.riskCategoryCode = riskCategoryCode;
	}

	public CoiRiskCategory getCoiRiskCategory() {
		return coiRiskCategory;
	}

	public void setCoiRiskCategory(CoiRiskCategory coiRiskCategory) {
		this.coiRiskCategory = coiRiskCategory;
	}

	public Date getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Date expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getCertificationText() {
		return certificationText;
	}

	public void setCertificationText(String certificationText) {
		this.certificationText = certificationText;
	}

	public String getCertifiedBy() {
		return certifiedBy;
	}

	public void setCertifiedBy(String certifiedBy) {
		this.certifiedBy = certifiedBy;
	}

	public String getRevisionComment() {
		return revisionComment;
	}

	public void setRevisionComment(String revisionComment) {
		this.revisionComment = revisionComment;
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

	public Timestamp getCertifiedAt() {
		return certifiedAt;
	}

	public void setCertifiedAt(Timestamp certifiedAt) {
		this.certifiedAt = certifiedAt;
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

	public Integer getDisclosureNumber() {
		return disclosureNumber;
	}

	public void setDisclosureNumber(Integer disclosureNumber) {
		this.disclosureNumber = disclosureNumber;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public Integer getNumberOfSFI() {
		return numberOfSFI;
	}

	public void setNumberOfSFI(Integer numberOfSFI) {
		this.numberOfSFI = numberOfSFI;
	}

	public String getCoiProjectTypeCode() {
		return coiProjectTypeCode;
	}

	public void setCoiProjectTypeCode(String coiProjectTypeCode) {
		this.coiProjectTypeCode = coiProjectTypeCode;
	}

	public String getHomeUnit() {
		return homeUnit;
	}

	public void setHomeUnit(String homeUnit) {
		this.homeUnit = homeUnit;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public String getAdminPersonId() {
		return adminPersonId;
	}

	public void setAdminPersonId(String adminPersonId) {
		this.adminPersonId = adminPersonId;
	}

	public String getAdminGroupName() {
		return adminGroupName;
	}

	public void setAdminGroupName(String adminGroupName) {
		this.adminGroupName = adminGroupName;
	}

	public String getAdminPersonName() {
		return adminPersonName;
	}

	public void setAdminPersonName(String adminPersonName) {
		this.adminPersonName = adminPersonName;
	}
	
}
