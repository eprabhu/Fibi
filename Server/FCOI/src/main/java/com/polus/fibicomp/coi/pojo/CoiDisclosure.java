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
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.util.JpaCharBooleanConversion;


@Entity
@Table(name = "COI_DISCLOSURE")
@EntityListeners(AuditingEntityListener.class)
public class CoiDisclosure implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "DISCLOSURE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer disclosureId;

	@Column(name = "DISCLOSURE_NUMBER")
	private String disclosureNumber;

	@Column(name = "DISCLOSURE_VERSION_NUMBER")
	private Integer disclosureVersionNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "DISCLOSURE_STATUS_CODE")
	private String disclosureStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_FK1"), name = "DISCLOSURE_STATUS_CODE", referencedColumnName = "DISCLOSURE_STATUS_CODE", insertable = false, updatable = false)
	private COIDisclosureStatus coiDisclosureStatus;

	@Column(name = "DISPOSITION_STATUS_TYPE_CODE")
	private String dispositionStatusTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_FK2"), name = "DISPOSITION_STATUS_TYPE_CODE", referencedColumnName = "DISPOSITION_STATUS_TYPE_CODE", insertable = false, updatable = false)
	private COIDispositionStatus coiDispositionStatus;

	@Column(name = "REVIEW_STATUS_TYPE_CODE")
	private String reviewStatusTypeCode ;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_FK3"), name = "REVIEW_STATUS_TYPE_CODE", referencedColumnName = "REVIEW_STATUS_TYPE_CODE", insertable = false, updatable = false)
	private COIReviewStatus coiReviewStatus;

	@Column(name = "DISCLOSURE_CATEGORY_TYPE_CODE")
	private String disclosureCategoryTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_FK4"), name = "DISCLOSURE_CATEGORY_TYPE_CODE", referencedColumnName = "DISCLOSURE_CATEGORY_TYPE_CODE", insertable = false, updatable = false)
	private COIDisclosureCategoryType coiDisclosureCategoryType;

	@Column(name = "DISCLOSURE_SEQUENCE_STATUS_CODE")
	private String disclosureSequenceStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_FK5"), name = "DISCLOSURE_SEQUENCE_STATUS_CODE", referencedColumnName = "DISCLOSURE_SEQUENCE_STATUS_CODE", insertable = false, updatable = false)
	private COIDisclosureSequenceStatus coiDisclosureSequence;

	@Column(name = "CERTIFICATION_TEXT")
	private String certificationText;

	@Column(name = "CERTIFICATION_TIMESTAMP")
	private Date certifiedTimestamp;

	@Column(name = "EXPIRATION_DATE")
	private Date expirationDate;

	@Column(name = "CERTIFIED_BY")
	private String certifiedBy;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "REVISE_COMMENT")
	private String reviseComment;

	@Column(name = "IS_DISCLOSURE_QUESTIONNAIRE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isDisclosureQuestionnaire;

	@Transient
	private String updateUserFullName;
	
	@Transient
	private String createUserFullName;

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
	}

	public String getDisclosureNumber() {
		return disclosureNumber;
	}

	public void setDisclosureNumber(String disclosureNumber) {
		this.disclosureNumber = disclosureNumber;
	}

	public Integer getDisclosureVersionNumber() {
		return disclosureVersionNumber;
	}

	public void setDisclosureVersionNumber(Integer disclosureVersionNumber) {
		this.disclosureVersionNumber = disclosureVersionNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getDisclosureStatusCode() {
		return disclosureStatusCode;
	}

	public void setDisclosureStatusCode(String disclosureStatusCode) {
		this.disclosureStatusCode = disclosureStatusCode;
	}

	public COIDisclosureStatus getCoiDisclosureStatus() {
		return coiDisclosureStatus;
	}

	public void setCoiDisclosureStatus(COIDisclosureStatus coiDisclosureStatus) {
		this.coiDisclosureStatus = coiDisclosureStatus;
	}

	public String getDispositionStatusTypeCode() {
		return dispositionStatusTypeCode;
	}

	public void setDispositionStatusTypeCode(String dispositionStatusTypeCode) {
		this.dispositionStatusTypeCode = dispositionStatusTypeCode;
	}

	public COIDispositionStatus getCoiDispositionStatus() {
		return coiDispositionStatus;
	}

	public void setCoiDispositionStatus(COIDispositionStatus coiDispositionStatus) {
		this.coiDispositionStatus = coiDispositionStatus;
	}

	public String getReviewStatusTypeCode() {
		return reviewStatusTypeCode;
	}

	public void setReviewStatusTypeCode(String reviewStatusTypeCode) {
		this.reviewStatusTypeCode = reviewStatusTypeCode;
	}

	public COIReviewStatus getCoiReviewStatus() {
		return coiReviewStatus;
	}

	public void setCoiReviewStatus(COIReviewStatus coiReviewStatus) {
		this.coiReviewStatus = coiReviewStatus;
	}

	public String getDisclosureCategoryTypeCode() {
		return disclosureCategoryTypeCode;
	}

	public void setDisclosureCategoryTypeCode(String disclosureCategoryTypeCode) {
		this.disclosureCategoryTypeCode = disclosureCategoryTypeCode;
	}

	public COIDisclosureCategoryType getCoiDisclosureCategoryType() {
		return coiDisclosureCategoryType;
	}

	public void setCoiDisclosureCategoryType(COIDisclosureCategoryType coiDisclosureCategoryType) {
		this.coiDisclosureCategoryType = coiDisclosureCategoryType;
	}

	public String getDisclosureSequenceStatusCode() {
		return disclosureSequenceStatusCode;
	}

	public void setDisclosureSequenceStatusCode(String disclosureSequenceStatusCode) {
		this.disclosureSequenceStatusCode = disclosureSequenceStatusCode;
	}

	public COIDisclosureSequenceStatus getCoiDisclosureSequence() {
		return coiDisclosureSequence;
	}

	public void setCoiDisclosureSequence(COIDisclosureSequenceStatus coiDisclosureSequence) {
		this.coiDisclosureSequence = coiDisclosureSequence;
	}

	public String getCertificationText() {
		return certificationText;
	}

	public void setCertificationText(String certificationText) {
		this.certificationText = certificationText;
	}

	public Date getCertifiedTimestamp() {
		return certifiedTimestamp;
	}

	public void setCertifiedTimestamp(Date certifiedTimestamp) {
		this.certifiedTimestamp = certifiedTimestamp;
	}

	public String getCertifiedBy() {
		return certifiedBy;
	}

	public void setCertifiedBy(String certifiedBy) {
		this.certifiedBy = certifiedBy;
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

	public Date getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Date expirationDate) {
		this.expirationDate = expirationDate;
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

	public String getReviseComment() {
		return reviseComment;
	}

	public void setReviseComment(String reviseComment) {
		this.reviseComment = reviseComment;
	}

	public Boolean getIsDisclosureQuestionnaire() {
		return isDisclosureQuestionnaire;
	}

	public void setIsDisclosureQuestionnaire(Boolean isDisclosureQuestionnaire) {
		this.isDisclosureQuestionnaire = isDisclosureQuestionnaire;
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

}
