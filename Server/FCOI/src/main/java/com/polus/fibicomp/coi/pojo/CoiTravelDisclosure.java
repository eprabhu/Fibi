package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.Map;

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

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COI_TRAVEL_DISCLOSURE")
@EntityListeners(AuditingEntityListener.class)
public class CoiTravelDisclosure implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TRAVEL_DISCLOSURE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer travelDisclosureId;
	
	@Column(name = "TRAVEL_NUMBER")
	private Integer travelNumber;
	
	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;
	
	@Column(name = "VERSION_STATUS")
	private String versionStatus;
	
	@Column(name = "PERSON_ENTITY_ID")
	private Integer personEntityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK1"), name = "PERSON_ENTITY_ID", referencedColumnName = "PERSON_ENTITY_ID", insertable = false, updatable = false)
	private PersonEntity personEntity;
	
	@Column(name = "ENTITY_ID")
	private Integer entityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK2"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
	private CoiEntity CoiEntity;
	
	@Column(name = "ENTITY_NUMBER")
	private Integer entityNumber;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK3"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;
	
	@Column(name = "TRAVEL_STATUS_CODE")
	private String travelStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK4"), name = "TRAVEL_STATUS_CODE", referencedColumnName = "TRAVEL_STATUS_CODE", insertable = false, updatable = false)
	private CoiTravelerStatusType coiTravelerStatusType;
	
	@Column(name = "IS_SPONSORED_TRAVEL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSponsoredTravel;
	
	@Column(name = "IS_INTERNATIONAL_TRAVEL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isInterNationalTravel;
	
	@Column(name = "TRAVEL_TITLE")
	private String travelTitle;
	
	@Column(name = "PURPOSE_OF_THE_TRIP")
	private String purposeOfTheTrip;
	
	@Column(name = "TRAVEL_AMOUNT", precision = 12, scale = 2)
	private BigDecimal travelAmount;
	
	@Column(name = "TRAVEL_START_DATE")
	private Date travelStartDate;
	
	@Column(name = "TRAVEL_END_DATE")
	private Date travelEndDate;

	@Column(name = "NO_OF_DAYS")
	private Integer noOfDays;
	
	@Column(name = "DESTINATION_CITY")
	private String destinationCity;
	
	@Column(name = "DESTINATION_COUNTRY")
	private String destinationCountry;
	
	@Column(name = "STATE")
	private String travelstate;
	
	@Column(name = "RELATIONSHIP_TO_YOUR_RESEARCH")
	private String relationshipToYourResearch;
	
	@Column(name = "ACKNOWLEDGE_BY")
	private String acknowledgeBy;
	
	@Column(name = "ACKNOWLEDGE_AT")
	private Timestamp acknowledgeAt;
	
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
	private List<String> coiTravellerTypeCodeList;
	
	@Column(name = "HOME_UNIT")
	private String travellerHomeUnit;
	
	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "SUBMISSION_DATE")
	private Date travelSubmissionDate;
	
	@Column(name = "TRAVEL_DISCLOSURE_STATUS_CODE")
	private String disclosureStatusCode;
	
	@Column(name = "DISPOSITION_STATUS_CODE")
	private String dispositionStatusCode;
	
	@Column(name = "REVIEW_STATUS_CODE")
	private String reviewStatusCode;
	
	@Column(name = "TRAVELER_TYPE_CODE")
	private String travellerTypeCode;
	
	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;
	
	@Column(name = "ADMIN_PERSON_ID")
	private String adminPersonId;
	
	@Column(name = "CERTIFIED_BY")
	private String certifiedBy;
	
	@Column(name = "CERTIFIED_AT")
	private Timestamp certifiedAt;
	
	@Column(name = "DOCUMENT_STATUS_CODE")
	private String documentStatusCode;

	@Transient
	private Unit travellerUnitDetails;
	
	@Transient
	private CoiTravelDisclosureStatusType coiTravelDisclosureStatusTypeDetalis;

	@Transient
	private String adminGroupName;

	@Transient
	private String adminPersonName;

	@Transient
	private String personFullName;
	
	@Transient
	Map<String, String> travellerTypeCodeList;
	
	@Transient
	private CoiTravelDocumentStatusType coiDocumentStatusTypeDetalis;
	
	@Transient
	private CoiTravelReviewStatusType coiTravelReviewStatusTypeDetails;

	@Transient
	private CoiTravelDisclosureStatusType coiTravelDisclosureStatusList;
	
	@Transient
	private CoiEntity entityDetails;

	public CoiEntity getEntityDetails() {
		return entityDetails;
	}

	public void setEntityDetails(CoiEntity entityDetails) {
		this.entityDetails = entityDetails;
	}

	public String getPersonFullName() {
		return personFullName;
	}

	public Map<String, String> getTravellerTypeCodeList() {
		return travellerTypeCodeList;
	}

	public void setTravellerTypeCodeList(Map<String, String> travellerTypeCodeList) {
		this.travellerTypeCodeList = travellerTypeCodeList;
	}

	public void setPersonFullName(String personFullName) {
		this.personFullName = personFullName;
	}

	public Timestamp getAcknowledgeAt() {
		return acknowledgeAt;
	}

	public void setAcknowledgeAt(Timestamp acknowledgeAt) {
		this.acknowledgeAt = acknowledgeAt;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public CoiTravelReviewStatusType getCoiTravelReviewStatusTypeDetails() {
		return coiTravelReviewStatusTypeDetails;
	}

	public void setCoiTravelReviewStatusTypeDetails(CoiTravelReviewStatusType coiTravelReviewStatusTypeDetails) {
		this.coiTravelReviewStatusTypeDetails = coiTravelReviewStatusTypeDetails;
	}

	public CoiTravelDocumentStatusType getCoiDocumentStatusTypeDetalis() {
		return coiDocumentStatusTypeDetalis;
	}

	public void setCoiDocumentStatusTypeDetalis(CoiTravelDocumentStatusType coiDocumentStatusTypeDetalis) {
		this.coiDocumentStatusTypeDetalis = coiDocumentStatusTypeDetalis;
	}

	public String getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(String documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public String getCertifiedBy() {
		return certifiedBy;
	}

	public void setCertifiedBy(String certifiedBy) {
		this.certifiedBy = certifiedBy;
	}

	public Timestamp getCertifiedAt() {
		return certifiedAt;
	}

	public void setCertifiedAt(Timestamp certifiedAt) {
		this.certifiedAt = certifiedAt;
	}
	
	public String getAdminPersonId() {
		return adminPersonId;
	}

	public void setAdminPersonId(String adminPersonId) {
		this.adminPersonId = adminPersonId;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
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

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public String getTravellerTypeCode() {
		return travellerTypeCode;
	}

	public void setTravellerTypeCode(String travellerTypeCode) {
		this.travellerTypeCode = travellerTypeCode;
	}

	public CoiTravelDisclosureStatusType getCoiTravelDisclosureStatusTypeDetalis() {
		return coiTravelDisclosureStatusTypeDetalis;
	}

	public void setCoiTravelDisclosureStatusTypeDetalis(
			CoiTravelDisclosureStatusType coiTravelDisclosureStatusTypeDetalis) {
		this.coiTravelDisclosureStatusTypeDetalis = coiTravelDisclosureStatusTypeDetalis;
	}

	public CoiTravelDisclosureStatusType getCoiTravelDisclosureStatusList() {
		return coiTravelDisclosureStatusList;
	}

	public void setCoiTravelDisclosureStatusList(CoiTravelDisclosureStatusType coiTravelDisclosureStatusList) {
		this.coiTravelDisclosureStatusList = coiTravelDisclosureStatusList;
	}

	public String getDisclosureStatusCode() {
		return disclosureStatusCode;
	}

	public void setDisclosureStatusCode(String disclosureStatusCode) {
		this.disclosureStatusCode = disclosureStatusCode;
	}

	public String getDispositionStatusCode() {
		return dispositionStatusCode;
	}

	public void setDispositionStatusCode(String dispositionStatusCode) {
		this.dispositionStatusCode = dispositionStatusCode;
	}

	public String getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(String reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public Unit getTravellerUnitDetails() {
		return travellerUnitDetails;
	}

	public void setTravellerUnitDetails(Unit travellerUnitDetails) {
		this.travellerUnitDetails = travellerUnitDetails;
	}

	public String getTravellerHomeUnit() {
		return travellerHomeUnit;
	}

	public void setTravellerHomeUnit(String travellerHomeUnit) {
		this.travellerHomeUnit = travellerHomeUnit;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Date getTravelSubmissionDate() {
		return travelSubmissionDate;
	}

	public void setTravelSubmissionDate(Date travelSubmissionDate) {
		this.travelSubmissionDate = travelSubmissionDate;
	}

	public List<String> getCoiTravellerTypeCodeList() {
		return coiTravellerTypeCodeList;
	}

	public void setCoiTravellerTypeCodeList(List<String> coiTravellerTypeCodeList) {
		this.coiTravellerTypeCodeList = coiTravellerTypeCodeList;
	}

	public Boolean getIsInterNationalTravel() {
		return isInterNationalTravel;
	}

	public void setIsInterNationalTravel(Boolean isInterNationalTravel) {
		this.isInterNationalTravel = isInterNationalTravel;
	}

	public String getTravelstate() {
		return travelstate;
	}

	public void setTravelstate(String travelstate) {
		this.travelstate = travelstate;
	}

	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public Integer getTravelNumber() {
		return travelNumber;
	}

	public void setTravelNumber(Integer travelNumber) {
		this.travelNumber = travelNumber;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
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
		return CoiEntity;
	}

	public void setCoiEntity(CoiEntity CoiEntity) {
		this.CoiEntity = CoiEntity;
	}

	public Integer getEntityNumber() {
		return entityNumber;
	}

	public void setEntityNumber(Integer entityNumber) {
		this.entityNumber = entityNumber;
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

	public String getTravelStatusCode() {
		return travelStatusCode;
	}

	public void setTravelStatusCode(String travelStatusCode) {
		this.travelStatusCode = travelStatusCode;
	}

	public CoiTravelerStatusType getCoiTravelerStatusType() {
		return coiTravelerStatusType;
	}

	public void setCoiTravelerStatusType(CoiTravelerStatusType coiTravelerStatusType) {
		this.coiTravelerStatusType = coiTravelerStatusType;
	}

	public Boolean getIsSponsoredTravel() {
		return isSponsoredTravel;
	}

	public void setIsSponsoredTravel(Boolean isSponsoredTravel) {
		this.isSponsoredTravel = isSponsoredTravel;
	}

	public String getTravelTitle() {
		return travelTitle;
	}

	public void setTravelTitle(String travelTitle) {
		this.travelTitle = travelTitle;
	}

	public String getPurposeOfTheTrip() {
		return purposeOfTheTrip;
	}

	public void setPurposeOfTheTrip(String purposeOfTheTrip) {
		this.purposeOfTheTrip = purposeOfTheTrip;
	}

	public BigDecimal getTravelAmount() {
		return travelAmount;
	}

	public void setTravelAmount(BigDecimal travelAmount) {
		this.travelAmount = travelAmount;
	}

	public Date getTravelStartDate() {
		return travelStartDate;
	}

	public void setTravelStartDate(Date travelStartDate) {
		this.travelStartDate = travelStartDate;
	}
	
	public Date getTravelEndDate() {
		return travelEndDate;
	}

	public void setTravelEndDate(Date travelEndDate) {
		this.travelEndDate = travelEndDate;
	}

	public Integer getNoOfDays() {
		return noOfDays;
	}

	public void setNoOfDays(Integer noOfDays) {
		this.noOfDays = noOfDays;
	}

	public String getDestinationCity() {
		return destinationCity;
	}

	public void setDestinationCity(String destinationCity) {
		this.destinationCity = destinationCity;
	}

	public String getDestinationCountry() {
		return destinationCountry;
	}

	public void setDestinationCountry(String destinationCountry) {
		this.destinationCountry = destinationCountry;
	}

	public String getRelationshipToYourResearch() {
		return relationshipToYourResearch;
	}

	public void setRelationshipToYourResearch(String relationshipToYourResearch) {
		this.relationshipToYourResearch = relationshipToYourResearch;
	}

	public String getAcknowledgeBy() {
		return acknowledgeBy;
	}

	public void setAcknowledgeBy(String acknowledgeBy) {
		this.acknowledgeBy = acknowledgeBy;
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

}
