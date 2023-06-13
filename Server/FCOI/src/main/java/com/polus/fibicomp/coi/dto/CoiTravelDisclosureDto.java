package com.polus.fibicomp.coi.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;

import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureTraveler;

public class CoiTravelDisclosureDto {

	private Integer travelDisclosureId;
	private String versionStatus;
	private Integer entityId;
	private Integer entityNumber;
	private String travelEntityName;
	private String travelTitle;
	private String purposeOfTheTrip;
	private BigDecimal travelAmount;
	private Date travelStartDate;
	private Date travelEndDate;
	private String destinationCity;
	private String destinationCountry;
	private String travelState;
	private String relationshipToYourResearch;
	private String acknowledgeBy;
	private String acknowledgeAt;
	private Timestamp updateTimestamp;
	private String updateUser;
	private String createUser;
	private Timestamp createTimestamp;
	private String travellerHomeUnit;
	private String description;
	private Date travelSubmissionDate;
	private String travelDisclosureStatus;
	private String travelDisclosureStatusCode;
	private String dispositionStatus;
	private String dispositionStatusCode;
	private String reviewStatus;
	private String reviewStatusCode;
	private String adminPersonId;
	private Integer adminGroupId;
	private String adminPersonName; 
	private String adminGroupName;
	private String homeUnitNumber;
	private String homeUnitName;
	private Boolean isInterNationalTravel;
	private List<String> travellerTypeCodeList;
	private String personId;
	private String personFullName;

	public String getPersonFullName() {
		return personFullName;
	}

	public void setPersonFullName(String personFullName) {
		this.personFullName = personFullName;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Boolean getIsInterNationalTravel() {
		return isInterNationalTravel;
	}

	public void setIsInterNationalTravel(Boolean isInterNationalTravel) {
		this.isInterNationalTravel = isInterNationalTravel;
	}

	public String getTravelEntityName() {
		return travelEntityName;
	}

	public void setTravelEntityName(String travelEntityName) {
		this.travelEntityName = travelEntityName;
	}

	public List<String> getTravellerTypeCodeList() {
		return travellerTypeCodeList;
	}

	public void setTravellerTypeCodeList(List<String> travellerTypeCodeList) {
		this.travellerTypeCodeList = travellerTypeCodeList;
	}

	public String getHomeUnitNumber() {
		return homeUnitNumber;
	}

	public void setHomeUnitNumber(String homeUnitNumber) {
		this.homeUnitNumber = homeUnitNumber;
	}

	public String getHomeUnitName() {
		return homeUnitName;
	}

	public void setHomeUnitName(String homeUnitName) {
		this.homeUnitName = homeUnitName;
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

	public String getTravelDisclosureStatusCode() {
		return travelDisclosureStatusCode;
	}

	public void setTravelDisclosureStatusCode(String travelDisclosureStatusCode) {
		this.travelDisclosureStatusCode = travelDisclosureStatusCode;
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

	public String getTravelDisclosureStatus() {
		return travelDisclosureStatus;
	}

	public void setTravelDisclosureStatus(String travelDisclosureStatus) {
		this.travelDisclosureStatus = travelDisclosureStatus;
	}

	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public Integer getEntityId() {
		return entityId;
	}

	public void setEntityId(Integer entityId) {
		this.entityId = entityId;
	}

	public Integer getEntityNumber() {
		return entityNumber;
	}

	public void setEntityNumber(Integer entityNumber) {
		this.entityNumber = entityNumber;
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

	public String getTravelState() {
		return travelState;
	}

	public void setTravelState(String travelstate) {
		this.travelState = travelstate;
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

	public String getAcknowledgeAt() {
		return acknowledgeAt;
	}

	public void setAcknowledgeAt(String acknowledgeAt) {
		this.acknowledgeAt = acknowledgeAt;
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

	public String getDispositionStatus() {
		return dispositionStatus;
	}

	public void setDispositionStatus(String dispositionStatus) {
		this.dispositionStatus = dispositionStatus;
	}

	public String getReviewStatus() {
		return reviewStatus;
	}

	public void setReviewStatus(String reviewStatus) {
		this.reviewStatus = reviewStatus;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public String getAdminPersonName() {
		return adminPersonName;
	}

	public void setAdminPersonName(String adminPersonName) {
		this.adminPersonName = adminPersonName;
	}

}
