package com.polus.fibicomp.claims.claimsIntegration.sapconcur.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "CONCUR_STAFF_TRAVEL_DTLS")
public class ConcurStaffTravelDtls {

	@Id
	@Column(name = "CONCUR_STAFF_TRAVEL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_CONCUR_STAFF_TRAVEL_DTLS")
	@SequenceGenerator(name = "SEQ_CONCUR_STAFF_TRAVEL_DTLS", sequenceName = "SEQ_CONCUR_STAFF_TRAVEL_DTLS", allocationSize = 1)
	private Integer concurStaffTravelId;

	@Column(name = "CONCUR_REFERENCE_NUMBER")
	private String concurReferenceNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "DESTINATION_CITY")
	private String destinationCity;

	@Column(name = "DESTINATION_COUNTRY")
	private String destinationCountry;

	@Column(name = "PARENT_EXPENSE_TYPE")
	private String parentExpenseType;

	@Column(name = "PURPOSE_OF_TRIP")
	private String purposeOfTrip;

	@Column(name = "BUSINESS_PURPOSE")
	private String businessPurpose;

	@Column(name = "COMMENT")
	private String comment;

	@Column(name = "VISIT_START_DATE")
	private String visitStartDate;

	@Column(name = "VISIT_END_DATE")
	private String visitEndDate;

	@Column(name = "EVENT_START_DATE")
	private String eventStartDate;

	@Column(name = "EVENT_END_DATE")
	private String eventEndDate;

	@Column(name = "TOTAL_COLA")
	private String totalCola;

	@Column(name = "DURATION")
	private String Duration;

	@Column(name = "FILE_ID")
	private Integer fileId;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "REMARKS")
	private String remarks;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getConcurReferenceNumber() {
		return concurReferenceNumber;
	}

	public void setConcurReferenceNumber(String concurReferenceNumber) {
		this.concurReferenceNumber = concurReferenceNumber;
	}

	public String getDestinationCity() {
		return destinationCity;
	}

	public void setDestinationCity(String destinationCity) {
		this.destinationCity = destinationCity;
	}

	public String getParentExpenseType() {
		return parentExpenseType;
	}

	public void setParentExpenseType(String parentExpenseType) {
		this.parentExpenseType = parentExpenseType;
	}

	public String getPurposeOfTrip() {
		return purposeOfTrip;
	}

	public void setPurposeOfTrip(String purposeOfTrip) {
		this.purposeOfTrip = purposeOfTrip;
	}

	public String getBusinessPurpose() {
		return businessPurpose;
	}

	public void setBusinessPurpose(String businessPurpose) {
		this.businessPurpose = businessPurpose;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getVisitStartDate() {
		return visitStartDate;
	}

	public void setVisitStartDate(String visitStartDate) {
		this.visitStartDate = visitStartDate;
	}

	public String getVisitEndDate() {
		return visitEndDate;
	}

	public void setVisitEndDate(String visitEndDate) {
		this.visitEndDate = visitEndDate;
	}

	public String getEventStartDate() {
		return eventStartDate;
	}

	public void setEventStartDate(String eventStartDate) {
		this.eventStartDate = eventStartDate;
	}

	public String getEventEndDate() {
		return eventEndDate;
	}

	public void setEventEndDate(String eventEndDate) {
		this.eventEndDate = eventEndDate;
	}

	public String getTotalCola() {
		return totalCola;
	}

	public void setTotalCola(String totalCola) {
		this.totalCola = totalCola;
	}

	public String getDuration() {
		return Duration;
	}

	public void setDuration(String duration) {
		Duration = duration;
	}

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
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

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}


	public Integer getConcurStaffTravelId() {
		return concurStaffTravelId;
	}


	public void setConcurStaffTravelId(Integer concurStaffTravelId) {
		this.concurStaffTravelId = concurStaffTravelId;
	}

	public String getDestinationCountry() {
		return destinationCountry;
	}

	public void setDestinationCountry(String destinationCountry) {
		this.destinationCountry = destinationCountry;
	}

}
