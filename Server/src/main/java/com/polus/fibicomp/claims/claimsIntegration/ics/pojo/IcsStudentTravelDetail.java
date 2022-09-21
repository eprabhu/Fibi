package com.polus.fibicomp.claims.claimsIntegration.ics.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "ICS_STUDENT_TRAVEL_DTLS")
public class IcsStudentTravelDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ICS_REFERENCE_NUMBER")
	private String icsReferenceNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "COUNTRY")
	private String country;

	@Column(name = "PURPOSE_HEADER")
	private String purposeHeader;

	@Column(name = "PURPOSE_OF_TRIP")
	private String purposeOfTrip;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "VISIT_START_DATE")
	private Timestamp visitStartDate;

	@Column(name = "VISIT_END_DATE")
	private Timestamp visitEndDate;

	@Column(name = "EVENT_START_DATE")
	private Timestamp eventStartDate;

	@Column(name = "EVENT_END_DATE")
	private Timestamp eventEndDate;

	@Column(name = "SA_RATE", precision = 14, scale = 2)
	private BigDecimal saRate;

	@Column(name = "RECEIPT_AMT", precision = 14, scale = 2)
	private BigDecimal receiptAmount;

	@Column(name = "CLAIM_ITEM_DESCRIPTION")
	private String claimItemDescription;

	@Column(name = "CLAIM_TYPE")
	private String claimType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getPurposeHeader() {
		return purposeHeader;
	}

	public void setPurposeHeader(String purposeHeader) {
		this.purposeHeader = purposeHeader;
	}

	public String getPurposeOfTrip() {
		return purposeOfTrip;
	}

	public void setPurposeOfTrip(String purposeOfTrip) {
		this.purposeOfTrip = purposeOfTrip;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Timestamp getVisitStartDate() {
		return visitStartDate;
	}

	public void setVisitStartDate(Timestamp visitStartDate) {
		this.visitStartDate = visitStartDate;
	}

	public Timestamp getVisitEndDate() {
		return visitEndDate;
	}

	public void setVisitEndDate(Timestamp visitEndDate) {
		this.visitEndDate = visitEndDate;
	}

	public Timestamp getEventStartDate() {
		return eventStartDate;
	}

	public void setEventStartDate(Timestamp eventStartDate) {
		this.eventStartDate = eventStartDate;
	}

	public Timestamp getEventEndDate() {
		return eventEndDate;
	}

	public void setEventEndDate(Timestamp eventEndDate) {
		this.eventEndDate = eventEndDate;
	}

	public BigDecimal getSaRate() {
		return saRate;
	}

	public void setSaRate(BigDecimal saRate) {
		this.saRate = saRate;
	}

	public BigDecimal getReceiptAmount() {
		return receiptAmount;
	}

	public void setReceiptAmount(BigDecimal receiptAmount) {
		this.receiptAmount = receiptAmount;
	}

	public String getClaimItemDescription() {
		return claimItemDescription;
	}

	public void setClaimItemDescription(String claimItemDescription) {
		this.claimItemDescription = claimItemDescription;
	}

	public String getClaimType() {
		return claimType;
	}

	public void setClaimType(String claimType) {
		this.claimType = claimType;
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

	public String getIcsReferenceNumber() {
		return icsReferenceNumber;
	}

	public void setIcsReferenceNumber(String icsReferenceNumber) {
		this.icsReferenceNumber = icsReferenceNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}

}
