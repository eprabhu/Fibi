package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AGREEMENT_SPONSOR_CONTACT")
public class AgreementSponsorContact implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_SPONSOR_CNTCT")
	@SequenceGenerator(name="SEQ_AGREEMENT_SPONSOR_CNTCT", sequenceName = "SEQ_AGREEMENT_SPONSOR_CNTCT", allocationSize=1)
	@Column(name = "AGREEMENT_SPONSOR_CONTACT_ID")
	private Integer agreementSponsorContactId;

	@Column(name = "AGREEMENT_SPONSOR_ID")
	private Integer agreementSponsorId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@Column(name = "SPONSOR_CONTCT_TYPE_CODE")
	private String sponsorContactTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_SPONSOR_CONTCT_FK2"), name = "SPONSOR_CONTCT_TYPE_CODE", referencedColumnName = "SPONSOR_CONTCT_TYPE_CODE", insertable = false, updatable = false)
	private AgreementSponsorContactType agreementSponsorContactType;

	@Column(name = "CONTACT_PERSON_NAME")
	private String contactPersonName;

	@Column(name = "CONTACT_EMAIL_ID")
	private String contactEmailId;

	@Column(name = "CONTACT_PHONE")
	private String contactPhone;

	@Column(name = "CONTACT_ADDRESS_LINE1")
	private String contactAddressLine;

	@Column(name = "CONTACT_CITY")
	private String contactCity;

	@Column(name = "CONTACT_STATE")
	private String contactState;

	@Column(name = "DESIGNATION")
	private String designation;

	@Column(name = "SALUTATION")
	private String salutation;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAgreementSponsorContactId() {
		return agreementSponsorContactId;
	}

	public void setAgreementSponsorContactId(Integer agreementSponsorContactId) {
		this.agreementSponsorContactId = agreementSponsorContactId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public String getSponsorContactTypeCode() {
		return sponsorContactTypeCode;
	}

	public void setSponsorContactTypeCode(String sponsorContactTypeCode) {
		this.sponsorContactTypeCode = sponsorContactTypeCode;
	}

	public String getContactPersonName() {
		return contactPersonName;
	}

	public void setContactPersonName(String contactPersonName) {
		this.contactPersonName = contactPersonName;
	}

	public String getContactEmailId() {
		return contactEmailId;
	}

	public void setContactEmailId(String contactEmailId) {
		this.contactEmailId = contactEmailId;
	}

	public String getContactPhone() {
		return contactPhone;
	}

	public void setContactPhone(String contactPhone) {
		this.contactPhone = contactPhone;
	}

	public String getContactAddressLine() {
		return contactAddressLine;
	}

	public void setContactAddressLine(String contactAddressLine) {
		this.contactAddressLine = contactAddressLine;
	}

	public String getContactCity() {
		return contactCity;
	}

	public void setContactCity(String contactCity) {
		this.contactCity = contactCity;
	}

	public String getContactState() {
		return contactState;
	}

	public void setContactState(String contactState) {
		this.contactState = contactState;
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

	public AgreementSponsorContactType getAgreementSponsorContactType() {
		return agreementSponsorContactType;
	}

	public void setAgreementSponsorContactType(AgreementSponsorContactType agreementSponsorContactType) {
		this.agreementSponsorContactType = agreementSponsorContactType;
	}

	public String getDesignation() {
		return designation;
	}

	public void setDesignation(String designation) {
		this.designation = designation;
	}

	public String getSalutation() {
		return salutation;
	}

	public void setSalutation(String salutation) {
		this.salutation = salutation;
	}

	public Integer getAgreementSponsorId() {
		return agreementSponsorId;
	}

	public void setAgreementSponsorId(Integer agreementSponsorId) {
		this.agreementSponsorId = agreementSponsorId;
	}

}
