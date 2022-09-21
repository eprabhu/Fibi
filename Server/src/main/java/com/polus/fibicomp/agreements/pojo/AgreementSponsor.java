package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

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
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.Sponsor;

@Entity
@Table(name = "AGREEMENT_SPONSOR")
public class AgreementSponsor implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_SPONSOR")
	@SequenceGenerator(name="SEQ_AGREEMENT_SPONSOR", sequenceName = "SEQ_AGREEMENT_SPONSOR", allocationSize=1)
	@Column(name = "AGREEMENT_SPONSOR_ID")
	private Integer agreementSponsorId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@Column(name = "SPONSOR_NAME")
	private String sponsorName;

	@Column(name = "REG_NUMBER")
	private String registrationNumber;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_SPONSOR_FK2"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "SPONSOR_ROLE_TYPE_CODE")
	private String sponsorRoleTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_SPONSOR_FK3"), name = "SPONSOR_ROLE_TYPE_CODE", referencedColumnName = "SPONSOR_ROLE_TYPE_CODE", insertable = false, updatable = false)
	private SponsorRole sponsorRole;

	@Column(name = "AGREEMENT_SPONSOR_TYPE_CODE")
	private String agreementSponsorTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_SPONSOR_FK4"), name = "AGREEMENT_SPONSOR_TYPE_CODE", referencedColumnName = "AGREEMENT_SPONSOR_TYPE_CODE", insertable = false, updatable = false)
	private AgreementSponsorType agreementSponsorType;

	@Column(name = "CONTACT_ADDRESS_LINE1")
	private String address;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private List<AgreementSponsorContact> agreementSponsorContacts;

	public AgreementSponsor() {
		agreementSponsorContacts = new ArrayList<>();
	}

	public Integer getAgreementSponsorId() {
		return agreementSponsorId;
	}

	public void setAgreementSponsorId(Integer agreementSponsorId) {
		this.agreementSponsorId = agreementSponsorId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getRegistrationNumber() {
		return registrationNumber;
	}

	public void setRegistrationNumber(String registrationNumber) {
		this.registrationNumber = registrationNumber;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public List<AgreementSponsorContact> getAgreementSponsorContacts() {
		return agreementSponsorContacts;
	}

	public void setAgreementSponsorContacts(List<AgreementSponsorContact> agreementSponsorContacts) {
		this.agreementSponsorContacts = agreementSponsorContacts;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public String getSponsorRoleTypeCode() {
		return sponsorRoleTypeCode;
	}

	public void setSponsorRoleTypeCode(String sponsorRoleTypeCode) {
		this.sponsorRoleTypeCode = sponsorRoleTypeCode;
	}

	public SponsorRole getSponsorRole() {
		return sponsorRole;
	}

	public void setSponsorRole(SponsorRole sponsorRole) {
		this.sponsorRole = sponsorRole;
	}

	public String getAgreementSponsorTypeCode() {
		return agreementSponsorTypeCode;
	}

	public void setAgreementSponsorTypeCode(String agreementSponsorTypeCode) {
		this.agreementSponsorTypeCode = agreementSponsorTypeCode;
	}

	public AgreementSponsorType getAgreementSponsorType() {
		return agreementSponsorType;
	}

	public void setAgreementSponsorType(AgreementSponsorType agreementSponsorType) {
		this.agreementSponsorType = agreementSponsorType;
	}

}
