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
import javax.persistence.Transient;

import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnelType;

@Entity
@Table(name = "AGREEMENT_PEOPLE")
public class AgreementPeople implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AGREEMENT_PEOPLE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_PEOPLE")
	@SequenceGenerator(name="SEQ_AGREEMENT_PEOPLE", sequenceName = "SEQ_AGREEMENT_PEOPLE", allocationSize=1)
	private Integer agreementPeopleId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "PEOPLE_TYPE_ID")
	private Integer peopleTypeId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_PEOPLE_FK2"), name = "PEOPLE_TYPE_ID", referencedColumnName = "PEOPLE_TYPE_ID", insertable = false, updatable = false)
	private AgreementPeopleType agreementPeopleType;

	@Column(name = "PI_PERSONNEL_TYPE_CODE")
	private String piPersonnelTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_PEOPLE_FK3"), name = "PI_PERSONNEL_TYPE_CODE", referencedColumnName = "PERSONNEL_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsPersonnelType piPersonnelType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String email;

	@Transient
	private String phoneNumber;

	@Transient
	private String department;

	public Integer getAgreementPeopleId() {
		return agreementPeopleId;
	}

	public void setAgreementPeopleId(Integer agreementPeopleId) {
		this.agreementPeopleId = agreementPeopleId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public Integer getPeopleTypeId() {
		return peopleTypeId;
	}

	public void setPeopleTypeId(Integer peopleTypeId) {
		this.peopleTypeId = peopleTypeId;
	}

	public AgreementPeopleType getAgreementPeopleType() {
		return agreementPeopleType;
	}

	public void setAgreementPeopleType(AgreementPeopleType agreementPeopleType) {
		this.agreementPeopleType = agreementPeopleType;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getPiPersonnelTypeCode() {
		return piPersonnelTypeCode;
	}

	public void setPiPersonnelTypeCode(String piPersonnelTypeCode) {
		this.piPersonnelTypeCode = piPersonnelTypeCode;
	}

	public NegotiationsPersonnelType getPiPersonnelType() {
		return piPersonnelType;
	}

	public void setPiPersonnelType(NegotiationsPersonnelType piPersonnelType) {
		this.piPersonnelType = piPersonnelType;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPhoneNumber() {
		return phoneNumber;
	}

	public void setPhoneNumber(String phoneNumber) {
		this.phoneNumber = phoneNumber;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}
}
