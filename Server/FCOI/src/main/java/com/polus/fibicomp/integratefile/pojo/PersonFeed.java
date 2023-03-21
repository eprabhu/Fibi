package com.polus.fibicomp.integratefile.pojo;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "PERSON_RT")
public class PersonFeed implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_RT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PERSON_RT_ID_GENERATOR")
	@SequenceGenerator(name = "PERSON_RT_ID_GENERATOR", sequenceName = "PERSON_RT_ID_GENERATOR", allocationSize = 1)
	private Integer personRtId;

	@Column(name = "PERSON_ID")
	private String personFeedId;

	@Column(name = "LAST_NAME")
	private String lastName;

	@Column(name = "USER_NAME")
	private String userName;

	@Column(name = "FIRST_NAME")
	private String firstName;

	@Column(name = "DEGREE")
	private String degree;

	@Column(name = "EMAIL_ADDRESS")
	private String emailAddress;

	@Column(name = "OFFICE_LOCATION")
	private String officeLocation;

	@Column(name = "OFFICE_PHONE")
	private String officePhone;

	@Column(name = "DIRECTORY_DEPARTMENT")
	private String directoryDepartment;

	@Column(name = "PRIMARY_TITLE")
	private String primaryTitle;

	@Column(name = "HOME_UNIT")
	private String homeUnit;

	@Column(name = "RELATIONSHIP")
	private String relationship;

	@Column(name = "ADDRESS_LINE_1")
	private String addressLineOne;

	@Column(name = "CITY")
	private String city;

	@Column(name = "STATE_PROVINCE")
	private String stateProvince;

	@Column(name = "ZIP_CODE")
	private String zipCode;

	public String getPersonFeedId() {
		return personFeedId;
	}

	public void setPersonFeedId(String personFeedId) {
		this.personFeedId = personFeedId;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getDegree() {
		return degree;
	}

	public void setDegree(String degree) {
		this.degree = degree;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public String getOfficeLocation() {
		return officeLocation;
	}

	public void setOfficeLocation(String officeLocation) {
		this.officeLocation = officeLocation;
	}

	public String getOfficePhone() {
		return officePhone;
	}

	public void setOfficePhone(String officePhone) {
		this.officePhone = officePhone;
	}

	public String getDirectoryDepartment() {
		return directoryDepartment;
	}

	public void setDirectoryDepartment(String directoryDepartment) {
		this.directoryDepartment = directoryDepartment;
	}

	public String getPrimaryTitle() {
		return primaryTitle;
	}

	public void setPrimaryTitle(String primaryTitle) {
		this.primaryTitle = primaryTitle;
	}

	public String getHomeUnit() {
		return homeUnit;
	}

	public void setHomeUnit(String homeUnit) {
		this.homeUnit = homeUnit;
	}

	public String getRelationship() {
		return relationship;
	}

	public void setRelationship(String relationship) {
		this.relationship = relationship;
	}

	public String getAddressLineOne() {
		return addressLineOne;
	}

	public void setAddressLineOne(String addressLineOne) {
		this.addressLineOne = addressLineOne;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getStateProvince() {
		return stateProvince;
	}

	public void setStateProvince(String stateProvince) {
		this.stateProvince = stateProvince;
	}

	public String getZipCode() {
		return zipCode;
	}

	public void setZipCode(String zipCode) {
		this.zipCode = zipCode;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public Integer getPersonRtId() {
		return personRtId;
	}

	public void setPersonRtId(Integer personRtId) {
		this.personRtId = personRtId;
	}

}
