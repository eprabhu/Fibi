package com.polus.fibicomp.person.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PERSON")
public class Person implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	/* commenting the auto generation for running the person feed in KU 
	@GenericGenerator(name = "personIdGenerator", strategy = "com.polus.fibicomp.generator.PersonIdGenerator")
	@GeneratedValue(generator = "personIdGenerator")*/
	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "LAST_NAME")
	private String lastName;

	@Column(name = "FIRST_NAME")
	private String firstName;

	@Column(name = "MIDDLE_NAME")
	private String middleName;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "PRIOR_NAME")
	private String priorName;

	@Column(name = "USER_NAME", unique = true)
	private String principalName;

	@Column(name = "EMAIL_ADDRESS")
	private String emailAddress;

	@Column(name = "DATE_OF_BIRTH")
	private Timestamp dateOfBirth;

	@Column(name = "AGE")
	private Integer age;

	@JsonProperty(access = Access.WRITE_ONLY)
	@Column(name = "AGE_BY_FISCAL_YEAR ")
	private Integer ageByFiscalYear;

	@Column(name = "GENDER")
	private String gender;

	@Column(name = "EDUCATION_LEVEL")
	private String educationLevel;

	@Column(name = "OFFICE_LOCATION")
	private String officeLocation;

	@Column(name = "SECONDRY_OFFICE_LOCATION")
	private String secOfficeLocation;

	@Column(name = "SECONDRY_OFFICE_PHONE")
	private String secOfficePhone;

	@Column(name = "SCHOOL")
	private String school;

	@Column(name = "DIRECTORY_DEPARTMENT")
	private String directoryDepartment;

	@JsonProperty(access = Access.WRITE_ONLY)
	@Column(name = "SALUTATION")
	private String salutation;

	@Column(name = "COUNTRY_OF_CITIZENSHIP")
	private String countryOfCitizenshipCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_COUNTRY_FK1"), name = "COUNTRY_OF_CITIZENSHIP", referencedColumnName = "COUNTRY_CODE", insertable = false, updatable = false)
	private Country countryOfCitizenshipDetails;

	@Column(name = "PRIMARY_TITLE")
	private String primaryTitle;

	@Column(name = "DIRECTORY_TITLE")
	private String directoryTitle;

	@Column(name = "HOME_UNIT")
	private String homeUnit;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_FK1"), name = "HOME_UNIT", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "IS_FACULTY")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isFaculty;

	@Column(name = "IS_GRADUATE_STUDENT_STAFF")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isGraduateStudentStaff;

	@Column(name = "IS_RESEARCH_STAFF")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isResearchStaff;

	@Column(name = "IS_SERVICE_STAFF")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isServiceStaff;

	@Column(name = "IS_SUPPORT_STAFF")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSupportStaff;

	@Column(name = "IS_OTHER_ACCADEMIC_GROUP")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isOtherAcadamic;

	@Column(name = "IS_MEDICAL_STAFF")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isMedicalStaff;

	@Column(name = "ADDRESS_LINE_1")
	private String addressLine1;

	@Column(name = "ADDRESS_LINE_2")
	private String addressLine2;

	@Column(name = "ADDRESS_LINE_3")
	private String addressLine3;

	@Column(name = "CITY")
	private String city;

	@Column(name = "COUNTY")
	private String country;

	@Column(name = "STATE")
	private String state;

	@Column(name = "POSTAL_CODE")
	private String postalCode;

	@Column(name = "COUNTRY_CODE")
	private String countryCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PERSON_COUNTRY_FK2"), name = "COUNTRY_CODE", referencedColumnName = "COUNTRY_CODE", insertable = false, updatable = false)
	private Country countryDetails;

	@Column(name = "FAX_NUMBER")
	private String faxNumber;

	@Column(name = "PAGER_NUMBER")
	private String pagerNumber;

	@Column(name = "MOBILE_PHONE_NUMBER")
	private String mobileNumber;

	@Column(name = "STATUS")
	private String status;

	@Column(name = "SALARY_ANNIVERSARY_DATE")
	private Timestamp salaryAnniversary;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonProperty(access = Access.WRITE_ONLY)
	@Column(name = "PASSWORD")
	private String password;

	@Column(name = "SUPERVISOR_PERSON_ID")
	private String supervisorPersonId;

	@Column(name = "ORCID_ID")
	private String orcidId;

	@Column(name = "IS_WEBHOOK_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isWebhookActive;

	@JsonProperty(access = Access.WRITE_ONLY)
	@Column(name = "ACCESS_TOKEN")
	private String accessToken;

	@Column(name = "DATE_WHEN_PERSON_INACTIVE")
	private Timestamp dateOfInactive;
	
	@Column(name = "IS_EXTERNAL_USER")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isExternalUser = false;

  @Column(name = "OFFICE_PHONE")
	private String officePhone;
  
	@Transient
	private Boolean isPasswordChange = false;

	@Transient
	private Boolean isUsernameChange = false;

	public Boolean getIsExternalUser() {
		return isExternalUser;
	}

	public void setIsExternalUser(Boolean isExternalUser) {
		this.isExternalUser = isExternalUser;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
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

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPriorName() {
		return priorName;
	}

	public void setPriorName(String priorName) {
		this.priorName = priorName;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public Timestamp getDateOfBirth() {
		return dateOfBirth;
	}

	public void setDateOfBirth(Timestamp dateOfBirth) {
		this.dateOfBirth = dateOfBirth;
	}

	public Integer getAge() {
		return age;
	}

	public void setAge(Integer age) {
		this.age = age;
	}

	public Integer getAgeByFiscalYear() {
		return ageByFiscalYear;
	}

	public void setAgeByFiscalYear(Integer ageByFiscalYear) {
		this.ageByFiscalYear = ageByFiscalYear;
	}

	public String getGender() {
		return gender;
	}

	public void setGender(String gender) {
		this.gender = gender;
	}

	public String getEducationLevel() {
		return educationLevel;
	}

	public void setEducationLevel(String educationLevel) {
		this.educationLevel = educationLevel;
	}

	public String getOfficeLocation() {
		return officeLocation;
	}

	public void setOfficeLocation(String officeLocation) {
		this.officeLocation = officeLocation;
	}

	public String getSecOfficeLocation() {
		return secOfficeLocation;
	}

	public void setSecOfficeLocation(String secOfficeLocation) {
		this.secOfficeLocation = secOfficeLocation;
	}

	public String getSecOfficePhone() {
		return secOfficePhone;
	}

	public void setSecOfficePhone(String secOfficePhone) {
		this.secOfficePhone = secOfficePhone;
	}

	public String getSchool() {
		return school;
	}

	public void setSchool(String school) {
		this.school = school;
	}

	public String getDirectoryDepartment() {
		return directoryDepartment;
	}

	public void setDirectoryDepartment(String directoryDepartment) {
		this.directoryDepartment = directoryDepartment;
	}

	public String getSalutation() {
		return salutation;
	}

	public void setSalutation(String salutation) {
		this.salutation = salutation;
	}

	public String getCountryOfCitizenshipCode() {
		return countryOfCitizenshipCode;
	}

	public void setCountryOfCitizenshipCode(String countryOfCitizenshipCode) {
		this.countryOfCitizenshipCode = countryOfCitizenshipCode;
	}

	public String getPrimaryTitle() {
		return primaryTitle;
	}

	public void setPrimaryTitle(String primaryTitle) {
		this.primaryTitle = primaryTitle;
	}

	public String getDirectoryTitle() {
		return directoryTitle;
	}

	public void setDirectoryTitle(String directoryTitle) {
		this.directoryTitle = directoryTitle;
	}

	public String getHomeUnit() {
		return homeUnit;
	}

	public void setHomeUnit(String homeUnit) {
		this.homeUnit = homeUnit;
	}

	public String getAddressLine1() {
		return addressLine1;
	}

	public void setAddressLine1(String addressLine1) {
		this.addressLine1 = addressLine1;
	}

	public String getAddressLine2() {
		return addressLine2;
	}

	public void setAddressLine2(String addressLine2) {
		this.addressLine2 = addressLine2;
	}

	public String getAddressLine3() {
		return addressLine3;
	}

	public void setAddressLine3(String addressLine3) {
		this.addressLine3 = addressLine3;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public String getFaxNumber() {
		return faxNumber;
	}

	public void setFaxNumber(String faxNumber) {
		this.faxNumber = faxNumber;
	}

	public String getPagerNumber() {
		return pagerNumber;
	}

	public void setPagerNumber(String pagerNumber) {
		this.pagerNumber = pagerNumber;
	}

	public String getMobileNumber() {
		return mobileNumber;
	}

	public void setMobileNumber(String mobileNumber) {
		this.mobileNumber = mobileNumber;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
		if (this.status.equals("A")) {
			this.setDateOfInactive(null);
		}
	}

	public Timestamp getSalaryAnniversary() {
		return salaryAnniversary;
	}

	public void setSalaryAnniversary(Timestamp salaryAnniversary) {
		this.salaryAnniversary = salaryAnniversary;
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

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getPrincipalName() {
		return principalName;
	}

	public void setPrincipalName(String principalName) {
		this.principalName = principalName;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public Boolean getIsFaculty() {
		return isFaculty;
	}

	public void setIsFaculty(Boolean isFaculty) {
		this.isFaculty = isFaculty;
	}

	public Boolean getIsGraduateStudentStaff() {
		return isGraduateStudentStaff;
	}

	public void setIsGraduateStudentStaff(Boolean isGraduateStudentStaff) {
		this.isGraduateStudentStaff = isGraduateStudentStaff;
	}

	public Boolean getIsResearchStaff() {
		return isResearchStaff;
	}

	public void setIsResearchStaff(Boolean isResearchStaff) {
		this.isResearchStaff = isResearchStaff;
	}

	public Boolean getIsServiceStaff() {
		return isServiceStaff;
	}

	public void setIsServiceStaff(Boolean isServiceStaff) {
		this.isServiceStaff = isServiceStaff;
	}

	public Boolean getIsSupportStaff() {
		return isSupportStaff;
	}

	public void setIsSupportStaff(Boolean isSupportStaff) {
		this.isSupportStaff = isSupportStaff;
	}

	public Boolean getIsOtherAcadamic() {
		return isOtherAcadamic;
	}

	public void setIsOtherAcadamic(Boolean isOtherAcadamic) {
		this.isOtherAcadamic = isOtherAcadamic;
	}

	public Boolean getIsMedicalStaff() {
		return isMedicalStaff;
	}

	public void setIsMedicalStaff(Boolean isMedicalStaff) {
		this.isMedicalStaff = isMedicalStaff;
	}

	public String getSupervisorPersonId() {
		return supervisorPersonId;
	}

	public void setSupervisorPersonId(String supervisorPersonId) {
		this.supervisorPersonId = supervisorPersonId;
	}

	public Boolean getIsPasswordChange() {
		return isPasswordChange;
	}

	public void setIsPasswordChange(Boolean isPasswordChange) {
		this.isPasswordChange = isPasswordChange;
	}

	public Boolean getIsUsernameChange() {
		return isUsernameChange;
	}

	public void setIsUsernameChange(Boolean isUsernameChange) {
		this.isUsernameChange = isUsernameChange;
	}

	public String getOrcidId() {
		return orcidId;
	}

	public void setOrcidId(String orcidId) {
		this.orcidId = orcidId;
	}

	public Boolean getIsWebhookActive() {
		return isWebhookActive;
	}

	public void setIsWebhookActive(Boolean isWebhookActive) {
		this.isWebhookActive = isWebhookActive;
	}

	public String getAccessToken() {
		return accessToken;
	}

	public void setAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}

	public Timestamp getDateOfInactive() {
		return dateOfInactive;
	}

	public void setDateOfInactive(Timestamp dateOfInactive) {
		this.dateOfInactive = dateOfInactive;
	}
	
	public String getOfficePhone() {
		return officePhone;
	}

	public void setOfficePhone(String officePhone) {
		this.officePhone = officePhone;
	}
    
	public Country getCountryOfCitizenshipDetails() {
		return countryOfCitizenshipDetails;
	}

	public void setCountryOfCitizenshipDetails(Country countryOfCitizenshipDetails) {
		this.countryOfCitizenshipDetails = countryOfCitizenshipDetails;
	}

	public Country getCountryDetails() {
		return countryDetails;
	}

	public void setCountryDetails(Country countryDetails) {
		this.countryDetails = countryDetails;
	}
    
}
