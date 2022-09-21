package com.polus.fibicomp.externalreviewer.pojo;

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
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import com.polus.fibicomp.pojo.Country;

@Entity
@Table(name = "EXTERNAL_REVIEWER")
public class ExternalReviewer implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EXTERNAL_REVIEWER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer externalReviewerId;

	@Column(name = "LAST_NAME")
	private String lastName;

	@Column(name = "FIRST_NAME")
	private String firstName;

	@Column(name = "MIDDLE_NAME")
	private String middleName;

	@Column(name = "FULL_NAME")
	private String passportName;

	@Column(name = "GENDER")
	private String gender;

	@Column(name = "EMAIL_ADDRESS_PRIMARY")
	private String primaryEmail;

	@Column(name = "EMAIL_ADDRESS_SECONDARY")
	private String secondaryEmail;

	@Column(name = "COUNTRY_CODE")
	private String countryCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_FK1"), name = "COUNTRY_CODE", referencedColumnName = "COUNTRY_CODE", insertable = false, updatable = false)
	private Country countryDetails;

	@Column(name = "ACADEMIC_RANK_CODE")
	private String academicRankCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_FK2"), name = "ACADEMIC_RANK_CODE", referencedColumnName = "ACADEMIC_RANK_CODE", insertable = false, updatable = false)
	private ExtReviewerAcademicRank academicRank;

	@Column(name = "AFFILATION_INSTITUITION_CODE")
	private String affilationInstitutionCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_FK3"), name = "AFFILATION_INSTITUITION_CODE", referencedColumnName = "AFFILATION_INSTITUITION_CODE", insertable = false, updatable = false)
	private ExtReviewerAffilation affilationInstitution;

	@Column(name = "IS_TOP_INSTITUITION")
	private String isTopInstitution;

	@Column(name = "AGREEMENT_START_DATE")
	private Timestamp agreementStartDate;

	@Column(name = "AGREEMENT_END_DATE")
	private Timestamp agreementEndDate;

	@Column(name = "ACADEMIC_AREA_CODE_SECONDARY")
	private String academicAreaCodeSecondary;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_FK4"), name = "ACADEMIC_AREA_CODE_SECONDARY", referencedColumnName = "ACADEMIC_AREA_CODE", insertable = false, updatable = false)
	private ExtReviewerAcademicArea academicAreaSecondary;

	@Column(name = "ACADEMIC_AREA_CODE_PRIMARY")
	private String academicAreaCodePrimary;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_FK5"), name = "ACADEMIC_AREA_CODE_PRIMARY", referencedColumnName = "ACADEMIC_AREA_CODE", insertable = false, updatable = false)
	private ExtReviewerAcademicArea academicAreaPrimary;
	
	@JsonIgnore
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonIgnore
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "STATUS")
	private String status;
	
	@Column(name = "DEPARTMENT")
	private String department;

	@JsonProperty(access = Access.WRITE_ONLY)
	@Column(name = "PASSWORD")
	private String password;
	
	@Column(name = "USER_NAME", unique = true, nullable = false)
	private String principalName;

	@Transient
	private Boolean isUsernameChange = false;

	public Integer getExternalReviewerId() {
		return externalReviewerId;
	}

	public void setExternalReviewerId(Integer externalReviewerId) {
		this.externalReviewerId = externalReviewerId;
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

	public String getPassportName() {
		return passportName;
	}

	public void setPassportName(String passportName) {
		this.passportName = passportName;
	}

	public String getGender() {
		return gender;
	}

	public void setGender(String gender) {
		this.gender = gender;
	}

	public String getPrimaryEmail() {
		return primaryEmail;
	}

	public void setPrimaryEmail(String primaryEmail) {
		this.primaryEmail = primaryEmail;
	}

	public String getSecondaryEmail() {
		return secondaryEmail;
	}

	public void setSecondaryEmail(String secondaryEmail) {
		this.secondaryEmail = secondaryEmail;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public Country getCountryDetails() {
		return countryDetails;
	}

	public void setCountryDetails(Country countryDetails) {
		this.countryDetails = countryDetails;
	}

	public String getAcademicRankCode() {
		return academicRankCode;
	}

	public void setAcademicRankCode(String academicRankCode) {
		this.academicRankCode = academicRankCode;
	}

	public ExtReviewerAcademicRank getAcademicRank() {
		return academicRank;
	}

	public void setAcademicRank(ExtReviewerAcademicRank academicRank) {
		this.academicRank = academicRank;
	}

	public String getAffilationInstitutionCode() {
		return affilationInstitutionCode;
	}

	public void setAffilationInstitutionCode(String affilationInstitutionCode) {
		this.affilationInstitutionCode = affilationInstitutionCode;
	}

	public ExtReviewerAffilation getAffilationInstitution() {
		return affilationInstitution;
	}

	public void setAffilationInstitution(ExtReviewerAffilation affilationInstitution) {
		this.affilationInstitution = affilationInstitution;
	}

	public String getIsTopInstitution() {
		return isTopInstitution;
	}

	public void setIsTopInstitution(String isTopInstitution) {
		this.isTopInstitution = isTopInstitution;
	}

	public Timestamp getAgreementStartDate() {
		return agreementStartDate;
	}

	public void setAgreementStartDate(Timestamp agreementStartDate) {
		this.agreementStartDate = agreementStartDate;
	}

	public Timestamp getAgreementEndDate() {
		return agreementEndDate;
	}

	public void setAgreementEndDate(Timestamp agreementEndDate) {
		this.agreementEndDate = agreementEndDate;
	}

	public String getAcademicAreaCodeSecondary() {
		return academicAreaCodeSecondary;
	}

	public void setAcademicAreaCodeSecondary(String academicAreaCodeSecondary) {
		this.academicAreaCodeSecondary = academicAreaCodeSecondary;
	}

	public ExtReviewerAcademicArea getAcademicAreaSecondary() {
		return academicAreaSecondary;
	}

	public void setAcademicAreaSecondary(ExtReviewerAcademicArea academicAreaSecondary) {
		this.academicAreaSecondary = academicAreaSecondary;
	}

	public String getAcademicAreaCodePrimary() {
		return academicAreaCodePrimary;
	}

	public void setAcademicAreaCodePrimary(String academicAreaCodePrimary) {
		this.academicAreaCodePrimary = academicAreaCodePrimary;
	}

	public ExtReviewerAcademicArea getAcademicAreaPrimary() {
		return academicAreaPrimary;
	}

	public void setAcademicAreaPrimary(ExtReviewerAcademicArea academicAreaPrimary) {
		this.academicAreaPrimary = academicAreaPrimary;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
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

	public Boolean getIsUsernameChange() {
		return isUsernameChange;
	}

	public void setIsUsernameChange(Boolean isUsernameChange) {
		this.isUsernameChange = isUsernameChange;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

}
