package com.polus.entity;

import java.io.Serializable;
import java.sql.Timestamp;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonProperty.Access;
import com.polus.config.JpaCharBooleanConversion;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ForeignKey;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
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
//	@Convert(converter = JpaCharBooleanConversion.class)
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

    @Column(name = "OFFICE_PHONE")
	private String officePhone;

	@Column(name = "IS_MFA_ENABLED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isMfaEnabled = false;

	@Column(name = "SECRET")
	private String secret;

	@Column(name = "IS_EXTERNAL_USER")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isExternalUser = false;

}
