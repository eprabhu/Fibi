package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name = "MANPOWER")
public class Manpower implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PERSON_ID")
	private String manpowerPersonId;

	@Column(name = "POSITION_ID")
	private String positionId;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "NATIONALITY")
	private String nationality;

	@Column(name = "CITIZENSHIP")
	private String citizenship;

	@Column(name = "CADIDATURE_START_DATE")
	private Timestamp candidatureStartDate;

	@Column(name = "CADIDATURE_END_DATE")
	private Timestamp candidatureEndDate;

	@Column(name = "CONTRACT_END_DATE")
	private Timestamp contractEndDate;

	@Column(name = "HIRE_DATE")
	private Timestamp hireDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "FILE_ID")
	private Integer fileId;

	@Column(name = "BASE_SALARY")
	private String baseSalary;

	@Column(name = "JOB_CODE")
	private String jobCode;

	@Transient
	private String decryptedNationality;
	
	@Transient
	private String decryptedCitizenShip;

	public Manpower() {

	}

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
	}

	public String getBaseSalary() {
		return baseSalary;
	}

	public void setBaseSalary(String baseSalary) {
		this.baseSalary = baseSalary;
	}

	public String getManpowerPersonId() {
		return manpowerPersonId;
	}

	public void setManpowerPersonId(String manpowerPersonId) {
		this.manpowerPersonId = manpowerPersonId;
	}

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getNationality() {
		return nationality;
	}

	public void setNationality(String nationality) {
		this.nationality = nationality;
	}

	public String getCitizenship() {
		return citizenship;
	}

	public void setCitizenship(String citizenship) {
		this.citizenship = citizenship;
	}

	public Timestamp getCandidatureStartDate() {
		return candidatureStartDate;
	}

	public void setCandidatureStartDate(Timestamp candidatureStartDate) {
		this.candidatureStartDate = candidatureStartDate;
	}

	public Timestamp getCandidatureEndDate() {
		return candidatureEndDate;
	}

	public void setCandidatureEndDate(Timestamp candidatureEndDate) {
		this.candidatureEndDate = candidatureEndDate;
	}

	public Timestamp getHireDate() {
		return hireDate;
	}

	public void setHireDate(Timestamp hireDate) {
		this.hireDate = hireDate;
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

	public Timestamp getContractEndDate() {
		return contractEndDate;
	}

	public void setContractEndDate(Timestamp contractEndDate) {
		this.contractEndDate = contractEndDate;
	}

	public String getDecryptedNationality() {
		return decryptedNationality;
	}

	public void setDecryptedNationality(String decryptedNationality) {
		this.decryptedNationality = decryptedNationality;
	}

	public String getDecryptedCitizenShip() {
		return decryptedCitizenShip;
	}

	public void setDecryptedCitizenShip(String decryptedCitizenShip) {
		this.decryptedCitizenShip = decryptedCitizenShip;
	}

	public String getJobCode() {
		return jobCode;
	}

	public void setJobCode(String jobCode) {
		this.jobCode = jobCode;
	}

}
