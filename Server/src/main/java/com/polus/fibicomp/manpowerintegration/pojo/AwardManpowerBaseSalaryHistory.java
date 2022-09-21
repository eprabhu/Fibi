package com.polus.fibicomp.manpowerintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "MANPOWER_BASE_SALARY_HISTORY")
public class AwardManpowerBaseSalaryHistory implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "BASE_SALARY_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_BASE_SALARY_HISTORY")
	@SequenceGenerator(name="SEQ_BASE_SALARY_HISTORY", sequenceName = "SEQ_BASE_SALARY_HISTORY", allocationSize=1)
	private Integer baseSalaryHistoryId;

	@Column(name = "RESOURCE_UNIQUE_ID")
	private String resourceUniqueId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "POSITION_ID")
	private String positionId;

	@Column(name = "CHARGE_START_DATE")
	private Timestamp chargeStartDate;

	@Column(name = "CHARGE_END_DATE")
	private Timestamp chargeEndDate;

	@Column(name = "CURRENT_BASE_SALARY")
	private String currentBaseSalary;

	@Column(name = "PREVIOUS_BASE_SALARY")
	private String previousBaseSalary;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getBaseSalaryHistoryId() {
		return baseSalaryHistoryId;
	}

	public void setBaseSalaryHistoryId(Integer baseSalaryHistoryId) {
		this.baseSalaryHistoryId = baseSalaryHistoryId;
	}

	public String getResourceUniqueId() {
		return resourceUniqueId;
	}

	public void setResourceUniqueId(String resourceUniqueId) {
		this.resourceUniqueId = resourceUniqueId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public Timestamp getChargeStartDate() {
		return chargeStartDate;
	}

	public void setChargeStartDate(Timestamp chargeStartDate) {
		this.chargeStartDate = chargeStartDate;
	}

	public Timestamp getChargeEndDate() {
		return chargeEndDate;
	}

	public void setChargeEndDate(Timestamp chargeEndDate) {
		this.chargeEndDate = chargeEndDate;
	}

	public String getCurrentBaseSalary() {
		return currentBaseSalary;
	}

	public void setCurrentBaseSalary(String currentBaseSalary) {
		this.currentBaseSalary = currentBaseSalary;
	}

	public String getPreviousBaseSalary() {
		return previousBaseSalary;
	}

	public void setPreviousBaseSalary(String previousBaseSalary) {
		this.previousBaseSalary = previousBaseSalary;
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

}
