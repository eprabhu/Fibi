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
@Table(name = "WORKDAY_JOB_PROFILE_CHANGE")
public class WorkdayJobProfileChange implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKDAY_JOB_PROFILE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_WORKDAY_JOB_PROFILE_ID_GENERATOR")
	@SequenceGenerator(name = "SEQ_WORKDAY_JOB_PROFILE_ID_GENERATOR", sequenceName = "SEQ_WORKDAY_JOB_PROFILE_ID_GENERATOR", allocationSize = 1)
	private Integer workdayJobProfileId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "JOB_CODE_CURRENT")
	private String jobCodeCurrent;

	@Column(name = "EFFECTIVE_DATE")
	private String effectiveDate;

	@Column(name = "JOB_CODE_PROPOSED")
	private String jobCodeProposed;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "TRIGGER_DATE")
	private Timestamp triggerDate;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getJobCodeCurrent() {
		return jobCodeCurrent;
	}

	public void setJobCodeCurrent(String jobCodeCurrent) {
		this.jobCodeCurrent = jobCodeCurrent;
	}

	public String getEffectiveDate() {
		return effectiveDate;
	}

	public void setEffectiveDate(String effectiveDate) {
		this.effectiveDate = effectiveDate;
	}

	public String getJobCodeProposed() {
		return jobCodeProposed;
	}

	public void setJobCodeProposed(String jobCodeProposed) {
		this.jobCodeProposed = jobCodeProposed;
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

	public Timestamp getTriggerDate() {
		return triggerDate;
	}

	public void setTriggerDate(Timestamp triggerDate) {
		this.triggerDate = triggerDate;
	}

	public Integer getWorkdayJobProfileId() {
		return workdayJobProfileId;
	}

	public void setWorkdayJobProfileId(Integer workdayJobProfileId) {
		this.workdayJobProfileId = workdayJobProfileId;
	}

}
