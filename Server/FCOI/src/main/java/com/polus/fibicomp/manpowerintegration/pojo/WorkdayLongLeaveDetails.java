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
@Table(name = "WORKDAY_LONG_LEAVE_DETAILS")
public class WorkdayLongLeaveDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "LONG_LEAVE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_LONG_LEAVE_ID_GENERATOR")
	@SequenceGenerator(name = "SEQ_LONG_LEAVE_ID_GENERATOR", sequenceName = "SEQ_LONG_LEAVE_ID_GENERATOR", allocationSize = 1)
	private Integer workdayLongLeaveId;

	@Column(name = "PERSON_ID")
	private String personId;
	
	@Column(name = "NAME")
	private String name;
	
	@Column(name = "JOB_FAMILY")
	private String jobFamily;
	
	@Column(name = "FIRST_DAY_OF_LEAVE")
	private Timestamp firstDayOfLeave;
	
	@Column(name = "INITIATED")
	private Timestamp initiated;
	
	@Column(name = "TITLE")
	private String title;
	
	@Column(name = "NTU_DEPARTMENT")
	private String nTUDepartment;
	
	@Column(name = "LAST_DAY_OF_LEAVE_ESTIMATED")
	private Timestamp lastDayOfLeaveEstimated;
	
	@Column(name = "LAST_DAY_OF_LEAVE_ACTUAL")
	private Timestamp lastDayOfLeaveActual;
	
	@Column(name = "APPROVAL_DATE")
	private Timestamp approvalDate;
	
	@Column(name = "AU")
	private String aU;
	
	@Column(name = "BUSINESS_TITLE")
	private String businessTitle;
	
	@Column(name = "JOB_FAMILY_GROUP")
	private String jobFamilyGroup;
	
	@Column(name = "JOB_PROFILE")
	private String jobProfile;
	
	@Column(name = "LEAVE_TYPE_EXCLUDING_FAMILY")
	private String leaveTypeExcludingFamily;
	
	@Column(name = "SUPERVISORY_ORGANIZATION")
	private String supervisoryOrganization;
	
	@Column(name = "STATUS")
	private String status;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "TRIGGER_DATE")
	private	Timestamp triggerDate;
	
	@Column(name = "UNIQUE_INITIATED")
	private String uniqueInitiated;

	public String getUniqueInitiated() {
		return uniqueInitiated;
	}

	public void setUniqueInitiated(String uniqueInitiated) {
		this.uniqueInitiated = uniqueInitiated;
	}

	public Timestamp getInitiated() {
		return initiated;
	}

	public void setInitiated(Timestamp initiated) {
		this.initiated = initiated;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getJobFamily() {
		return jobFamily;
	}

	public void setJobFamily(String jobFamily) {
		this.jobFamily = jobFamily;
	}

	public Timestamp getFirstDayOfLeave() {
		return firstDayOfLeave;
	}

	public void setFirstDayOfLeave(Timestamp firstDayOfLeave) {
		this.firstDayOfLeave = firstDayOfLeave;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getnTUDepartment() {
		return nTUDepartment;
	}

	public void setnTUDepartment(String nTUDepartment) {
		this.nTUDepartment = nTUDepartment;
	}

	public Timestamp getLastDayOfLeaveEstimated() {
		return lastDayOfLeaveEstimated;
	}

	public void setLastDayOfLeaveEstimated(Timestamp lastDayOfLeaveEstimated) {
		this.lastDayOfLeaveEstimated = lastDayOfLeaveEstimated;
	}

	public Timestamp getLastDayOfLeaveActual() {
		return lastDayOfLeaveActual;
	}

	public void setLastDayOfLeaveActual(Timestamp lastDayOfLeaveActual) {
		this.lastDayOfLeaveActual = lastDayOfLeaveActual;
	}

	public Timestamp getApprovalDate() {
		return approvalDate;
	}

	public void setApprovalDate(Timestamp approvalDate) {
		this.approvalDate = approvalDate;
	}

	public String getaU() {
		return aU;
	}

	public void setaU(String aU) {
		this.aU = aU;
	}

	public String getBusinessTitle() {
		return businessTitle;
	}

	public void setBusinessTitle(String businessTitle) {
		this.businessTitle = businessTitle;
	}

	public String getJobFamilyGroup() {
		return jobFamilyGroup;
	}

	public void setJobFamilyGroup(String jobFamilyGroup) {
		this.jobFamilyGroup = jobFamilyGroup;
	}

	public String getJobProfile() {
		return jobProfile;
	}

	public void setJobProfile(String jobProfile) {
		this.jobProfile = jobProfile;
	}

	public String getLeaveTypeExcludingFamily() {
		return leaveTypeExcludingFamily;
	}

	public void setLeaveTypeExcludingFamily(String leaveTypeExcludingFamily) {
		this.leaveTypeExcludingFamily = leaveTypeExcludingFamily;
	}

	public String getSupervisoryOrganization() {
		return supervisoryOrganization;
	}

	public void setSupervisoryOrganization(String supervisoryOrganization) {
		this.supervisoryOrganization = supervisoryOrganization;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
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

	public Integer getWorkdayLongLeaveId() {
		return workdayLongLeaveId;
	}

	public void setWorkdayLongLeaveId(Integer workdayLongLeaveId) {
		this.workdayLongLeaveId = workdayLongLeaveId;
	}

}
