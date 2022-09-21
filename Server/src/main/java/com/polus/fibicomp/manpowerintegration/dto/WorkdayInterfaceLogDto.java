package com.polus.fibicomp.manpowerintegration.dto;

import java.math.BigDecimal;
import java.util.Date;

public class WorkdayInterfaceLogDto {

	private Integer awardId;

	private Integer workdayManpowerInterfaceId;

	private String awardTitle;

	private String errorMessage;

	private String messageType;

	private String interfaceTypeCode;

	private String interfaceTypeName;

	private String comments;

	private String userActionCode;

	private String awardNumber;

	private Integer manpowerLogId;

	private String userActionName;

	private Date planStartDate;

	private Date planEndDate;

	private Date chargeStartDate;

	private Date chargeEndDate;

	private String positionOwnedByAward;

	private BigDecimal costAllocation;

	private String positionStatus;

	private String personFullName;

	private String positionId;

	private String personId;

	private String jobProfileType;

	private String budgetReferenceNumber;

	private String interfaceStatusCode;

	private String interfaceStatusName;

	private String resourceUniqueId;

	private String resourceName;

	public WorkdayInterfaceLogDto(Integer awardId, Integer workdayManpowerInterfaceId, String awardTitle,
			String errorMessage, String messageType, String interfaceTypeCode, String interfaceTypeName,
			String comments, String userActionCode, String awardNumber, Integer manpowerLogId, String userActionName) {
		super();
		this.awardId = awardId;
		this.workdayManpowerInterfaceId = workdayManpowerInterfaceId;
		this.awardTitle = awardTitle;
		this.errorMessage = errorMessage;
		this.messageType = messageType;
		this.interfaceTypeCode = interfaceTypeCode;
		this.interfaceTypeName = interfaceTypeName;
		this.comments = comments;
		this.userActionCode = userActionCode;
		this.awardNumber = awardNumber;
		this.manpowerLogId = manpowerLogId;
		this.userActionName = userActionName;
	}

	public WorkdayInterfaceLogDto(String awardNumber, Date planStartDate, Date planEndDate, Date chargeStartDate,
			Date chargeEndDate, String positionOwnedByAward, BigDecimal costAllocation, String positionStatus,
			String personFullName, String positionId, String personId, String jobProfileType,
			String budgetReferenceNumber) {
		super();
		this.awardNumber = awardNumber;
		this.planStartDate = planStartDate;
		this.planEndDate = planEndDate;
		this.chargeStartDate = chargeStartDate;
		this.chargeEndDate = chargeEndDate;
		this.positionOwnedByAward = positionOwnedByAward;
		this.costAllocation = costAllocation;
		this.positionStatus = positionStatus;
		this.personFullName = personFullName;
		this.positionId = positionId;
		this.personId = personId;
		this.jobProfileType = jobProfileType;
		this.budgetReferenceNumber = budgetReferenceNumber;
	}

	public WorkdayInterfaceLogDto() {
		// TODO Auto-generated constructor stub
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Integer getWorkdayManpowerInterfaceId() {
		return workdayManpowerInterfaceId;
	}

	public void setWorkdayManpowerInterfaceId(Integer workdayManpowerInterfaceId) {
		this.workdayManpowerInterfaceId = workdayManpowerInterfaceId;
	}

	public String getAwardTitle() {
		return awardTitle;
	}

	public void setAwardTitle(String awardTitle) {
		this.awardTitle = awardTitle;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getMessageType() {
		return messageType;
	}

	public void setMessageType(String messageType) {
		this.messageType = messageType;
	}

	public String getInterfaceTypeCode() {
		return interfaceTypeCode;
	}

	public void setInterfaceTypeCode(String interfaceTypeCode) {
		this.interfaceTypeCode = interfaceTypeCode;
	}

	public String getInterfaceTypeName() {
		return interfaceTypeName;
	}

	public void setInterfaceTypeName(String interfaceTypeName) {
		this.interfaceTypeName = interfaceTypeName;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getUserActionCode() {
		return userActionCode;
	}

	public void setUserActionCode(String userActionCode) {
		this.userActionCode = userActionCode;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getManpowerLogId() {
		return manpowerLogId;
	}

	public void setManpowerLogId(Integer manpowerLogId) {
		this.manpowerLogId = manpowerLogId;
	}

	public String getUserActionName() {
		return userActionName;
	}

	public void setUserActionName(String userActionName) {
		this.userActionName = userActionName;
	}

	public Date getPlanStartDate() {
		return planStartDate;
	}

	public void setPlanStartDate(Date planStartDate) {
		this.planStartDate = planStartDate;
	}

	public Date getPlanEndDate() {
		return planEndDate;
	}

	public void setPlanEndDate(Date planEndDate) {
		this.planEndDate = planEndDate;
	}

	public Date getChargeStartDate() {
		return chargeStartDate;
	}

	public void setChargeStartDate(Date chargeStartDate) {
		this.chargeStartDate = chargeStartDate;
	}

	public Date getChargeEndDate() {
		return chargeEndDate;
	}

	public void setChargeEndDate(Date chargeEndDate) {
		this.chargeEndDate = chargeEndDate;
	}

	public String getPositionOwnedByAward() {
		return positionOwnedByAward;
	}

	public void setPositionOwnedByAward(String positionOwnedByAward) {
		this.positionOwnedByAward = positionOwnedByAward;
	}

	public BigDecimal getCostAllocation() {
		return costAllocation;
	}

	public void setCostAllocation(BigDecimal costAllocation) {
		this.costAllocation = costAllocation;
	}

	public String getPositionStatus() {
		return positionStatus;
	}

	public void setPositionStatus(String positionStatus) {
		this.positionStatus = positionStatus;
	}

	public String getPersonFullName() {
		return personFullName;
	}

	public void setPersonFullName(String personFullName) {
		this.personFullName = personFullName;
	}

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getJobProfileType() {
		return jobProfileType;
	}

	public void setJobProfileType(String jobProfileType) {
		this.jobProfileType = jobProfileType;
	}

	public String getBudgetReferenceNumber() {
		return budgetReferenceNumber;
	}

	public void setBudgetReferenceNumber(String budgetReferenceNumber) {
		this.budgetReferenceNumber = budgetReferenceNumber;
	}

	public String getInterfaceStatusCode() {
		return interfaceStatusCode;
	}

	public void setInterfaceStatusCode(String interfaceStatusCode) {
		this.interfaceStatusCode = interfaceStatusCode;
	}

	public String getInterfaceStatusName() {
		return interfaceStatusName;
	}

	public void setInterfaceStatusName(String interfaceStatusName) {
		this.interfaceStatusName = interfaceStatusName;
	}

	public String getResourceUniqueId() {
		return resourceUniqueId;
	}

	public void setResourceUniqueId(String resourceUniqueId) {
		this.resourceUniqueId = resourceUniqueId;
	}

	public String getResourceName() {
		return resourceName;
	}

	public void setResourceName(String resourceName) {
		this.resourceName = resourceName;
	}

}
