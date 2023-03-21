package com.polus.fibicomp.integration.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.award.pojo.Publication;
import com.polus.fibicomp.integration.pojo.AwardHoursLogRT;
import com.polus.fibicomp.integration.pojo.ExpenseTrackerRT;
import com.polus.fibicomp.task.pojo.Task;

public class IntegrationVO {

	private AwardHoursLogRT awardHoursLogRT;

	private List<AwardHoursLogRT> awardHoursLogRTs;

	private List<String> messages;

	private List<ExpenseTrackerRT> expenseTrackerRTs;

	private List<Publication> publications;

//	The variables below are used for data loading of proposal and award

	/**
	 * Seed award Id - id of award, from which we need to make new awards
	 */
	private Integer awardId;

	/**
	 * Title of award
	 */
	private String title;

	private String updateUser;

	/**
	 * Specify if we need to create active award
	 */
	private Boolean active;

	/**
	 * Id of PI
	 */
	private String personId;

	/**
	 * Lead unit for the award
	 */
	private String unitNumber;

	private String personName;

	private Task task;

	/**
	 * List of task to generate in award
	 */
	private List<String> taskTypeCodes;
	
	/**
	 * Specify task is active or not
	 */
	private Boolean taskTypeActive;

	/**
	 * Scenario to understand the condition to we need to create the award
	 */
	private String scenario;

	/**
	 * To know whether it is pi feed or not
	 */
	private Boolean piFeed;

	private Integer loaTaskId;

	private Integer ethicstaskId;

	/**
	 * Seed proposal Id
	 */
	private Integer proposalId;

	/**
	 * To know whether to create variation or not
	 */
	private Boolean createVariationRequest;

	/**
	 * To specify the service request type
	 */
	private String serviceRequestTypeCode;

	public IntegrationVO() {
		messages = new ArrayList<String>();
	}

	public AwardHoursLogRT getAwardHoursLogRT() {
		return awardHoursLogRT;
	}

	public void setAwardHoursLogRT(AwardHoursLogRT awardHoursLogRT) {
		this.awardHoursLogRT = awardHoursLogRT;
	}

	public List<AwardHoursLogRT> getAwardHoursLogRTs() {
		return awardHoursLogRTs;
	}

	public void setAwardHoursLogRTs(List<AwardHoursLogRT> awardHoursLogRTs) {
		this.awardHoursLogRTs = awardHoursLogRTs;
	}

	public List<String> getMessages() {
		return messages;
	}

	public void setMessages(List<String> messages) {
		this.messages = messages;
	}

	public List<ExpenseTrackerRT> getExpenseTrackerRTs() {
		return expenseTrackerRTs;
	}

	public void setExpenseTrackerRTs(List<ExpenseTrackerRT> expenseTrackerRTs) {
		this.expenseTrackerRTs = expenseTrackerRTs;
	}

	public List<Publication> getPublications() {
		return publications;
	}

	public void setPublications(List<Publication> publications) {
		this.publications = publications;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Boolean getActive() {
		return active;
	}

	public void setActive(Boolean active) {
		this.active = active;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public Task getTask() {
		return task;
	}

	public void setTask(Task task) {
		this.task = task;
	}

	public List<String> getTaskTypeCodes() {
		return taskTypeCodes;
	}

	public void setTaskTypeCodes(List<String> taskTypeCodes) {
		this.taskTypeCodes = taskTypeCodes;
	}

	public Boolean getTaskTypeActive() {
		return taskTypeActive;
	}

	public void setTaskTypeActive(Boolean taskTypeActive) {
		this.taskTypeActive = taskTypeActive;
	}

	public String getScenario() {
		return scenario;
	}

	public void setScenario(String scenario) {
		this.scenario = scenario;
	}

	public Boolean getPiFeed() {
		return piFeed;
	}

	public void setPiFeed(Boolean piFeed) {
		this.piFeed = piFeed;
	}

	public Integer getLoaTaskId() {
		return loaTaskId;
	}

	public void setLoaTaskId(Integer loaTaskId) {
		this.loaTaskId = loaTaskId;
	}

	public Integer getEthicstaskId() {
		return ethicstaskId;
	}

	public void setEthicstaskId(Integer ethicstaskId) {
		this.ethicstaskId = ethicstaskId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Boolean getCreateVariationRequest() {
		return createVariationRequest;
	}

	public void setCreateVariationRequest(Boolean createVariationRequest) {
		this.createVariationRequest = createVariationRequest;
	}

	public String getServiceRequestTypeCode() {
		return serviceRequestTypeCode;
	}

	public void setServiceRequestTypeCode(String serviceRequestTypeCode) {
		this.serviceRequestTypeCode = serviceRequestTypeCode;
	}

}
