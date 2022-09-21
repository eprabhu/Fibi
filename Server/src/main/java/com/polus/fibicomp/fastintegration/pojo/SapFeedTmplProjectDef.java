package com.polus.fibicomp.fastintegration.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SAP_FEED_TMPL_PROJECT_DEF")
@EntityListeners(AuditingEntityListener.class)
public class SapFeedTmplProjectDef {

	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_FEED_TMPL_GRANT_BUD_MASTER_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_FEED_TMPL_GRANT_BUD_MASTER_ID_GENERATOR", sequenceName = "SAP_FEED_TMPL_GRANT_BUD_MASTER_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "FEED_ID")
	private Integer feedId;
	
	@Column(name = "PROJECT_DEFINITION")
	private String projectDefinition;
	
	@Column(name = "PROJECT_PROFILE")
	private String projectProfile;
	
	@Column(name = "SHORT_DESCRIPTION")
	private String shortDescription;
	
	@Column(name = "NUM_OF_THE_RESPONSIBLE_PERSON")
	private String numOfTheResponsiblePerson;
	
	@Column(name = "CONTROLLING_AREA")
	private String controllingArea;
	
	@Column(name = "COMPANY_CODE")
	private String companyCode;
	
	@Column(name = "BUSINESS_AREA")
	private String businessArea;
	
	@Column(name = "PLANT")
	private String plant;
	
	@Column(name = "FUNCTIONAL_AREA")
	private String functionalArea;
	
	@Column(name = "PROFIT_CENTER")
	private String profitCenter;
	
	@Column(name = "PROJECT_CURRENCY")
	private String projectCurrency;
	
	@Column(name = "START_DATE")
	private Timestamp startDate;
	
	@Column(name = "FINISH_DATE")
	private Timestamp finishDate;
	
	@Column(name = "FACTORY_CALENDER_KEY")
	private String factoryCalenderKey;
	
	@Column(name = "BUDGET_PROFILE")
	private String budgetrofile;
	
	@Column(name = "PLANNING_PROFILE")
	private String planningProfile;
	
	@Column(name = "AWARD_STATUS")
	private String awardStatus;
	
	@Column(name = "CREATE_STATUS")
	private String createStatus;

	@Column(name = "FEED_STATUS")
	private String feedStatus;

	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "USER_COMMENT")
	private String userComment;

	public SapFeedTmplProjectDef() {}
	
	public SapFeedTmplProjectDef(String projectDefinition, Integer batchId, String feedStatus, String message) {
		this.projectDefinition = projectDefinition;
		this.batchId = batchId;
		this.feedStatus = feedStatus;
		this.errorMessage = message;
	}

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public Integer getFeedId() {
		return feedId;
	}

	public void setFeedId(Integer feedId) {
		this.feedId = feedId;
	}

	public String getProjectDefinition() {
		return projectDefinition;
	}

	public void setProjectDefinition(String projectDefinition) {
		this.projectDefinition = projectDefinition;
	}

	public String getProjectProfile() {
		return projectProfile;
	}

	public void setProjectProfile(String projectProfile) {
		this.projectProfile = projectProfile;
	}

	public String getShortDescription() {
		return shortDescription;
	}

	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}

	public String getNumOfTheResponsiblePerson() {
		return numOfTheResponsiblePerson;
	}

	public void setNumOfTheResponsiblePerson(String numOfTheResponsiblePerson) {
		this.numOfTheResponsiblePerson = numOfTheResponsiblePerson;
	}

	public String getControllingArea() {
		return controllingArea;
	}

	public void setControllingArea(String controllingArea) {
		this.controllingArea = controllingArea;
	}

	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getBusinessArea() {
		return businessArea;
	}

	public void setBusinessArea(String businessArea) {
		this.businessArea = businessArea;
	}

	public String getPlant() {
		return plant;
	}

	public void setPlant(String plant) {
		this.plant = plant;
	}

	public String getFunctionalArea() {
		return functionalArea;
	}

	public void setFunctionalArea(String functionalArea) {
		this.functionalArea = functionalArea;
	}

	public String getProfitCenter() {
		return profitCenter;
	}

	public void setProfitCenter(String profitCenter) {
		this.profitCenter = profitCenter;
	}

	public String getProjectCurrency() {
		return projectCurrency;
	}

	public void setProjectCurrency(String projectCurrency) {
		this.projectCurrency = projectCurrency;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getFinishDate() {
		return finishDate;
	}

	public void setFinishDate(Timestamp finishDate) {
		this.finishDate = finishDate;
	}

	public String getFactoryCalenderKey() {
		return factoryCalenderKey;
	}

	public void setFactoryCalenderKey(String factoryCalenderKey) {
		this.factoryCalenderKey = factoryCalenderKey;
	}

	public String getBudgetrofile() {
		return budgetrofile;
	}

	public void setBudgetrofile(String budgetrofile) {
		this.budgetrofile = budgetrofile;
	}

	public String getPlanningProfile() {
		return planningProfile;
	}

	public void setPlanningProfile(String planningProfile) {
		this.planningProfile = planningProfile;
	}

	public String getAwardStatus() {
		return awardStatus;
	}

	public void setAwardStatus(String awardStatus) {
		this.awardStatus = awardStatus;
	}

	public String getCreateStatus() {
		return createStatus;
	}

	public void setCreateStatus(String createStatus) {
		this.createStatus = createStatus;
	}

	public String getFeedStatus() {
		return feedStatus;
	}

	public void setFeedStatus(String feedStatus) {
		this.feedStatus = feedStatus;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
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

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}
	
}
