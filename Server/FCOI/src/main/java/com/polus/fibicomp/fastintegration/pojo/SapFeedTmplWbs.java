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
@Table(name = "SAP_FEED_TMPL_WBS")
@EntityListeners(AuditingEntityListener.class)
public class SapFeedTmplWbs {
	
	@Id
	@Column(name = "ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_FEED_TMPL_WBS_ID_GENERATOR")
	@SequenceGenerator(name = "SAP_FEED_TMPL_WBS_ID_GENERATOR", sequenceName = "SAP_FEED_TMPL_WBS_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "BATCH_ID")
	private Integer batchId;

	@Column(name = "FEED_ID")
	private Integer feedId;
	
	@Column(name = "WBS_ELEMENT")
	private String wbsElement;
	
	@Column(name = "PROJECT")
	private String project;
	
	@Column(name = "WBS_LEVEL")
	private Integer wbsLevel;
	
	@Column(name = "WBS_ELEMENT_HIERARCHY")
	private String wbsElementHierarchy;
	
	@Column(name = "SHORT_DESCRIPTION")
	private String shortDescription;
	
	@Column(name = "PERSON_RESPONSIBLE_NUMBER")
	private String personResponsibleNumber;	
	
	@Column(name = "COMPANY_CODE")
	private String companyCode;
	
	@Column(name = "BUSINESS_AREA")
	private String businessArea;
	
	@Column(name = "CONTROLLING_AREA")
	private String controllingArea;
	
	@Column(name = "PROFIT_CENTER")
	private String profitCenter;
	
	@Column(name = "PROJECT_TYPE")
	private String projectType;
	
	@Column(name = "PLANT")
	private String plant;
	
	@Column(name = "BASIC_START_DATE")
	private Timestamp basicStartDate;
	
	@Column(name = "BASIC_FINISH_DATE")
	private Timestamp basicFinishDate;
	
	@Column(name = "KEYWORD_ID")
	private String keywordId;
	
	@Column(name = "RESPONSIBLE_COST_CENTER")
	private String responsibleCostCenter;
	
	@Column(name = "ACCOUNT_ASSIGNMENT_ELEMENT")
	private String accountAssignmentElement;
	
	@Column(name = "BILLING_ELEMENT")
	private String billingElement;
	
	@Column(name = "CURRENCY")
	private String currency;
	
	@Column(name = "OBJECT_CLASS")
	private String objectClass;
	
	@Column(name = "FACTORY_CALENDER")
	private String factoryCalender;
	
	@Column(name = "FUND")
	private String fund;
	
	@Column(name = "PRNCP_INVESTGTR")
	private String prncpInvestgtr;
	
	@Column(name = "GST_CLAIMABLE")
	private String gstClaimable;
	
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

	public SapFeedTmplWbs() {
		
	}
	
	public SapFeedTmplWbs(String wbsElement, Integer batchId, String feedStatus, String message) {
		this.wbsElement = wbsElement;
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

	public String getWbsElement() {
		return wbsElement;
	}

	public void setWbsElement(String wbsElement) {
		this.wbsElement = wbsElement;
	}

	public String getProject() {
		return project;
	}

	public void setProject(String project) {
		this.project = project;
	}

	public String getWbsElementHierarchy() {
		return wbsElementHierarchy;
	}

	public void setWbsElementHierarchy(String wbsElementHierarchy) {
		this.wbsElementHierarchy = wbsElementHierarchy;
	}

	public String getShortDescription() {
		return shortDescription;
	}

	public void setShortDescription(String shortDescription) {
		this.shortDescription = shortDescription;
	}

	public String getPersonResponsibleNumber() {
		return personResponsibleNumber;
	}

	public void setPersonResponsibleNumber(String personResponsibleNumber) {
		this.personResponsibleNumber = personResponsibleNumber;
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

	public String getControllingArea() {
		return controllingArea;
	}

	public void setControllingArea(String controllingArea) {
		this.controllingArea = controllingArea;
	}

	public String getProfitCenter() {
		return profitCenter;
	}

	public void setProfitCenter(String profitCenter) {
		this.profitCenter = profitCenter;
	}

	public String getProjectType() {
		return projectType;
	}

	public void setProjectType(String projectType) {
		this.projectType = projectType;
	}

	public String getPlant() {
		return plant;
	}

	public void setPlant(String plant) {
		this.plant = plant;
	}

	public Timestamp getBasicStartDate() {
		return basicStartDate;
	}

	public void setBasicStartDate(Timestamp basicStartDate) {
		this.basicStartDate = basicStartDate;
	}

	public Timestamp getBasicFinishDate() {
		return basicFinishDate;
	}

	public void setBasicFinishDate(Timestamp basicFinishDate) {
		this.basicFinishDate = basicFinishDate;
	}

	public String getKeywordId() {
		return keywordId;
	}

	public void setKeywordId(String keywordId) {
		this.keywordId = keywordId;
	}

	public String getResponsibleCostCenter() {
		return responsibleCostCenter;
	}

	public void setResponsibleCostCenter(String responsibleCostCenter) {
		this.responsibleCostCenter = responsibleCostCenter;
	}

	public String getAccountAssignmentElement() {
		return accountAssignmentElement;
	}

	public void setAccountAssignmentElement(String accountAssignmentElement) {
		this.accountAssignmentElement = accountAssignmentElement;
	}

	public String getBillingElement() {
		return billingElement;
	}

	public void setBillingElement(String billingElement) {
		this.billingElement = billingElement;
	}

	public String getCurrency() {
		return currency;
	}

	public void setCurrency(String currency) {
		this.currency = currency;
	}

	public String getObjectClass() {
		return objectClass;
	}

	public void setObjectClass(String objectClass) {
		this.objectClass = objectClass;
	}

	public String getFactoryCalender() {
		return factoryCalender;
	}

	public void setFactoryCalender(String factoryCalender) {
		this.factoryCalender = factoryCalender;
	}

	public String getFund() {
		return fund;
	}

	public void setFund(String fund) {
		this.fund = fund;
	}

	public String getPrncpInvestgtr() {
		return prncpInvestgtr;
	}

	public void setPrncpInvestgtr(String prncpInvestgtr) {
		this.prncpInvestgtr = prncpInvestgtr;
	}

	public String getGstClaimable() {
		return gstClaimable;
	}

	public void setGstClaimable(String gstClaimable) {
		this.gstClaimable = gstClaimable;
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

	public Integer getWbsLevel() {
		return wbsLevel;
	}

	public void setWbsLevel(Integer wbsLevel) {
		this.wbsLevel = wbsLevel;
	}

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}

}
