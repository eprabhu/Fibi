package com.polus.fibicomp.rule.dto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class WorkFlowDataBus {

	private Integer ruleId;

	private Integer questionId;

	private Integer moduleCode;

	private Integer subModuleCode;

	private String lookUpWindowName;

	private ArrayList<HashMap<String, Object>> unitList;

	private ArrayList<HashMap<String, Object>> notificationList;

	private ArrayList<HashMap<String, Object>> moduleSubmoduleList;

	private ArrayList<HashMap<String, Object>> deleteRuleList;

	private ArrayList<HashMap<String, Object>> ruleEvaluationOrderList;

	private List<HashMap<String, Object>> functionArguments;

	private String unitNumber;

	private String ruleType;

	private String functionName;

	private Integer questionNumber;

	public ArrayList<HashMap<String, Object>> getDeleteRuleList() {
		return deleteRuleList;
	}

	public void setDeleteRuleList(ArrayList<HashMap<String, Object>> deleteRuleList) {
		this.deleteRuleList = deleteRuleList;
	}

	private BusinessRule businessRule = new BusinessRule();

	private BusinessRuleDetails businessRuleDetails = new BusinessRuleDetails();

	public BusinessRule getBusinessRule() {
		return businessRule;
	}

	public void setBusinessRule(BusinessRule businessRule) {
		this.businessRule = businessRule;
	}

	public ArrayList<HashMap<String, Object>> getUnitList() {
		return unitList;
	}

	public void setUnitList(ArrayList<HashMap<String, Object>> unitList) {
		this.unitList = unitList;
	}

	public ArrayList<HashMap<String, Object>> getNotificationList() {
		return notificationList;
	}

	public void setNotificationList(ArrayList<HashMap<String, Object>> notificationList) {
		this.notificationList = notificationList;
	}

	public ArrayList<HashMap<String, Object>> getModuleSubmoduleList() {
		return moduleSubmoduleList;
	}

	public void setModuleSubmoduleList(ArrayList<HashMap<String, Object>> moduleSubmoduleList) {
		this.moduleSubmoduleList = moduleSubmoduleList;
	}

	public BusinessRuleDetails getBusinessRuleDetails() {
		return businessRuleDetails;
	}

	public void setBusinessRuleDetails(BusinessRuleDetails businessRuleDetails) {
		this.businessRuleDetails = businessRuleDetails;
	}

	public Integer getRuleId() {
		return ruleId;
	}

	public void setRuleId(Integer ruleId) {
		this.ruleId = ruleId;
	}

	public String getLookUpWindowName() {
		return lookUpWindowName;
	}

	public void setLookUpWindowName(String lookUpWindowName) {
		this.lookUpWindowName = lookUpWindowName;
	}

	public Integer getQuestionId() {
		return questionId;
	}

	public void setQuestionId(Integer questionId) {
		this.questionId = questionId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public ArrayList<HashMap<String, Object>> getRuleEvaluationOrderList() {
		return ruleEvaluationOrderList;
	}

	public void setRuleEvaluationOrderList(ArrayList<HashMap<String, Object>> ruleEvaluationOrderList) {
		this.ruleEvaluationOrderList = ruleEvaluationOrderList;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getRuleType() {
		return ruleType;
	}

	public void setRuleType(String ruleType) {
		this.ruleType = ruleType;
	}

	public String getFunctionName() {
		return functionName;
	}

	public void setFunctionName(String functionName) {
		this.functionName = functionName;
	}

	public List<HashMap<String, Object>> getFunctionArguments() {
		return functionArguments;
	}

	public void setFunctionArguments(List<HashMap<String, Object>> functionArguments) {
		this.functionArguments = functionArguments;
	}

	public Integer getQuestionNumber() {
		return questionNumber;
	}

	public void setQuestionNumber(Integer questionNumber) {
		this.questionNumber = questionNumber;
	}
}
