package com.polus.fibicomp.rule.dto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BusinessRuleDetails {

	private ArrayList<HashMap<String, Object>> ruleVariable;
	private ArrayList<HashMap<String, Object>> ruleFunction;
	private Map<String, List<HashMap<String, Object>>> ruleQuestion;
	private ArrayList<HashMap<String, Object>> lookUpDetails;
	private ArrayList<HashMap<String, Object>> questionDetails;
	private ArrayList<HashMap<String, Object>> optionList;

	public ArrayList<HashMap<String, Object>> getRuleVariable() {
		return ruleVariable;
	}

	public void setRuleVariable(ArrayList<HashMap<String, Object>> ruleVariable) {
		this.ruleVariable = ruleVariable;
	}

	public ArrayList<HashMap<String, Object>> getRuleFunction() {
		return ruleFunction;
	}

	public void setRuleFunction(ArrayList<HashMap<String, Object>> ruleFunction) {
		this.ruleFunction = ruleFunction;
	}

	public ArrayList<HashMap<String, Object>> getLookUpDetails() {
		return lookUpDetails;
	}

	public void setLookUpDetails(ArrayList<HashMap<String, Object>> lookUpDetails) {
		this.lookUpDetails = lookUpDetails;
	}

	public ArrayList<HashMap<String, Object>> getQuestionDetails() {
		return questionDetails;
	}

	public void setQuestionDetails(ArrayList<HashMap<String, Object>> questionDetails) {
		this.questionDetails = questionDetails;
	}

	public ArrayList<HashMap<String, Object>> getOptionList() {
		return optionList;
	}

	public void setOptionList(ArrayList<HashMap<String, Object>> optionList) {
		this.optionList = optionList;
	}

	public Map<String, List<HashMap<String, Object>>> getRuleQuestion() {
		return ruleQuestion;
	}

	public void setRuleQuestion(Map<String, List<HashMap<String, Object>>> ruleQuestion) {
		this.ruleQuestion = ruleQuestion;
	}

}
