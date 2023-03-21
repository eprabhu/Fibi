package com.polus.fibicomp.rule.dto;

import java.util.ArrayList;
import java.util.HashMap;

public class BusinessRule {

	private ArrayList<HashMap<String, Object>> rule;
	private ArrayList<HashMap<String, Object>> ruleExpression;
	private ArrayList<Integer> deletedRuleExpressionList;
	private ArrayList<Integer> deletedRuleExpressionArgs;

	public ArrayList<HashMap<String, Object>> getRule() {
		return rule;
	}

	public void setRule(ArrayList<HashMap<String, Object>> rule) {
		this.rule = rule;
	}

	public ArrayList<HashMap<String, Object>> getRuleExpression() {
		return ruleExpression;
	}

	public void setRuleExpression(ArrayList<HashMap<String, Object>> ruleExpression) {
		this.ruleExpression = ruleExpression;
	}

	public ArrayList<Integer> getDeletedRuleExpressionList() {
		return deletedRuleExpressionList;
	}

	public void setDeletedRuleExpressionList(ArrayList<Integer> deletedRuleExpressionList) {
		this.deletedRuleExpressionList = deletedRuleExpressionList;
	}

	public ArrayList<Integer> getDeletedRuleExpressionArgs() {
		return deletedRuleExpressionArgs;
	}

	public void setDeletedRuleExpressionArgs(ArrayList<Integer> deletedRuleExpressionArgs) {
		this.deletedRuleExpressionArgs = deletedRuleExpressionArgs;
	}

}
