package com.polus.fibicomp.rule.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.rule.dao.WorkFlowDao;
import com.polus.fibicomp.rule.dto.WorkFlowDataBus;
import com.polus.fibicomp.vo.EndPoints;

@Service
public class WorkFlowService {

	protected static Logger logger = LogManager.getLogger(WorkFlowService.class.getName());

	@Autowired
	WorkFlowDao workFlowDao;

	public WorkFlowDataBus getRuleType(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> moduleSubmodules = workFlowDao.getModuleSubmodules();
		workFlowDataBus.setModuleSubmoduleList(moduleSubmodules);
		return workFlowDataBus;
	}

	public WorkFlowDataBus getRuleLists(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> businessRuleList = workFlowDao.getAllBusinessRule(workFlowDataBus);
		workFlowDataBus.getBusinessRule().setRule(businessRuleList);
		return workFlowDataBus;
	}

	public WorkFlowDataBus getUnitList(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> unitList = workFlowDao.getAllUnits();
		workFlowDataBus.setUnitList(unitList);
		return workFlowDataBus;
	}

	public WorkFlowDataBus inActivateRule(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> conditions = workFlowDataBus.getDeleteRuleList();
		for (HashMap<String, Object> hmRules : conditions) {
			workFlowDao.inactivateBusinessRule(hmRules);
		}
		return workFlowDataBus;
	}

	/* Insertion of Rules */
	public WorkFlowDataBus insertRule(WorkFlowDataBus workFlowDataBus) {

		ArrayList<HashMap<String, Object>> businessRuleList = workFlowDataBus.getBusinessRule().getRule();
		ArrayList<HashMap<String, Object>> businessRuleExpressionList = workFlowDataBus.getBusinessRule()
				.getRuleExpression();
		try {
			int ruleId = workFlowDao.getNextRuleId();
			for (HashMap<String, Object> hmRules : businessRuleList) {
				//String ruleType = (String) hmRules.get("RULE_TYPE");
				Integer ruleEvaluvation = 0;
				//if (ruleType.equals("R")) {
					ruleEvaluvation = workFlowDao.getRuleEvaluationOrder(hmRules);
				//}
				int isInserted = workFlowDao.insertBusinessRule(hmRules, ruleId, ruleEvaluvation);
				if (isInserted == 1) {
					for (HashMap<String, Object> ruleExpressionList : businessRuleExpressionList) {
						workFlowDao.insertRuleExpression(ruleExpressionList, ruleId);
					}
				}
			}
		} catch (Exception e) {
			System.out.println("Exception in insertMap" + e.getMessage());
		}
		return workFlowDataBus;
	}

	public WorkFlowDataBus getRuleDetails(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> ruleVariable = workFlowDao.getRuleVariable(workFlowDataBus.getModuleCode(),
				workFlowDataBus.getSubModuleCode());
		ArrayList<HashMap<String, Object>> ruleFunction = workFlowDao.getRuleFunction(workFlowDataBus.getModuleCode(),
				workFlowDataBus.getSubModuleCode());
		workFlowDataBus.getBusinessRuleDetails().setRuleVariable(ruleVariable);
		workFlowDataBus.getBusinessRuleDetails().setRuleFunction(ruleFunction);
		return workFlowDataBus;
	}

	public WorkFlowDataBus updateRule(WorkFlowDataBus workFlowDataBus) {

		ArrayList<HashMap<String, Object>> businessRuleList = workFlowDataBus.getBusinessRule().getRule();
		ArrayList<HashMap<String, Object>> businessRuleExpressionList = workFlowDataBus.getBusinessRule()
				.getRuleExpression();
		try {
			if (workFlowDataBus.getBusinessRule().getDeletedRuleExpressionList() != null) {
				for (Integer item : workFlowDataBus.getBusinessRule().getDeletedRuleExpressionList()) {
					workFlowDao.deleteRuleExpressionList(item);
					workFlowDao.deleteRuleExpressionArgs(item);
				}
			}
			if (workFlowDataBus.getBusinessRule().getDeletedRuleExpressionArgs() != null) {
				workFlowDataBus.getBusinessRule().getDeletedRuleExpressionArgs().forEach(expressionArg -> {
					workFlowDao.deleteRuleExpressionArgById(expressionArg);
				});
			}
			for (HashMap<String, Object> hmRules : businessRuleList) {
				int ruleId = (int) hmRules.get("RULE_ID");
				String ruleType = (String) hmRules.get("RULE_TYPE");
				String updateOrder = (String) hmRules.get("UPDATE_RULE_ORDER");
				Integer ruleEvaluvation = 0;
				if (ruleType.equals("R") && updateOrder.equals("true")) {
					ruleEvaluvation = workFlowDao.getRuleEvaluationOrder(hmRules);
				} else if (ruleType.equals("R") && updateOrder.equals("false")) {
					ruleEvaluvation = (int) hmRules.get("RULE_EVALUATION_ORDER");
				} else {
					ruleEvaluvation = (int) hmRules.get("RULE_EVALUATION_ORDER");
				}
				int isInserted = workFlowDao.updateBusinessRule(hmRules, ruleEvaluvation);
				if (isInserted == 1) {
					for (HashMap<String, Object> ruleExpressionList : businessRuleExpressionList) {
						int ruleExpressionId = (int) ruleExpressionList.get("RULES_EXPERSSION_ID");
						if (ruleExpressionId == 0) {
							workFlowDao.insertRuleExpression(ruleExpressionList, ruleId);
						} else {
							workFlowDao.updateRuleExpression(ruleExpressionList, ruleId);
						}
					}
				}
			}
		} catch (Exception e) {
			System.out.println("Exception in updateMap" + e.getMessage());
		}
		return workFlowDataBus;
	}

	public WorkFlowDataBus getBusinessRuleById(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> businessRuleList = workFlowDao.getBusinessRules(workFlowDataBus.getRuleId());
		ArrayList<HashMap<String, Object>> ruleExpressionList = workFlowDao
				.getBusinessRulesExpression(workFlowDataBus.getRuleId());
		workFlowDataBus.getBusinessRule().setRule(businessRuleList);
		setTheExpressionArgs(ruleExpressionList);
		updateRuleExpersionForVariable(ruleExpressionList);		
		workFlowDataBus.getBusinessRule().setRuleExpression(ruleExpressionList);
		workFlowDataBus.getBusinessRuleDetails().setLookUpDetails(getLookupDetails(workFlowDataBus.getLookUpWindowName()));
		return workFlowDataBus;
	}

	
	private ArrayList<HashMap<String, Object>> updateRuleExpersionForVariable(ArrayList<HashMap<String, Object>> ruleExpressionList){
		if(ruleExpressionList !=null && !ruleExpressionList.isEmpty()) {
			for(HashMap<String, Object> hmExpression : ruleExpressionList) {
				if("V".equals(hmExpression.get("EXPRESSION_TYPE_CODE"))) { //Expression Type is Variable
					ArrayList<HashMap<String, Object>> lookUpDetails = workFlowDao.getVariableLookupDetails((String) hmExpression.get("LVALUE"));
					if (lookUpDetails != null && !lookUpDetails.isEmpty()) {
						HashMap<String, Object> lookup = lookUpDetails.get(0);
						String tableName = (String) lookup.get("TABLE_NAME");
						String columnCode = (String) lookup.get("COLUMN_NAME");
						String columnDesc = (String) lookup.get("OTHERS_DISPLAY_COLUMNS");
						String lookUpDescription = (String) lookup.get("DESCRIPTION");
						String whereClauseValue = (String) hmExpression.get("RVALUE");
						ArrayList<HashMap<String, Object>> dataList = workFlowDao.getLookupTable(tableName, columnCode,
								columnDesc, whereClauseValue);
						if (dataList != null && !dataList.isEmpty()) {
							String rValueLabel = (dataList.get(0).get("DESCRIPTION") != null
									? dataList.get(0).get("DESCRIPTION").toString()
									: "");
							hmExpression.put("RVALUE_LABEL", rValueLabel);
						}
						hmExpression.put("LVALUE_LABEL", lookUpDescription);
					} else {
						hmExpression.put("RVALUE_LABEL", (String) hmExpression.get("RVALUE"));
						hmExpression.put("LVALUE_LABEL", (String) hmExpression.get("LVALUE"));
					}  					
				}else {
					hmExpression.put("RVALUE_LABEL", hmExpression.get("RVALUE"));
					hmExpression.put("LVALUE_LABEL", (String) hmExpression.get("LVALUE"));
				}
				
			}			
		}
		return ruleExpressionList;
	}
	
	public WorkFlowDataBus getByRuleVariable(WorkFlowDataBus workFlowDataBus) {
		try {
			
			String lookUpWindowName = workFlowDataBus.getLookUpWindowName();
			workFlowDataBus.getBusinessRuleDetails().setLookUpDetails(getLookupDetails(lookUpWindowName));
		} catch (Exception e) {
			logger.error("Exception in getByRuleVariable" + e.getMessage());
		}
		return workFlowDataBus;
	}
	
	public ArrayList<HashMap<String, Object>> getLookupDetails(String lookUpWindowName) {
		ArrayList<HashMap<String, Object>> dataList = new ArrayList<>();
		try {
			ArrayList<HashMap<String, Object>> lookUpDetails = workFlowDao.getLookUpDetails(lookUpWindowName);
			if (lookUpDetails != null && !lookUpDetails.isEmpty()) {
				HashMap<String, Object> ruleExpressionList = lookUpDetails.get(0);
				String tableName = (String) ruleExpressionList.get("TABLE_NAME");
				String columnCode = (String) ruleExpressionList.get("COLUMN_NAME");
				String columnDesc = (String) ruleExpressionList.get("OTHERS_DISPLAY_COLUMNS");
				dataList = workFlowDao.getLookupTable(tableName, columnCode, columnDesc);
			}			
		} catch (Exception e) {
			logger.error("Exception in getLookupDetails" + e.getMessage());
		}
		return dataList;
	}	
	

	public WorkFlowDataBus getQuestionsById(WorkFlowDataBus workFlowDataBus) {
		try {
			ArrayList<HashMap<String, Object>> dataList = workFlowDao.getQuestionsByNumber(workFlowDataBus.getQuestionNumber());
			if (dataList.isEmpty()) {
				dataList = workFlowDao.getLatestQuestionsByNumber(workFlowDataBus.getQuestionNumber());
			}
			if (!dataList.isEmpty()) {
				Integer questionId = (Integer) dataList.get(0).get("QUESTION_ID");
				ArrayList<HashMap<String, Object>> optionList = workFlowDao
						.getQuestionOptionList(questionId);
				workFlowDataBus.getBusinessRuleDetails().setQuestionDetails(dataList);
				workFlowDataBus.getBusinessRuleDetails().setOptionList(optionList);
			}
		} catch (Exception e) {
			logger.error("Exception in getQuestionbyid : {}", e.getMessage());
		}
		return workFlowDataBus;
	}

	public WorkFlowDataBus getNotificationList(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> notificationList = workFlowDao.getNotificationLists();
		workFlowDataBus.setNotificationList(notificationList);
		return workFlowDataBus;
	}

	public WorkFlowDataBus updateRuleEvaluationOrder(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> ruleOrderList = workFlowDataBus.getRuleEvaluationOrderList();
		for (HashMap<String, Object> hmRules : ruleOrderList) {
			workFlowDao.updateRuleOrder(hmRules);
		}
		return workFlowDataBus;
	}

	public WorkFlowDataBus getAllRuleLists(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> businessRuleList = workFlowDao.getAllBusinessRules();
		workFlowDataBus.getBusinessRule().setRule(businessRuleList);
		return workFlowDataBus;
	}

	private void setTheExpressionArgs(ArrayList<HashMap<String, Object>> ruleExpressionList) {
			ruleExpressionList.forEach(hmExpression -> {
			ArrayList<HashMap<String, Object>> ruleExpressionArgValueList = new ArrayList<>();
			if (hmExpression.get("EXPRESSION_TYPE_CODE").equals("F")) {
				List<HashMap<String, Object>> functionParameters = workFlowDao.getFunctionParameters(hmExpression.get("LVALUE").toString());
				functionParameters.forEach(functionParameter -> 
				getExpressionArgValues(functionParameter, ruleExpressionArgValueList, (int) hmExpression.get("RULES_EXPERSSION_ID"))
				);
				hmExpression.put("EXPRESSION_ARGUMENTS", ruleExpressionArgValueList);
				hmExpression.put("FUNCTION_PARAMETERS", functionParameters);
			}
		});
	}

	private void getExpressionArgValues(HashMap<String, Object> functionParameter,
			ArrayList<HashMap<String, Object>> ruleExpressionArgValueList, int ruleExpressionId) {
			String lookUpType = functionParameter.get("LOOKUP_TYPE").toString();
			String argumentName = functionParameter.get("ARGUMENT_NAME").toString();
		if (lookUpType.equals("System")) {
			String lookupField = functionParameter.get("LOOKUP_WINDOW_NAME").toString();
			String tableName = lookupField.substring(0, lookupField.indexOf('#'));
			String columnName = lookupField.substring(lookupField.indexOf('#')+1, lookupField.length());
			List<HashMap<String, Object>> functionArgValues = workFlowDao.getFunctionArgValues(ruleExpressionId,argumentName,
					tableName, columnName, "DESCRIPTION");
			ruleExpressionArgValueList.addAll(functionArgValues);
		} else if (lookUpType.equals("EndPoint") || lookUpType.equals("Elastic")) {
			EndPoints endPoints = EndPoints.getEndPointDetails(functionParameter.get("LOOKUP_WINDOW_NAME").toString());
			List<HashMap<String, Object>> functionArgValues = workFlowDao.getFunctionArgValues(ruleExpressionId,argumentName,
					endPoints.getTableName(), endPoints.getPrimaryColumn(), endPoints.getDescriptionColumn());
			ruleExpressionArgValueList.addAll(functionArgValues);
		}
		else {
			List<HashMap<String, Object>> functionArgValues = workFlowDao.getFunctionArgValues(ruleExpressionId,argumentName);
			ruleExpressionArgValueList.addAll(functionArgValues);
		}
	}

	public WorkFlowDataBus getFunctionParameters(WorkFlowDataBus workFlowDataBus) {
		List<HashMap<String, Object>> functionArguments = workFlowDao.getFunctionParameters(workFlowDataBus.getFunctionName());
		workFlowDataBus.setFunctionArguments(functionArguments);
		return workFlowDataBus;
	}

	public WorkFlowDataBus getActiveQuestions(WorkFlowDataBus workFlowDataBus) {
		List<HashMap<String, Object>> ruleQuestion = workFlowDao.getRuleQuestion(workFlowDataBus.getModuleCode());
		Map<String, List<HashMap<String, Object>>> groupedRuleQuestion = ruleQuestion.stream().collect(Collectors.groupingBy(t -> t.get("QUESTIONNAIRE").toString()));
		workFlowDataBus.getBusinessRuleDetails().setRuleQuestion(groupedRuleQuestion);
		return workFlowDataBus;
	}

	public boolean deleteBusinessRule(Integer ruleId) throws Exception {
		try {
			ArrayList<HashMap<String, Object>> data = workFlowDao.getBusinessRules(ruleId);
			if (data != null && !data.isEmpty() && data.get(0).get("IS_ACTIVE").equals("Y")) {
				return false;
			}
			workFlowDao.deleteBusinessRulesExpressionArgumentByRuleId(ruleId);
			workFlowDao.deleteBusinessRulesExpressionByRuleId(ruleId);
			workFlowDao.deleteBusinessRuleById(ruleId);
			return true;
		} catch(Exception e) {
			logger.error("Exception in deleteBusinessRule : {}",e.getMessage());
			return false;
		}
	}
}
