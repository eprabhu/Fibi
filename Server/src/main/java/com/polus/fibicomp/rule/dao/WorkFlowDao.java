package com.polus.fibicomp.rule.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.rule.dto.WorkFlowDataBus;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.utils.QueryBuilder;

@Service
public class WorkFlowDao {

	protected static Logger logger = LogManager.getLogger(WorkFlowDao.class.getName());

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private CommonDao commonDao;

	public synchronized Integer getNextRuleId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_WRKLFW_BUSINESS_RULE_ID", "RULE_ID");
			updateMaxColumnValue("UPDATE_MAX_WRKLFW_BUSINESS_RULE_ID", "RULE_ID", columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireId" + e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextRuleExpressionId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("get_max_rule_expression_id", "RULES_EXPERSSION_ID");
			updateMaxColumnValue("update_max_rule_expression_id", "RULES_EXPERSSION_ID", columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireId" + e.getMessage());
			return 0;
		}
	}

	public ArrayList<HashMap<String, Object>> getAllBusinessRule(WorkFlowDataBus workFlowDataBus) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER, workFlowDataBus.getModuleCode()));
			inParam.add(new Parameter("<<SUB_MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER, workFlowDataBus.getSubModuleCode()));
			inParam.add(new Parameter("<<UNIT_NUMBER>>", DBEngineConstants.TYPE_STRING, workFlowDataBus.getUnitNumber()));
			inParam.add(new Parameter("<<RULE_TYPE>>", DBEngineConstants.TYPE_STRING, workFlowDataBus.getRuleType()));
			output = dbEngine.executeQuery(inParam, "get_business_rule");
		} catch (Exception e) {
			logger.error("Exception in getAllBusinessRule " + e.getMessage());
		}
		return output;
	}

	public ArrayList<HashMap<String, Object>> getAllUnits() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_ALL_UNITS");
        } catch (Exception e) {
			logger.error("Exception in getAllUnits " + e.getMessage());
        }
		return output;
	}

	public ArrayList<HashMap<String, Object>> getNotificationLists() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_NOTIFICATIONS");
        } catch (Exception e) {
			logger.error("Exception in getNotifications " + e.getMessage());
        }
		return output;
	}

	public ArrayList<HashMap<String, Object>> getModuleSubmodules() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_MODULE_SUBMODULE_CODE");
		} catch (Exception e) {
			logger.error("Exception in getAllUnits " + e.getMessage());
		}
		return output;
	}

	public int insertBusinessRule(HashMap<String, Object> hmRules, int ruleId, int ruleEvaluvation) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
			inParam.add(new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, 
					hmRules.get("DESCRIPTION")));
			inParam.add(new Parameter("<<RULE_TYPE>>", DBEngineConstants.TYPE_STRING, hmRules.get("RULE_TYPE")));
			inParam.add(new Parameter("<<RULE_CATEGORY>>", DBEngineConstants.TYPE_STRING, 
					hmRules.get("RULE_CATEGORY")));
			inParam.add(new Parameter("<<UNIT_NUMBER>>", DBEngineConstants.TYPE_STRING, hmRules.get("UNIT_NUMBER")));
			inParam.add(new Parameter("<<MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER, hmRules.get("MODULE_CODE")));
			inParam.add(new Parameter("<<SUB_MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER,
					hmRules.get("SUB_MODULE_CODE")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, 
					hmRules.get("UPDATE_USER")));
			inParam.add(new Parameter("<<IS_ACTIVE>>", DBEngineConstants.TYPE_STRING, 
					hmRules.get("IS_ACTIVE")));
			inParam.add(new Parameter("<<RULE_EXPRESSION>>", DBEngineConstants.TYPE_STRING,
					hmRules.get("RULE_EXPRESSION")));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, hmRules.get("MAP_ID")));
			inParam.add(new Parameter("<<NOTIFICATION_ID>>", DBEngineConstants.TYPE_INTEGER,
					hmRules.get("NOTIFICATION_ID")));
			inParam.add(new Parameter("<<USER_MESSAGE>>", DBEngineConstants.TYPE_STRING, 
					hmRules.get("USER_MESSAGE")));
			inParam.add(new Parameter("<<RULE_EVALUATION_ORDER>>", DBEngineConstants.TYPE_INTEGER, ruleEvaluvation));			
			dbEngine.executeUpdate(inParam, "INSERT_BUSINESS_RULE");
			return 1;
		} catch (Exception e) {
			logger.error("Exception in insertBusinessRule" + e.getMessage());
			return 0;
		}
	}

	public int getRuleEvaluationOrder(HashMap<String, Object> hmRule) {
		Integer ruleEvaluvationId = 1;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<SUB_MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER,
					hmRule.get("SUB_MODULE_CODE")));
			inParam.add(new Parameter("<<UNIT_NUMBER>>", DBEngineConstants.TYPE_STRING, hmRule.get("UNIT_NUMBER")));
			inParam.add(new Parameter("<<MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER, hmRule.get("MODULE_CODE")));
			inParam.add(new Parameter("<<RULE_TYPE>>", DBEngineConstants.TYPE_STRING, hmRule.get("RULE_TYPE")));
			ArrayList<HashMap<String, Object>> output = dbEngine.executeQuery(inParam,
					"GET_MAX_WRKLFW_BUSINESS_RULE_ORDER_ID");
			HashMap<String, Object> response = output.get(0);
			if (response.get("ID") != null) {
				ruleEvaluvationId = Integer.parseInt(response.get("ID").toString());
				ruleEvaluvationId++;
			}
		} catch (Exception e) {
			logger.error("Exception in insertBusinessRule" + e.getMessage());
			e.printStackTrace();
		}
		return ruleEvaluvationId;
	}

	public void insertRuleExpression(HashMap<String, Object> hmRules, int ruleId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			Integer ruleExpressionId = getNextRuleExpressionId();
			inParam.add(new Parameter("<<RULES_EXPERSSION_ID>>", DBEngineConstants.TYPE_INTEGER,
					ruleExpressionId));
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
			inParam.add(new Parameter("<<EXPRESSION_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					hmRules.get("EXPRESSION_NUMBER")));
			inParam.add(new Parameter("<<EXPRESSION_TYPE_CODE>>", DBEngineConstants.TYPE_STRING,
					hmRules.get("EXPRESSION_TYPE_CODE")));
			inParam.add(new Parameter("<<LVALUE>>", DBEngineConstants.TYPE_STRING, hmRules.get("LVALUE")));
			inParam.add(new Parameter("<<CONDITION_OPERATOR>>", DBEngineConstants.TYPE_STRING,
					hmRules.get("CONDITION_OPERATOR")));
			inParam.add(new Parameter("<<RVALUE>>", DBEngineConstants.TYPE_STRING,
					(hmRules.get("RVALUE") != null ? hmRules.get("RVALUE").toString():"")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, hmRules.get("UPDATE_USER")));
			inParam.add(new Parameter("<<PARENT_EXPRESSION_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					hmRules.get("PARENT_EXPRESSION_NUMBER")));
			dbEngine.executeUpdate(inParam, "INSERT_BUSINESS_RULE_EXPRESSION");
			List<HashMap<String, Object>> expressionArguments = (ArrayList<HashMap<String, Object>>) hmRules.get("EXPRESSION_ARGUMENTS");
			if(expressionArguments != null && !expressionArguments.isEmpty()) {
				expressionArguments.forEach(expressionArgument -> {
			saveBusinessRuleExpressionArgument(ruleExpressionId, expressionArgument, ruleId);
				});
			}
		} catch (Exception e) {
			logger.error("Exception in insertBusinessRule" + e.getMessage());
		}
	}

	public int updateBusinessRule(HashMap<String, Object> hmRules,int ruleEvaluvation) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, hmRules.get("DESCRIPTION")));
			inParam.add(new Parameter("<<RULE_TYPE>>", DBEngineConstants.TYPE_STRING, hmRules.get("RULE_TYPE")));
			inParam.add(new Parameter("<<RULE_CATEGORY>>", DBEngineConstants.TYPE_STRING, 
					hmRules.get("RULE_CATEGORY")));
			inParam.add(new Parameter("<<UNIT_NUMBER>>", DBEngineConstants.TYPE_STRING, hmRules.get("UNIT_NUMBER")));
			inParam.add(new Parameter("<<MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER, hmRules.get("MODULE_CODE")));
			inParam.add(new Parameter("<<SUB_MODULE_CODE>>", DBEngineConstants.TYPE_INTEGER,
					hmRules.get("SUB_MODULE_CODE")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, hmRules.get("UPDATE_USER")));
			inParam.add(new Parameter("<<IS_ACTIVE>>", DBEngineConstants.TYPE_STRING, hmRules.get("IS_ACTIVE")));
			inParam.add(new Parameter("<<RULE_EXPRESSION>>", DBEngineConstants.TYPE_STRING,
					hmRules.get("RULE_EXPRESSION")));
			inParam.add(new Parameter("<<MAP_ID>>", DBEngineConstants.TYPE_INTEGER, hmRules.get("MAP_ID")));
			inParam.add(new Parameter("<<NOTIFICATION_ID>>", DBEngineConstants.TYPE_INTEGER,
					hmRules.get("NOTIFICATION_ID")));
			inParam.add(new Parameter("<<USER_MESSAGE>>", DBEngineConstants.TYPE_STRING,
					hmRules.get("USER_MESSAGE")));
			inParam.add(new Parameter("<<RULE_EVALUATION_ORDER>>", DBEngineConstants.TYPE_INTEGER, ruleEvaluvation));
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, hmRules.get("RULE_ID")));
			dbEngine.executeUpdate(inParam, "UPDATE_BUSINESS_RULE");
			return 1;
		} catch (Exception e) {
			logger.error("Exception in updateBusinessRule" + e.getMessage());
			e.printStackTrace();
			return 0;
		}

	}

	public void updateRuleExpression(HashMap<String, Object> ruleExpressionList, int ruleId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<EXPRESSION_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					ruleExpressionList.get("EXPRESSION_NUMBER")));
			inParam.add(new Parameter("<<EXPRESSION_TYPE_CODE>>", DBEngineConstants.TYPE_STRING,
					ruleExpressionList.get("EXPRESSION_TYPE_CODE")));
			inParam.add(new Parameter("<<LVALUE>>", DBEngineConstants.TYPE_STRING, ruleExpressionList.get("LVALUE")));
			inParam.add(new Parameter("<<CONDITION_OPERATOR>>", DBEngineConstants.TYPE_STRING,
					ruleExpressionList.get("CONDITION_OPERATOR")));
			inParam.add(new Parameter("<<RVALUE>>", DBEngineConstants.TYPE_STRING, String.valueOf(ruleExpressionList.get("RVALUE"))));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING,
					ruleExpressionList.get("UPDATE_USER")));
			inParam.add(new Parameter("<<PARENT_EXPRESSION_NUMBER>>", DBEngineConstants.TYPE_INTEGER,
					ruleExpressionList.get("PARENT_EXPRESSION_NUMBER")));
			inParam.add(new Parameter("<<RULES_EXPERSSION_ID>>", DBEngineConstants.TYPE_INTEGER,
					ruleExpressionList.get("RULES_EXPERSSION_ID")));
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
			dbEngine.executeUpdate(inParam, "UPDATE_BUSINESS_RULE_EXPRESSION");
			List<HashMap<String, Object>> expressionArguments =  (List<HashMap<String, Object>>) ruleExpressionList.get("EXPRESSION_ARGUMENTS");
			if(expressionArguments != null && !expressionArguments.isEmpty()) {
				expressionArguments.forEach(expressionArgument -> {
				int ruleExpressionArgId = expressionArgument.get("BUSINESS_RULE_EXP_ARGS_ID") != null ? (int) expressionArgument.get("BUSINESS_RULE_EXP_ARGS_ID") : 0;
					if (ruleExpressionArgId == 0) {
						saveBusinessRuleExpressionArgument((int)ruleExpressionList.get("RULES_EXPERSSION_ID"), expressionArgument, ruleId);
					} else {
						updateBusinessRuleExpressionArgument((int)ruleExpressionList.get("RULES_EXPERSSION_ID"), expressionArgument, ruleId);
					}
					});
			}
		} catch (Exception e) {
			logger.error("Exception in updateBusinessRule" + e.getMessage());
		}
	}

	public void inactivateBusinessRule(HashMap<String, Object> hmRules) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<IS_ACTIVE>>", DBEngineConstants.TYPE_STRING, hmRules.get("IS_ACTIVE")));
			inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, hmRules.get("UPDATE_USER")));
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, hmRules.get("RULE_ID")));
			dbEngine.executeUpdate(inParam, "INACTIVATE_BUSINESS_RULE");
		} catch (Exception e) {
			logger.error("Exception in inactivate" + e.getMessage());
		}

	}

	private Integer getMaxColumnValue(String SqlId, String columnName) {
		Integer columnId = null;
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			ArrayList<HashMap<String, Object>> result = dbEngine.executeQuery(inputParam, SqlId);
			HashMap<String, Object> hmResult = result.get(0);
			if (hmResult.get(columnName) == null) {
				return 0;
			} else {
				columnId = Integer.parseInt(hmResult.get(columnName).toString());
			}
			return ++columnId;
		} catch (Exception e) {
			logger.error("Exception in getMaxColumnValue" + e.getMessage());
			return null;
		}
	}

	private void updateMaxColumnValue(String SqlId, String columnName, Integer maxColumnValue) {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter(columnName, DBEngineConstants.TYPE_INTEGER, maxColumnValue));
			dbEngine.executeUpdate(inputParam, SqlId);
		} catch (Exception e) {
			logger.error("Exception in updateMaxColumnValue" + e.getMessage());
        }
	}

	public ArrayList<HashMap<String, Object>> getRuleVariable(Integer moduleCode, Integer subModuleCode) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_ITEM_CODE>>", DBEngineConstants.TYPE_INTEGER, moduleCode));
			output = dbEngine.executeQuery(inParam, "get_rule_variable_details");
        } catch (Exception e) {
			logger.error("Exception in getAllBusinessRule " + e.getMessage());
        }

		return output;
	}

	public ArrayList<HashMap<String, Object>> getRuleFunction(Integer moduleCode, Integer subModuleCode) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_ITEM_CODE>>", DBEngineConstants.TYPE_INTEGER, moduleCode));
			inParam.add(new Parameter("<<MODULE_SUB_ITEM_CODE>>", DBEngineConstants.TYPE_INTEGER, subModuleCode));
			output = dbEngine.executeQuery(inParam, "get_rule_function_details");
        } catch (Exception e) {
			logger.error("Exception in get Function Details " + e.getMessage());
        }
		return output;
	}

	public List<HashMap<String, Object>> getRuleQuestion(Integer moduleCode) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_ITEM_CODE>>", DBEngineConstants.TYPE_INTEGER, moduleCode));
			output = dbEngine.executeQuery(inParam, "get_rule_question_details");
        } catch (Exception e) {
			logger.error("Exception in get Question Details : {}", e.getMessage());
        }
		return output;
	}

	public ArrayList<HashMap<String, Object>> getBusinessRules(Integer ruleId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
            inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
			output = dbEngine.executeQuery(inParam, "GET_RULE_BY_ID");
		} catch (Exception e) {
			logger.error("Exception in get business rule " + e.getMessage());
		}
		return output;
	}

	public ArrayList<HashMap<String, Object>> getBusinessRulesExpression(Integer ruleId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
			output = dbEngine.executeQuery(inParam, "GET_RULEEXPRESSION_BY_ID");
		} catch (Exception e) {
			logger.error("Exception in get business rule details " + e.getMessage());
		}
		return output;
	}

	public ArrayList<HashMap<String, Object>> getLookUpDetails(String lookUpWindowName) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<LOOKUP_WINDOW_NAME>>", DBEngineConstants.TYPE_STRING, lookUpWindowName));
			output = dbEngine.executeQuery(inParam, "GET_LOOK_UP_DETAILS");
		} catch (Exception e) {
			logger.error("Exception in get Look Up Table" + e.getMessage());
		}
		return output;
	}

	public ArrayList<HashMap<String, Object>> getLookupTable(String tableName, String columnName) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		String query = QueryBuilder.selectQueryForMap(tableName, columnName);
		try {
			output = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
		} catch (Exception e) {
			logger.error("Exception in get business rule details " + e.getMessage());
		}
		return output;
	}
	
	public ArrayList<HashMap<String, Object>> getLookupTable(String tableName, String columnCode, String columnDesc) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		String query = QueryBuilder.selectQueryForCodetable(tableName, columnCode,columnDesc);
		try {
			output = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
		} catch (Exception e) {
			logger.error("Exception in get business rule details ColumnList" + e.getMessage());
		}
		return output;
	}
	public ArrayList<HashMap<String, Object>> getLookupTable(String tableName, String columnCode, String columnDesc,String whereClauseValue) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		String query = QueryBuilder.selectQueryForCodetableSelectedColumn(tableName, columnCode,columnDesc, whereClauseValue);
		try {
			output = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
		} catch (Exception e) {
			logger.error("Exception in getLookupTable" + e.getMessage());
		}
		return output;
	}	
	

	public ArrayList<HashMap<String, Object>> getQuestionsByNumber(Integer questionNumber) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUESTION_NUMBER>>", DBEngineConstants.TYPE_INTEGER, questionNumber));
			output = dbEngine.executeQuery(inParam, "GET_QUESTION_DETAILS_BY_NUMBER");
		} catch (Exception e) {
			logger.error("Exception in getQuestionsByNumber" + e.getMessage());
		}
		return output;
	}

	public ArrayList<HashMap<String, Object>> getQuestionOptionList(Integer questionId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUESTION_ID>>", DBEngineConstants.TYPE_INTEGER, questionId));
			output = dbEngine.executeQuery(inParam, "get_option_details_by_id");
		} catch (Exception e) {
			logger.error("Exception in get Look Up Table" + e.getMessage());
		}
		return output;
	}

	public void deleteRuleExpressionList(Integer item) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<RULES_EXPERSSION_ID>>", DBEngineConstants.TYPE_INTEGER, item));
			dbEngine.executeUpdate(inParam, "delete_rule_expression_by_id");
        } catch (Exception e) {
			logger.error("Exception in delete rule expression details " + e.getMessage());
		}
    }

	public void updateRuleOrder(HashMap<String, Object> hmRules) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<RULE_EVALUATION_ORDER>>", DBEngineConstants.TYPE_INTEGER, 
					hmRules.get("RULE_EVALUATION_ORDER")));
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, hmRules.get("RULE_ID")));
			dbEngine.executeUpdate(inParam, "UPDATE_RULE_ORDER");
		} catch (Exception e) {
			logger.error("Exception in updating rule order" + e.getMessage());
		}

	}
	
	public ArrayList<HashMap<String, Object>> getVariableLookupDetails(String variableName) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<VARIABLE_NAME>>", DBEngineConstants.TYPE_STRING, variableName));
			output = dbEngine.executeQuery(inParam, "GET_VARIABLE_LOOKUP_DETAILS");			
		} catch (Exception e) {
			logger.error("Exception in getVariableLookupName" + e.getMessage());
			
		}		
		return output;
	}

	public ArrayList<HashMap<String, Object>> getAllBusinessRules() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "get_all_business_rule");
		} catch (Exception e) {
			logger.error("Exception in getAllBusinessRule " + e.getMessage());
		}

		return output;
	}

	private void saveBusinessRuleExpressionArgument(Integer ruleExpressionId, HashMap<String, Object> expressionArgument, int ruleId) {
		Integer expArgId = getNextExpArgId();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter("<<BUSINESS_RULE_EXP_ARGS_ID>>", DBEngineConstants.TYPE_INTEGER, expArgId));
		inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
		inParam.add(new Parameter("<<RULES_EXPERSSION_ID>>", DBEngineConstants.TYPE_INTEGER, ruleExpressionId));
		inParam.add(new Parameter("<<FUNCTION_NAME>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("FUNCTION_NAME")));
		inParam.add(new Parameter("<<ARGUMENT_NAME>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("ARGUMENT_NAME")));
		inParam.add(new Parameter("<<VALUE>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("VALUE").toString()));
		inParam.add(new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("DESCRIPTION")));
		inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
		inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, AuthenticatedUser.getLoginUserName()));	
		try {
			dbEngine.executeUpdate(inParam, "INSERT_BUSINESS_RULE_EXP_ARGS");
		} catch (Exception e) {
			logger.error("Exception in saveBusinessRuleExpressionArgument : {}",  e.getMessage());
		}
	}

	public synchronized Integer getNextExpArgId() {
			Integer columnId = null;
			try {
				columnId = getMaxColumnValue("get_max_rule_expression_arg_id", "RULES_EXPERSSION_ARG_ID");
				updateMaxColumnValue("update_max_rule_expression_arg_id", "RULES_EXPERSSION_ARG_ID", columnId);
				return columnId;
			} catch (Exception e) {
				logger.error("Exception in getNextExpArgId : {}", e.getMessage());
				return 0;
			}
		}

	private void updateBusinessRuleExpressionArgument(Integer ruleExpressionId, HashMap<String, Object> expressionArgument, int ruleId) {
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
		inParam.add(new Parameter("<<RULES_EXPERSSION_ID>>", DBEngineConstants.TYPE_INTEGER, ruleExpressionId));
		inParam.add(new Parameter("<<FUNCTION_NAME>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("FUNCTION_NAME")));
		inParam.add(new Parameter("<<ARGUMENT_NAME>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("ARGUMENT_NAME")));
		inParam.add(new Parameter("<<VALUE>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("VALUE").toString()));
		inParam.add(new Parameter("<<DESCRIPTION>>", DBEngineConstants.TYPE_STRING, expressionArgument.get("DESCRIPTION")));
		inParam.add(new Parameter("<<UPDATE_TIMESTAMP>>", DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
		inParam.add(new Parameter("<<UPDATE_USER>>", DBEngineConstants.TYPE_STRING, AuthenticatedUser.getLoginUserName()));
		inParam.add(new Parameter("<<BUSINESS_RULE_EXP_ARGS_ID>>", DBEngineConstants.TYPE_INTEGER, expressionArgument.get("BUSINESS_RULE_EXP_ARGS_ID")));
		try {
			dbEngine.executeUpdate(inParam, "UPDATE_BUSINESS_RULE_EXP_ARGS");
		} catch (Exception e) {
			logger.error("Exception in updateBusinessRuleExpressionArgument : {}", e.getMessage());
		}
	}

	public List<HashMap<String, Object>> getFunctionParameters(String functionName) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<FUNCTION_NAME>>", DBEngineConstants.TYPE_STRING, functionName));
			output = dbEngine.executeQuery(inParam, "GET_FUNCTION_ARGUMENT_DETAILS");			
		} catch (Exception e) {
			logger.error("Exception in getFunctionParameters : {}", e.getMessage());
		}
		return output;
	}

	public void deleteRuleExpressionArgs(Integer item) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<RULES_EXPERSSION_ID>>", DBEngineConstants.TYPE_INTEGER, item));
			dbEngine.executeUpdate(inParam, "delete_rule_expression_args_by_rule_exp_id");
        } catch (Exception e) {
			logger.error("Exception in deleteRuleExpressionArgs : {}", e.getMessage());
		}		
	}

	public void deleteRuleExpressionArgById(Integer expressionArg) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<RULES_EXPERSSION_ARG_ID>>", DBEngineConstants.TYPE_INTEGER, expressionArg));
			dbEngine.executeUpdate(inParam, "delete_rule_expression_args_by_rule_exp_arg_id");
        } catch (Exception e) {
			logger.error("Exception in delete rule expression details : {}", e.getMessage());
		}
	}

	public List<HashMap<String, Object>> getFunctionArgsLookupDetails(String functionName) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<FUNCTION_NAME>>", DBEngineConstants.TYPE_STRING, functionName));
			output = dbEngine.executeQuery(inParam, "get_function_arg_lookup_details");
        } catch (Exception e) {
			logger.error("Exception in method for getFunctionArgsLookupDetails : {}", e.getMessage());
		}
		return output;
	}

	public List<HashMap<String, Object>> getFunctionArgValues(Integer expressionId, String argumentName, String tableName, String columnName, String descriptionColumn) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			String joinQuery = new StringBuilder(" LEFT OUTER JOIN ").
					append(tableName).append(" S2 ON T1.VALUE = S2.").append(columnName).append(" ").toString();
			String query = new StringBuilder("select T1.*,T2.LOOKUP_TYPE,T2.ARGUMENT_LABEL,T2.LOOKUP_WINDOW_NAME,S2.").append(descriptionColumn).append(" AS VALUE_DESCRIPTION  from business_rule_exp_args T1").
					append(" INNER JOIN business_rule_function_arg T2 ON T2.FUNCTION_NAME = T1.FUNCTION_NAME AND T1.ARGUMENT_NAME = T2.ARGUMENT_NAME").
					append(joinQuery).append(" where T1.RULES_EXPERSSION_ID = ").append(expressionId).append(" AND T1.ARGUMENT_NAME = '").
					append(argumentName).append("'").toString();
			output = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
        } catch (Exception e) {
			logger.error("Exception in method for getFunctionArgValues : {}", e.getMessage());
		}
		return output;
	}

	public List<HashMap<String, Object>> getFunctionArgValues(int expressionId, String argumentName) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			String query = new StringBuilder("select T1.*,T2.LOOKUP_TYPE,T2.ARGUMENT_LABEL,T2.LOOKUP_WINDOW_NAME,T1.VALUE AS VALUE_DESCRIPTION").append(" from business_rule_exp_args T1").
					append(" INNER JOIN business_rule_function_arg T2 ON T2.FUNCTION_NAME = T1.FUNCTION_NAME AND T1.ARGUMENT_NAME = T2.ARGUMENT_NAME").
					append(" where T1.RULES_EXPERSSION_ID = ").append(expressionId).append(" AND T1.ARGUMENT_NAME = '").
					append(argumentName).append("'").toString();
			output = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
        } catch (Exception e) {
			logger.error("Exception in method getFunctionArgValues : {}", e.getMessage());
		}
		return output;
	}

	public ArrayList<HashMap<String, Object>> getLatestQuestionsByNumber(Integer questionNumber) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUESTION_NUMBER>>", DBEngineConstants.TYPE_INTEGER, questionNumber));
			output = dbEngine.executeQuery(inParam, "GET_LATEST_QUESTION_DETAILS_BY_NUMBER");
		} catch (Exception e) {
			logger.error("Exception in getLatestQuestionsByNumber : {}", e.getMessage());
		}
		return output;
	}

	public void deleteBusinessRulesExpressionByRuleId(Integer ruleId) throws Exception {
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
		dbEngine.executeUpdate(inParam, "DELETE_RULE_EXPRESSION_BY_RULE_ID");
	}

	public void deleteBusinessRuleById(Integer ruleId) throws Exception {
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
		dbEngine.executeUpdate(inParam, "DELETE_RULE_BY_ID");
	}

	public void deleteBusinessRulesExpressionArgumentByRuleId(Integer ruleId) throws Exception {
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_INTEGER, ruleId));
		dbEngine.executeUpdate(inParam, "DELETE_RULE_EXPRESSION_ARGUMENT_BY_RULE_ID");
	}
}
