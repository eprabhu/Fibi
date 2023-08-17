package com.polus.fibicomp.questionnaire.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.Query;
import javax.persistence.ParameterMode;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.hibernate.query.NativeQuery;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.coi.pojo.CoiQuestTableAnswer;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.general.dao.GeneralInformationDao;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.pojo.QuestAnswerHeader;
import com.polus.fibicomp.questionnaire.pojo.QuestTableAnswer;
import com.polus.fibicomp.security.AuthenticatedUser;

import oracle.jdbc.OracleTypes;

@Service
@Transactional
public class QuestionnaireDAO {

	protected static Logger logger = LogManager.getLogger(QuestionnaireDAO.class.getName());

	@Autowired
	public DBEngine dbEngine;

	@Autowired
	private GeneralInformationDao generalInformationDao;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	private static final Boolean TRUE = true;
	private static final Boolean FALSE = false;
	private static final String QUESTIONNAIRE_ID = "QUESTIONNAIRE_ID";
	private static final String QUESTIONNAIRE_NUMBER = "QUESTIONNAIRE_NUMBER";
	private static final String QUESTION_ID = "QUESTION_ID";
	private static final String QUESTION_NUMBER = "QUESTION_NUMBER";
	private static final String QUESTION_CONDITION_ID = "QUESTION_CONDITION_ID";
	private static final String QUESTION_OPTION_ID = "QUESTION_OPTION_ID";
	private static final String QUESTIONNAIRE_ANSWER_HEADER_ID = "QUESTIONNAIRE_ANS_HEADER_ID";
	private static final String QUESTIONNAIRE_ANSWER_ID = "QUESTIONNAIRE_ANSWER_ID";
	private static final String QUESTIONNAIRE_USAGE_ID = "QUESTIONNAIRE_USAGE_ID";
	private static final String IS_FINAL = "IS_FINAL";
	private static final String QUESTIONNAIRE_ID_PARAM = "<<QUESTIONNAIRE_ID>>";
	private static final String IS_FINAL_PARAM = "<<IS_FINAL>>";
	private static final String UPDATE_TIMESTAMP = "<<UPDATE_TIMESTAMP>>";
	private static final String UPDATE_USER_PARAM = "<<UPDATE_USER>>";
	private static final String UPDATE_USER = "UPDATE_USER";
	private static final String DESCRIPTION = "<<DESCRIPTION>>";
	private static final String FOOTER_DESCRIPTION = "<<FOOTER_DESCRIPTION>>";
	private static final String GROUP_NAME = "GROUP_NAME";
	private static final String QUESTION_ID_PARAM = "<<QUESTION_ID>>";
	private static final String QUESTION_OPTION_ID_PARAM = "<<QUESTION_OPTION_ID>>";
	private static final String SORT_ORDER = "SORT_ORDER";
	private static final String SORT_ORDER_PARAM = "<<SORT_ORDER>>";
	private static final String GROUP_NAME_PARAM = "<<GROUP_NAME>>";
	private static final String REQUIRE_EXPLANATION = "REQUIRE_EXPLANATION";
	private static final String VERSION_NUMBER = "VERSION_NUMBER";
	private static final String QUESTIONNAIRE_COMPLETED_FLAG = "QUESTIONNAIRE_COMPLETED_FLAG";
	private static final String QUESTIONNAIRE = "QUESTIONNAIRE";
	private static final String IS_MANDATORY = "IS_MANDATORY";
	private static final String MODULE_ITEM_CODE = "MODULE_ITEM_CODE";
	private static final String MODULE_ITEM_CODE_PARAM = "<<MODULE_ITEM_CODE>>";
	private static final String MODULE_SUB_ITEM_CODE_PARAM = "<<MODULE_SUB_ITEM_CODE>>";
	private static final String MODULE_SUB_ITEM_CODE = "MODULE_SUB_ITEM_CODE";
	private static final String RULE_ID = "RULE_ID";
	private static final String QUESTIONNAIRE_USAGE_ID_PARAM = "<<QUESTIONNAIRE_USAGE_ID>>";
	private static final String QUESTION_ID_DEL_PARAM = "<<question_id>>";
	private static final String QUESTIONNAIRE_LABEL = "QUESTIONNAIRE_LABEL";
	private static final String QUESTIONNAIRE_NUMBER_PARAM = "<<QUESTIONNAIRE_NUMBER>>";
	private static final String QUESTIONNAIRE_ANSWER_TABLE_ID = "QUESTIONNAIRE_ANSWER_TABLE_ID";
	private static final String QUESTIONNAIRE_ANS_HEADER_ID = "<<AV_QNR_ANS_HEADER_ID>>";

    public synchronized Integer getNextQuestionnaireId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_ID", QUESTIONNAIRE_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_ID", QUESTIONNAIRE_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionnaireId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionnaireNumber() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_NUMBER", QUESTIONNAIRE_NUMBER);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_NUMBER", QUESTIONNAIRE_NUMBER, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionnaireNumber : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getMaxQuestionnaireVersionNumber(Integer questionnaireNumber) {
		Integer maxVersionNumber = null;
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(
					new Parameter(QUESTIONNAIRE_NUMBER_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireNumber));
			ArrayList<HashMap<String, Object>> result = dbEngine.executeQuery(inputParam, "UPDATE_MAX_QUESTIONNAIRE_VERSION_NUMBER");
			HashMap<String, Object> hmResult = result.get(0);
			if (hmResult.get("MAX_VERSION_NUMBER") == null) {
				return 0;
			} else {
				maxVersionNumber = Integer.parseInt(hmResult.get("MAX_VERSION_NUMBER").toString());
			}
			return maxVersionNumber;
		} catch (Exception e) {
			logger.error("Exception in getMaxQuestionnaireVersionNumber : {} ", e.getMessage());
			return null;
		}
	}

	public synchronized Integer getNextQuestionId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTION_ID", QUESTION_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTION_ID", QUESTION_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionNumber() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTION_NUMBER", QUESTION_NUMBER);
			updateMaxColumnValue("UPDATE_MAX_QUESTION_NUMBER", QUESTION_NUMBER, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionNumber : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionConditionId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTION_CONDITION_ID", QUESTION_CONDITION_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTION_CONDITION_ID", QUESTION_CONDITION_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionConditionId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionOptionId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTION_OPTION_ID", QUESTION_OPTION_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTION_OPTION_ID", QUESTION_OPTION_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionOptionId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionAnsHeaderId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_ANS_HEADER_ID", QUESTIONNAIRE_ANSWER_HEADER_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_ANS_HEADER_ID", QUESTIONNAIRE_ANSWER_HEADER_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionAnsHeaderId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionAnswerId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_ANSWER_ID", QUESTIONNAIRE_ANSWER_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_ANSWER_ID", QUESTIONNAIRE_ANSWER_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionAnswerId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionAnswerAttachId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_ANSWER_ATT_ID", "QUESTIONNAIRE_ANSWER_ATT_ID");
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_ANSWER_ATT_ID", "QUESTIONNAIRE_ANSWER_ATT_ID", columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionAnswerAttachId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionnaireUsageId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_USAGE_ID", QUESTIONNAIRE_USAGE_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_USAGE_ID", QUESTIONNAIRE_USAGE_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionnaireUsageId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionnaireAnswerHeaderId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_ANS_HEADER_ID", QUESTIONNAIRE_ANSWER_HEADER_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_ANS_HEADER_ID", QUESTIONNAIRE_ANSWER_HEADER_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionnaireAnswerHeaderId : {} ", e.getMessage());
			return 0;
		}
	}

	public synchronized Integer getNextQuestionnaireAnswerId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_ANSWER_ID", QUESTIONNAIRE_ANSWER_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_ANSWER_ID", QUESTIONNAIRE_ANSWER_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionnaireAnswerHeaderId : {} ", e.getMessage());
			return 0;
		}
	}

	private Integer getMaxColumnValue(String sqlId, String columnName) {
		Integer columnId = null;
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			ArrayList<HashMap<String, Object>> result = dbEngine.executeQuery(inputParam, sqlId);
			HashMap<String, Object> hmResult = result.get(0);
			if (hmResult.get(columnName) == null) {
				return 0;
			} else {
				columnId = Integer.parseInt(hmResult.get(columnName).toString());
			}
			return ++columnId;
		} catch (Exception e) {
			logger.error("Exception in getMaxColumnValue : {} ", e.getMessage());
			return null;
		}
	}

	private void updateMaxColumnValue(String sqlId, String columnName, Integer maxColumnValue) {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter(columnName, DBEngineConstants.TYPE_INTEGER, maxColumnValue));
			dbEngine.executeUpdate(inputParam, sqlId);
		} catch (Exception e) {
			logger.error("Exception in updateMaxColumnValue : {} ", e.getMessage());
		}
	}

	public void insertHeader(QuestionnaireDataBus questionnaireDataBus) {
		try {
			Map<String, Object> hmHeader = questionnaireDataBus.getHeader();
			String isFinal = hmHeader.get(IS_FINAL) != null && hmHeader.get(IS_FINAL).toString().equalsIgnoreCase("true")  ? "Y" : "N";
			isFinal = (isFinal.equals("Y") && questionnaireDataBus.isNewQuestionnaireVersion() ? "N" : isFinal);
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmHeader.get(QUESTIONNAIRE_ID)));
			inParam.add(new Parameter(QUESTIONNAIRE_NUMBER_PARAM, DBEngineConstants.TYPE_INTEGER, hmHeader.get(QUESTIONNAIRE_NUMBER)));
			inParam.add(new Parameter("<<VERSION_NUMBER>>", DBEngineConstants.TYPE_INTEGER, hmHeader.get("QUESTIONNAIRE_VERSION")));
			inParam.add(new Parameter("<<QUESTIONNAIRE>>", DBEngineConstants.TYPE_STRING, hmHeader.get("QUESTIONNAIRE_NAME")));
			inParam.add(new Parameter(DESCRIPTION, DBEngineConstants.TYPE_STRING, hmHeader.get("QUESTIONNAIRE_DESCRIPTION")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmHeader.get(UPDATE_USER)));
			inParam.add(new Parameter("<<QUEST_GROUP_TYPE_CODE>>", DBEngineConstants.TYPE_STRING, hmHeader.get("QUEST_GROUP_TYPE_CODE")));
			inParam.add(new Parameter(IS_FINAL_PARAM, DBEngineConstants.TYPE_STRING, isFinal));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTIONNAIRE_HEADER");
		} catch (Exception e) {
			logger.error("Exception in insertHeader : {} ", e.getMessage());
		}
	}

	public void updateHeader(QuestionnaireDataBus questionnaireDataBus) {
		try {
			Map<String, Object> hmHeader = questionnaireDataBus.getHeader();
			String isFinal = hmHeader.get(IS_FINAL) != null && hmHeader.get(IS_FINAL).toString().equalsIgnoreCase("true")  ? "Y" : "N";
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUESTIONNAIRE>>", DBEngineConstants.TYPE_STRING, hmHeader.get("QUESTIONNAIRE_NAME")));
			inParam.add(new Parameter(DESCRIPTION, DBEngineConstants.TYPE_STRING, hmHeader.get("QUESTIONNAIRE_DESCRIPTION")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmHeader.get(UPDATE_USER)));
			inParam.add(new Parameter("<<QUEST_GROUP_TYPE_CODE>>", DBEngineConstants.TYPE_STRING, hmHeader.get("QUEST_GROUP_TYPE_CODE")));
			inParam.add(new Parameter(IS_FINAL_PARAM, DBEngineConstants.TYPE_STRING, (isFinal)));
			inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmHeader.get(QUESTIONNAIRE_ID)));
			if (isFinal.equals("Y") && hmHeader.get(QUESTIONNAIRE_NUMBER) != null) {
				deActivateQuestionnaire(hmHeader.get(QUESTIONNAIRE_NUMBER), hmHeader.get(QUESTIONNAIRE_ID));
			}
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTIONNAIRE_HEADER");
		} catch (Exception e) {
			logger.error("Exception in updateHeader : {}", e.getMessage());
		}
	}

	public void insertQuestion(Map<String, Object> hmQuestion) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmQuestion.get(QUESTION_ID)));
			inParam.add(new Parameter("<<QUESTION_NUMBER>>", DBEngineConstants.TYPE_INTEGER, hmQuestion.get(QUESTION_NUMBER)));
			inParam.add(new Parameter("<<QUESTION_VERSION_NUMBER>>", DBEngineConstants.TYPE_INTEGER, hmQuestion.get("QUESTION_VERSION_NUMBER")));
			inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmQuestion.get(QUESTIONNAIRE_ID)));
			inParam.add(new Parameter(SORT_ORDER_PARAM, DBEngineConstants.TYPE_INTEGER, hmQuestion.get(SORT_ORDER)));
			inParam.add(new Parameter("<<QUESTION>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("QUESTION")));
			inParam.add(new Parameter(DESCRIPTION, DBEngineConstants.TYPE_STRING, hmQuestion.get("DESCRIPTION")));
			inParam.add(new Parameter(FOOTER_DESCRIPTION, DBEngineConstants.TYPE_STRING, hmQuestion.get("FOOTER_DESCRIPTION")));
			inParam.add(new Parameter("<<PARENT_QUESTION_ID>>", DBEngineConstants.TYPE_INTEGER, hmQuestion.get("PARENT_QUESTION_ID")));
			inParam.add(new Parameter("<<HELP_LINK>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("HELP_LINK")));
			inParam.add(new Parameter("<<ANSWER_TYPE>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("ANSWER_TYPE")));
			inParam.add(new Parameter("<<ANSWER_LENGTH>>", DBEngineConstants.TYPE_INTEGER, hmQuestion.get("ANSWER_LENGTH")));
			inParam.add(new Parameter("<<NO_OF_ANSWERS>>", DBEngineConstants.TYPE_INTEGER, hmQuestion.get("NO_OF_ANSWERS")));
			inParam.add(new Parameter("<<LOOKUP_TYPE>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("LOOKUP_TYPE")));
			inParam.add(new Parameter("<<LOOKUP_NAME>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("LOOKUP_NAME")));
			inParam.add(new Parameter("<<LOOKUP_FIELD>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("LOOKUP_FIELD")));
			inParam.add(new Parameter(GROUP_NAME_PARAM, DBEngineConstants.TYPE_STRING, hmQuestion.get(GROUP_NAME)));
			inParam.add(new Parameter("<<GROUP_LABEL>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("GROUP_LABEL")));
			inParam.add(new Parameter("<<HAS_CONDITION>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("HAS_CONDITION")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmQuestion.get(UPDATE_USER)));
			inParam.add(new Parameter(RULE_ID, DBEngineConstants.TYPE_INTEGER, hmQuestion.get(RULE_ID)));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTION");
		} catch (Exception e) {
			logger.error("Exception in insertQuestion : {} ", e.getMessage());
		}

	}

	public void updateQuestion(Map<String, Object> hmQuestion) {
		try {			
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUESTION>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("QUESTION")));
			inParam.add(new Parameter(DESCRIPTION, DBEngineConstants.TYPE_STRING, hmQuestion.get("DESCRIPTION")));
			inParam.add(new Parameter(FOOTER_DESCRIPTION, DBEngineConstants.TYPE_STRING, hmQuestion.get("FOOTER_DESCRIPTION")));
			inParam.add(new Parameter("<<PARENT_QUESTION_ID>>", DBEngineConstants.TYPE_INTEGER,	hmQuestion.get("PARENT_QUESTION_ID")));
			inParam.add(new Parameter("<<HELP_LINK>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("HELP_LINK")));
			inParam.add(new Parameter("<<ANSWER_TYPE>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("ANSWER_TYPE")));
			inParam.add(new Parameter("<<ANSWER_LENGTH>>", DBEngineConstants.TYPE_INTEGER,hmQuestion.get("ANSWER_LENGTH")));
			inParam.add(new Parameter("<<NO_OF_ANSWERS>>", DBEngineConstants.TYPE_INTEGER,hmQuestion.get("NO_OF_ANSWERS")));
			inParam.add(new Parameter("<<LOOKUP_TYPE>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("LOOKUP_TYPE")));
			inParam.add(new Parameter("<<LOOKUP_NAME>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("LOOKUP_NAME")));
			inParam.add(new Parameter("<<LOOKUP_FIELD>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("LOOKUP_FIELD")));
			inParam.add(new Parameter(GROUP_NAME_PARAM, DBEngineConstants.TYPE_STRING, hmQuestion.get(GROUP_NAME)));
			inParam.add(new Parameter("<<GROUP_LABEL>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("GROUP_LABEL")));
			inParam.add(new Parameter("<<HAS_CONDITION>>", DBEngineConstants.TYPE_STRING, hmQuestion.get("HAS_CONDITION")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmQuestion.get(UPDATE_USER)));
			inParam.add(new Parameter(SORT_ORDER_PARAM, DBEngineConstants.TYPE_INTEGER, hmQuestion.get(SORT_ORDER)));
			inParam.add(new Parameter(RULE_ID, DBEngineConstants.TYPE_INTEGER, hmQuestion.get(RULE_ID)));
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmQuestion.get(QUESTION_ID)));
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTION");
		} catch (Exception e) {
			logger.error("Exception in updateQuestion : {} ", e.getMessage());
		}
	}

	public void deleteQuestion(Integer questionID) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_ID_DEL_PARAM, DBEngineConstants.TYPE_INTEGER, questionID));
			dbEngine.executeUpdate(inParam, "delete_questionnaire_question");
		} catch (Exception e) {
			logger.error("Exception in deleteQuestion : {} ", e.getMessage());
		}
	}

	public void insertCondition(Map<String, Object> hmCondition) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUESTION_CONDITION_ID>>", DBEngineConstants.TYPE_INTEGER, hmCondition.get(QUESTION_CONDITION_ID)));
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmCondition.get(QUESTION_ID)));
			inParam.add(new Parameter("<<CONDITION_TYPE>>", DBEngineConstants.TYPE_STRING, hmCondition.get("CONDITION_TYPE")));
			inParam.add(new Parameter("<<CONDITION_VALUE>>", DBEngineConstants.TYPE_STRING, hmCondition.get("CONDITION_VALUE")));
			inParam.add(new Parameter(GROUP_NAME_PARAM, DBEngineConstants.TYPE_STRING, hmCondition.get(GROUP_NAME)));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmCondition.get(UPDATE_USER)));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTION_CONDITION");
		} catch (Exception e) {
			logger.error("Exception in insertCondition : {} ", e.getMessage() + hmCondition.get(QUESTION_ID));
		}
	}

	public void updateCondition(Map<String, Object> hmCondition) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<CONDITION_TYPE>>", DBEngineConstants.TYPE_STRING, hmCondition.get("CONDITION_TYPE")));
			inParam.add(new Parameter("<<CONDITION_VALUE>>", DBEngineConstants.TYPE_STRING, hmCondition.get("CONDITION_VALUE")));
			inParam.add(new Parameter(GROUP_NAME_PARAM, DBEngineConstants.TYPE_STRING, hmCondition.get(GROUP_NAME)));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmCondition.get(UPDATE_USER)));
			inParam.add(new Parameter("<<QUESTION_CONDITION_ID>>", DBEngineConstants.TYPE_INTEGER, hmCondition.get(QUESTION_CONDITION_ID)));
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTION_CONDITION");
		} catch (Exception e) {
			logger.error("Exception in updateCondition : {} ", e.getMessage());
		}
	}

	public void deleteCondition(Integer conditionId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<question_condition_id>>", DBEngineConstants.TYPE_INTEGER, conditionId));
			dbEngine.executeUpdate(inParam, "delete_condition");
		} catch (Exception e) {
			logger.error("Exception in deleteCondition : {} ", e.getMessage());
		}
	}

	public void deleteConditionForQuestion(Integer questionId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_ID_DEL_PARAM, DBEngineConstants.TYPE_INTEGER, questionId));
			dbEngine.executeUpdate(inParam, "delete_question_condition");
		} catch (Exception e) {
			logger.error("Exception in deleteConditionForQuestion : {} ", e.getMessage());
		}
	}

	public void insertOption(Map<String, Object> hmOption) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_OPTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmOption.get(QUESTION_OPTION_ID)));
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmOption.get(QUESTION_ID)));
			inParam.add(new Parameter("<<OPTION_NUMBER>>", DBEngineConstants.TYPE_INTEGER, hmOption.get("OPTION_NUMBER")));
			inParam.add(new Parameter("<<OPTION_LABEL>>", DBEngineConstants.TYPE_STRING, hmOption.get("OPTION_LABEL")));
			inParam.add(new Parameter("<<REQUIRE_EXPLANATION>>", DBEngineConstants.TYPE_STRING, (hmOption.get(REQUIRE_EXPLANATION) == null ? "N":hmOption.get(REQUIRE_EXPLANATION))));
			inParam.add(new Parameter("<<EXPLANTION_LABEL>>", DBEngineConstants.TYPE_STRING, hmOption.get("EXPLANTION_LABEL")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmOption.get(UPDATE_USER)));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTION_OPTION");
		} catch (Exception e) {
			logger.error("Exception in insertOption : {} ", e.getMessage() + hmOption.get(QUESTION_ID));
		}

	}

	public void updateOption(Map<String, Object> hmOption) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<OPTION_LABEL>>", DBEngineConstants.TYPE_STRING, hmOption.get("OPTION_LABEL")));
			inParam.add(new Parameter("<<REQUIRE_EXPLANATION>>", DBEngineConstants.TYPE_STRING, hmOption.get(REQUIRE_EXPLANATION)));
			inParam.add(new Parameter("<<EXPLANTION_LABEL>>", DBEngineConstants.TYPE_STRING, hmOption.get("EXPLANTION_LABEL")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmOption.get(UPDATE_USER)));
			inParam.add(new Parameter(QUESTION_OPTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmOption.get(QUESTION_OPTION_ID)));
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTION_OPTION");
		} catch (Exception e) {
			logger.error("Exception in updateOption : {} ", e.getMessage());
		}
	}

	public void deleteOption(Integer optionId) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_OPTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, optionId));
			dbEngine.executeUpdate(inParam, "delete_option");
		} catch (Exception e) {
			logger.error("Exception in deleteOption : {} ", e.getMessage());
		}
	}

	public void deleteOptionForQuestion(Integer questionID) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_ID_DEL_PARAM, DBEngineConstants.TYPE_INTEGER, questionID));
			dbEngine.executeUpdate(inParam, "delete_question_option");
		} catch (Exception e) {
			logger.error("Exception in delete_question_option : {} ", e.getMessage());
		}
	}

	public void insertUsage(Map<String, Object> hmUsage) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			String isMandatory = Boolean.TRUE.equals(hmUsage.get(IS_MANDATORY))? "Y":"N";
			inParam.add(new Parameter(QUESTIONNAIRE_USAGE_ID_PARAM, DBEngineConstants.TYPE_INTEGER,hmUsage.get(QUESTIONNAIRE_USAGE_ID)));
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER,Integer.parseInt(hmUsage.get(MODULE_ITEM_CODE).toString())));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER,(hmUsage.get(MODULE_SUB_ITEM_CODE) == null ? 0 :Integer.parseInt(hmUsage.get(MODULE_SUB_ITEM_CODE).toString()))));
			inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmUsage.get(QUESTIONNAIRE_ID)));
			inParam.add(new Parameter("<<QUESTIONNAIRE_LABEL>>", DBEngineConstants.TYPE_STRING,hmUsage.get(QUESTIONNAIRE_LABEL)));
			inParam.add(new Parameter("<<IS_MANDATORY>>", DBEngineConstants.TYPE_STRING,(hmUsage.get(IS_MANDATORY) == null ? "N": isMandatory)));
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_STRING,hmUsage.get(RULE_ID)));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmUsage.get(UPDATE_USER)));
			inParam.add(new Parameter(SORT_ORDER_PARAM, DBEngineConstants.TYPE_INTEGER, hmUsage.get(SORT_ORDER)));
			dbEngine.executeUpdate(inParam, "insert_questionnaire_usage");
		} catch (Exception e) {
			logger.error("Exception in insertUsage : {} ", e.getMessage());
		}
	}

	public void deleteUsage(Map<String, Object> hmUsage) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTIONNAIRE_USAGE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, hmUsage.get(QUESTIONNAIRE_USAGE_ID)));
			dbEngine.executeUpdate(inParam, "delete_questionnaire_usage");
		} catch (Exception e) {
			logger.error("Exception in deleteUsage : {} ", e.getMessage());
		}
	}

	public void updateUsage(Map<String, Object> hmUsage) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER,Integer.parseInt(hmUsage.get(MODULE_ITEM_CODE).toString())));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER,(hmUsage.get(MODULE_SUB_ITEM_CODE) == null ? 0 
						:Integer.parseInt(hmUsage.get(MODULE_SUB_ITEM_CODE).toString()))));
			inParam.add(new Parameter("<<QUESTIONNAIRE_LABEL>>", DBEngineConstants.TYPE_STRING,hmUsage.get(QUESTIONNAIRE_LABEL)));
			inParam.add(new Parameter("<<IS_MANDATORY>>", DBEngineConstants.TYPE_STRING,(Boolean.TRUE.equals(hmUsage.get(IS_MANDATORY))? "Y":"N")));
			inParam.add(new Parameter("<<RULE_ID>>", DBEngineConstants.TYPE_STRING,hmUsage.get(RULE_ID)));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, hmUsage.get(UPDATE_USER)));
			inParam.add(new Parameter(QUESTIONNAIRE_USAGE_ID_PARAM, DBEngineConstants.TYPE_INTEGER,hmUsage.get(QUESTIONNAIRE_USAGE_ID)));
			dbEngine.executeUpdate(inParam, "update_questionnaire_usage");
		} catch (Exception e) {
			logger.error("Exception in updateUsage : {} ", e.getMessage());
		}
	}

	public List<HashMap<String, Object>> getApplicableQuestionnaireData(String moduleItemKey,String moduleSubItemKey, Integer moduleItemCode, Integer moduleSubItemCode, String logginPersonId, String updateUser, String questionaireMode) throws Exception {	
		List<HashMap<String, Object>> allApplicableQNR = getAllApplicableQuestionnaire(moduleItemKey, moduleSubItemKey, moduleItemCode, moduleSubItemCode);
		List<HashMap<String, Object>> ruleFilterQNR = evalueQuestionnaireRule(allApplicableQNR,moduleItemKey, moduleItemCode, moduleSubItemCode,logginPersonId,  updateUser, moduleSubItemKey);
		List<HashMap<String, Object>> answeredQNR = getAnsweredQuestionnaire(moduleItemCode, moduleSubItemCode, moduleItemKey, moduleSubItemKey);
		return consolidatedApplicableQuestionnaire(ruleFilterQNR,answeredQNR);
	}

	private List<HashMap<String, Object>> consolidatedApplicableQuestionnaire(
			List<HashMap<String, Object>> ruleFilterQNR, List<HashMap<String, Object>> answeredQNR) {
		ArrayList<HashMap<String, Object>> applicableQNR = new ArrayList<>();
		if (ruleFilterQNR != null && !ruleFilterQNR.isEmpty()) {
			for (HashMap<String, Object> hmQNR : ruleFilterQNR) {
				Integer questionnaireId = (hmQNR.get(QUESTIONNAIRE_NUMBER) == null ? 0 : Integer.parseInt((hmQNR.get(QUESTIONNAIRE_NUMBER).toString())));
				Integer questionnaireVersion = (hmQNR.get(VERSION_NUMBER) == null ? 0 : Integer.parseInt(hmQNR.get(VERSION_NUMBER).toString()));
				hmQNR.put("IS_NEW_VERSION", "N");
				hmQNR.put("ANSWERED_VERSION_NUMBER", null);
				hmQNR.put("NEW_QUESTIONNAIRE_ID", null);
				hmQNR.put("NEW_QUESTIONNAIRE", null);
				hmQNR.put("NEW_QUESTIONNAIRE_LABEL", null);
				hmQNR.put("QUESTIONNAIRE_NUMBER", questionnaireId);
				if (hmQNR.get(QUESTIONNAIRE_ANSWER_HEADER_ID) == null) {
					for (HashMap<String, Object> hmAnsQNR : answeredQNR) {
						Integer ansQuestionnaireId = (hmAnsQNR.get(QUESTIONNAIRE_NUMBER) == null ? -1 : Integer.parseInt(hmAnsQNR.get(QUESTIONNAIRE_NUMBER).toString()));
						Integer ansQuestionnaireVersion = (hmAnsQNR.get(VERSION_NUMBER) == null ? -1 : Integer.parseInt(hmAnsQNR.get(VERSION_NUMBER).toString()));
						if (questionnaireId.equals(ansQuestionnaireId)) {
							if (!questionnaireVersion.equals(ansQuestionnaireVersion)) {
								hmQNR.put("IS_NEW_VERSION", "Y");
								hmQNR.put("NEW_QUESTIONNAIRE_ID", hmQNR.get(QUESTIONNAIRE_ID));
								hmQNR.put(QUESTIONNAIRE_ID, hmAnsQNR.get(QUESTIONNAIRE_ID));
								hmQNR.put(QUESTIONNAIRE_ANSWER_HEADER_ID, hmAnsQNR.get(QUESTIONNAIRE_ANSWER_HEADER_ID));
								hmQNR.put(QUESTIONNAIRE_COMPLETED_FLAG, hmAnsQNR.get(QUESTIONNAIRE_COMPLETED_FLAG));
								hmQNR.put("NEW_QUESTIONNAIRE", hmQNR.get(QUESTIONNAIRE));
								hmQNR.put(QUESTIONNAIRE, hmAnsQNR.get(QUESTIONNAIRE));
								hmQNR.put("NEW_QUESTIONNAIRE_LABEL", hmQNR.get(QUESTIONNAIRE_LABEL));
								hmQNR.put(QUESTIONNAIRE_LABEL, hmAnsQNR.get(QUESTIONNAIRE_LABEL));
							}
							hmQNR.put("ANSWERED_VERSION_NUMBER", hmAnsQNR.get(VERSION_NUMBER));
							hmQNR.put("QUESTIONNAIRE_NUMBER", ansQuestionnaireId);
							break;
						}
					}
				}
				hmQNR.remove(RULE_ID);
				applicableQNR.add(hmQNR);
			}
		}
		return applicableQNR.stream().distinct().collect(Collectors.toList());
	}

	public List<HashMap<String, Object>> evalueQuestionnaireRule(List<HashMap<String, Object>> allApplicableQNR, String moduleItemKey,
			Integer moduleItemCode, Integer moduleSubItemCode, String logginPersonId, String updateUser, String moduleSubItemKey) {	
		ArrayList<HashMap<String, Object>> applicableQNR = new ArrayList<>();
		if(allApplicableQNR != null && !allApplicableQNR.isEmpty()) {
			for(HashMap<String, Object> hmQNR: allApplicableQNR ) {
				if(hmQNR.get(QUESTIONNAIRE_COMPLETED_FLAG) == null){
					Integer ruleId =  (hmQNR.get(RULE_ID) == null ? 0: Integer.parseInt((String) hmQNR.get(RULE_ID)));	
					if(ruleId == 0) {
						applicableQNR.add(hmQNR);
					}else {
						boolean isrulePassed = generalInformationDao.evaluateRule(moduleItemCode, moduleSubItemCode,
								moduleItemKey, ruleId, logginPersonId, updateUser, moduleSubItemKey != null  && !moduleSubItemKey.isEmpty() ? moduleSubItemKey : Constants.SUBMODULE_ITEM_KEY);
						if(isrulePassed) {
							applicableQNR.add(hmQNR);
						}	
					}	
				}else {
					applicableQNR.add(hmQNR);
				}
			}
		}		
		return applicableQNR;
	}

	private ArrayList<HashMap<String, Object>> getAllApplicableQuestionnaire(String moduleItemKey,String moduleSubItemKey,
			Integer moduleItemCode, Integer moduleSubItemCode) throws Exception {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, moduleItemKey));
	        inParam.add(new Parameter("<<MODULE_SUB_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, moduleSubItemKey));
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			output = dbEngine.executeQuery(inParam, "GET_APPLICABLE_QUESTIONNAIRE");		
		} catch (Exception e) {
			logger.error("Exception in getAllApplicableQuestionnaire : {} ", e.getMessage());

		}
		return output;
	}

	public List<HashMap<String, Object>> getQuestionnaireQuestion(Integer questionnaireId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();		
		inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireId));
		try {
			output = dbEngine.executeQuery(inParam, "GET_QUESTIONNAIRE_QUESTIONS");			
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireQuestion : {}", e.getMessage());
		}
		return output;
	}

	public List<HashMap<String, Object>> getQuestionnaireUsage(Integer questionnaireId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireId));
		try {
			output = dbEngine.executeQuery(inParam, "GET_QUESTIONNAIRE_HEADER_USAGE");
			if (output != null && !output.isEmpty()) {
			output.forEach(hmResult -> {	
				hmResult.put("AC_TYPE", "U");
					if ("Y".equals(hmResult.get(IS_MANDATORY))) {
						hmResult.put(IS_MANDATORY, TRUE);
					} else {
						hmResult.put(IS_MANDATORY, FALSE);
					}
			});
			}			
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireUsage : {} ", e.getMessage());
		}
		return output;
	}

	public List<HashMap<String, Object>> getQuestionnaireHeader(Integer questionnaireId, Integer questionnaireAnswerHeaderId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(QUESTIONNAIRE_ANS_HEADER_ID, DBEngineConstants.TYPE_INTEGER, questionnaireAnswerHeaderId));
		inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireId));
		try {
			output = dbEngine.executeQuery(inParam, "GET_QUESTIONNAIRE_HEADER_INFO");
			if (output != null && !output.isEmpty()) {
				output.forEach(hmResult -> {	
					hmResult.put("AC_TYPE", "U");
					if ("Y".equals(hmResult.get(IS_FINAL))) {
						hmResult.put(IS_FINAL, TRUE);
					} else {
						hmResult.put(IS_FINAL, FALSE);
					}
				});
			}
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireHeader : {} ", e.getMessage());
		}
		return output;
	}

	public List<HashMap<String, Object>> getAllQuestionnaire() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_ALL_QUESTIONNAIRE");	
		} catch (Exception e) {
			logger.error("Exception in getAllQuestionnaire : {} ", e.getMessage());
		}
		return output;
	}

	public List<HashMap<String, Object>> getQuestionnaireGroup() {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_QUESTIONNAIRE_GROUP");
		} catch (Exception e) {
			logger.error("Exception in getAllQuestionnaire : {} ", e.getMessage());
		}
		return output;
	}

	public List<HashMap<String, Object>> getQuestionnaireCondition(Integer questionnaireId)
			throws Exception {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter("<<AV_QUESTIONNAIRE_ID>>", DBEngineConstants.TYPE_INTEGER, questionnaireId));
			return dbEngine.executeQuery(inputParam, "GET_QUESTIONNAIRE_CONDITIONS");
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireQuestionsCondition : {} ", e.getMessage());
			return new ArrayList<>();
		}
	}

	public List<HashMap<String, Object>> getQuestionnaireOptions(Integer questionnaireId)
			throws Exception {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter("<<AV_QUESTIONNAIRE_ID>>", DBEngineConstants.TYPE_INTEGER, questionnaireId));
			return dbEngine.executeQuery(inputParam, "GET_QUESTIONNAIRE_OPTIONS");
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireQuestionsOptions : {} ", e.getMessage());
			return new ArrayList<>();
		}
	}

	public List<HashMap<String, Object>> getQuestionnaireAttachment(Integer questionnaireAnsAttachmentId, Integer moduleCode) 
		throws Exception {
			try {
				String genericSQL = Constants.COI_MODULE_CODE.equals(moduleCode) ? "GET_COI_QUESTIONNAIRE_ATTACHMENT"
						: "GET_QUESTIONNAIRE_ATTACHMENT";
				ArrayList<Parameter> inputParam = new ArrayList<>();
				inputParam.add(new Parameter("<<AV_QUESTIONNAIRE_ANS_ATTACHMENT_ID>>", DBEngineConstants.TYPE_INTEGER, questionnaireAnsAttachmentId));
				return dbEngine.executeQuery(inputParam, genericSQL);
			} catch (Exception e) {
				logger.error("Exception in getQuestionnaireQuestionsOptions : {} ", e.getMessage());
				return new ArrayList<>();
			}
		}

	public List<HashMap<String, Object>> getBusinessRuleForQuestionnaire(Integer moduleItemCode,
			Integer moduleSubItemCode) throws Exception {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			output = dbEngine.executeQuery(inParam, "GET_BUSINESS_RULE_FOR_QUESTIONNAIRE");
		} catch (Exception e) {
			logger.error("Exception in getBusinessRuleForQuestionnaire : {} ", e.getMessage());

		}
		return output;
	}

	private ArrayList<HashMap<String, Object>> getAnsweredQuestionnaire(Integer moduleItemCode, Integer moduleSubItemCode, String moduleItemKey,String moduleSubItemKey) throws Exception {		
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, moduleItemKey));
	        inParam.add(new Parameter("<<MODULE_SUB_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, moduleSubItemKey));
	        inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
	        inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			output = dbEngine.executeQuery(inParam, "GET_ANSWERED_QUESTIONNAIRE");		
		} catch (Exception e) {
			logger.error("Exception in getAnsweredQuestionnaire : {} ", e.getMessage());
		}
		return output;
	}

	public void activeDeactivateQuestionnaire(QuestionnaireDataBus questionnaireDataBus, String isFinal) throws Exception{
		try {		
			ArrayList<Parameter> inParam = new ArrayList<>();			
			inParam.add(new Parameter(IS_FINAL_PARAM, DBEngineConstants.TYPE_STRING, isFinal));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, AuthenticatedUser.getLoginUserName()));
			inParam.add(new Parameter(UPDATE_TIMESTAMP, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireDataBus.getQuestionnaireId()));
			dbEngine.executeUpdate(inParam, "ACTIVATE_DEACTIVATE_QUESTIONNAIRE");
		} catch (Exception e) {
			logger.error("Exception in activeDeactivateQuestionnaire : {} ", e.getMessage());
		}
	}

	public List<HashMap<String, Object>> getModuleList() {
		List<HashMap<String, Object>> output = new ArrayList<>();
		try {		
			output = dbEngine.executeQuery(new ArrayList<>(), "GET_QUEST_MODULE_SUBMODULE_CODE");
		} catch (Exception e) {
			logger.error("Exception in getModuleList : {} ", e.getMessage());
		}
		return output;
	}

	public void updateUsageForSortOrder(Map<String, Object> questionnaireUsage, Integer moduleItemCode, Integer moduleSubItemCode) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(SORT_ORDER_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireUsage.get(SORT_ORDER)));
			inParam.add(new Parameter(QUESTIONNAIRE_NUMBER_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireUsage.get(QUESTIONNAIRE_NUMBER)));
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTIONNAIRE_USAGE_FOR_SORT");
		} catch (Exception e) {
			logger.error("Exception in updateUsageForSortOrder : {}", e.getMessage());
		}
	}

	public Integer getNextSortOrder(Integer moduleItemCode, Integer moduleSubItemCode) {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
			inputParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			ArrayList<HashMap<String, Object>> result = dbEngine.executeQuery(inputParam, "GET_MAX_QUESTIONNAIRE_SORT_ORDER");
			HashMap<String, Object> hmResult = result.get(0);
			Integer maxSortOrder = hmResult.get("MAX_SORT_ORDER") == null ? 0 : Integer.parseInt(hmResult.get("MAX_SORT_ORDER").toString());
			return ++maxSortOrder;
		} catch (Exception e) {
			logger.error("Exception in getNextSortOrder :{}", e.getMessage());
			return 0;
		}
	}

	public List<HashMap<String, Object>> getQuestionnaireListByModule(Integer moduleItemCode, Integer moduleSubItemCode) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			output = dbEngine.executeQuery(inParam, "GET_ALL_QUESTIONNAIRE_BY_MODULE");	
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireListByModule : {} ", e.getMessage());
		}
		return output;
	}

	public void deActivateQuestionnaire(Object questionnaireNumber, Object questionnaireId) {
		try {		
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTIONNAIRE_NUMBER_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireNumber));
			inParam.add(new Parameter(QUESTIONNAIRE_ID_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireId));
			dbEngine.executeUpdate(inParam, "DEACTIVATE_QUESTIONNAIRE");
		} catch (Exception e) {
			logger.error("Exception in deActivateQuestionnaire : {} ", e.getMessage());
		}	
	}

	public List<QuestAnswerHeader> getQuestionanswerHeadersToCopy(Integer moduleCode, List<Integer> moduleSubItemCode, String moduleItemKey, String moduleSubItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<QuestAnswerHeader> query = builder.createQuery(QuestAnswerHeader.class);
		Root<QuestAnswerHeader> rootQuestAnswerHeader = query.from(QuestAnswerHeader.class);
		Predicate moduleCodePredicate = builder.equal(rootQuestAnswerHeader.get("moduleItemCode"), moduleCode);
		Predicate subModulePredicate = rootQuestAnswerHeader.get("moduleSubItemCode").in(moduleSubItemCode);
		Predicate moduleItemKeyPredicate = builder.equal(rootQuestAnswerHeader.get("moduleItemKey"), moduleItemKey);
		query.where(builder.and(moduleCodePredicate, subModulePredicate, moduleItemKeyPredicate));
		return session.createQuery(query).getResultList();
	}

	public void copyQuestionAnswers(QuestAnswerHeader copyAnswerHeader) {
		hibernateTemplate.saveOrUpdate(copyAnswerHeader);
	}

	public void deleteQuestAnswerAttachment(String moduleItemKey, Integer moduleItemCode, Integer moduleSubItemCode, String subModuleItemKey) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();
			if(Constants.COI_MODULE_CODE.equals(moduleItemCode)) {
				hqlQuery.append("delete CoiQuestAnswerAttachment t1 where t1.questAnswer.questAnswerId in (select t2.questAnswerId from CoiQuestAnswer t2 ");
			} else {
				hqlQuery.append("delete QuestAnswerAttachment t1 where t1.questAnswer.questAnswerId in (select t2.questAnswerId from QuestAnswer t2 ");
			}
			hqlQuery.append("where t2.questAnswerHeader.questAnsHeaderId in (select t3.questAnsHeaderId from QuestAnswerHeader t3 where t3.moduleItemKey=:moduleItemKey ");
			hqlQuery.append("and t3.moduleItemCode =:moduleItemCode and t3.moduleSubItemCode =:moduleSubItemCode and t3.moduleSubItemKey =:moduleSubItemKey)) ");
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("moduleItemKey", moduleItemKey);
			query.setParameter("moduleItemCode", moduleItemCode);
			query.setParameter("moduleSubItemCode", moduleSubItemCode);
			query.setParameter("moduleSubItemKey", subModuleItemKey);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("error occured in deleteQuestAnswerAttachment : {}", e.getMessage());
		}
	}

	public void deleteQuestAnswer(String moduleItemKey, Integer moduleItemCode, Integer moduleSubItemCode, String subModuleItemKey) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();
			if(Constants.COI_MODULE_CODE.equals(moduleItemCode)) {
				hqlQuery.append("delete CoiQuestAnswer t1 where t1.questAnswerHeader.questAnsHeaderId in (select t2.questAnsHeaderId from QuestAnswerHeader ");
			} else {
				hqlQuery.append("delete QuestAnswer t1 where t1.questAnswerHeader.questAnsHeaderId in (select t2.questAnsHeaderId from QuestAnswerHeader ");
			}
			hqlQuery.append("t2 where t2.moduleItemKey=:moduleItemKey and t2.moduleItemCode =:moduleItemCode and t2.moduleSubItemCode =:moduleSubItemCode and t2.moduleSubItemKey =:moduleSubItemKey) ");
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("moduleItemKey", moduleItemKey);
			query.setParameter("moduleItemCode", moduleItemCode);
			query.setParameter("moduleSubItemCode", moduleSubItemCode);
			query.setParameter("moduleSubItemKey", subModuleItemKey);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("error occured in deleteQuestAnswer : {}", e.getMessage());
		}
	}

	public void deleteQuestAnswerHeader(String moduleItemKey, Integer moduleItemCode, Integer moduleSubItemCode, String subModuleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<QuestAnswerHeader> delete = builder.createCriteriaDelete(QuestAnswerHeader.class);
		Root<QuestAnswerHeader> root = delete.from(QuestAnswerHeader.class);
		Predicate predicateModuleItemKey = builder.equal(root.get("moduleItemKey"), moduleItemKey);
		Predicate predicateModuleItemCode = builder.equal(root.get("moduleItemCode"), moduleItemCode);
		Predicate predicateModuleSubItemCode = builder.equal(root.get("moduleSubItemCode"), moduleSubItemCode);
		Predicate predicateModuleSubItemkey = builder.equal(root.get("moduleSubItemKey"), subModuleItemKey);
		delete.where(builder.and(predicateModuleItemKey, predicateModuleItemCode, predicateModuleSubItemCode, predicateModuleSubItemkey));
		session.createQuery(delete).executeUpdate();
	}

	public void deleteQuestTableAnswers(String moduleItemKey, Integer moduleItemCode, Integer moduleSubItemCode, String subModuleItemKey) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();
			if(Constants.COI_MODULE_CODE.equals(moduleItemCode)) {
				hqlQuery.append("delete CoiQuestTableAnswer t1 where t1.questAnsHeaderId in ");
			} else {
				hqlQuery.append("delete QuestTableAnswer t1 where t1.questAnsHeaderId in ");
			}
			hqlQuery.append("(select t3.questAnsHeaderId from QuestAnswerHeader t3 where t3.moduleItemKey=:moduleItemKey ");
			hqlQuery.append("and t3.moduleItemCode =:moduleItemCode and t3.moduleSubItemCode =:moduleSubItemCode and t3.moduleSubItemKey =:moduleSubItemKey)");
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("moduleItemKey", moduleItemKey);
			query.setParameter("moduleItemCode", moduleItemCode);
			query.setParameter("moduleSubItemCode", moduleSubItemCode);
			query.setParameter("moduleSubItemKey", subModuleItemKey);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("error occurred in deleteQuestTableAnswers : {}", e.getMessage());
		}
	}

	public List<HashMap<String, Object>> getQuestionsByModule(Integer moduleItemCode, Integer moduleSubItemCode) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(MODULE_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleItemCode));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE_PARAM, DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			output = dbEngine.executeQuery(inParam, "GET_ALL_QUESTIONS_FOR_AGREEMENT");	
		} catch (Exception e) {
			logger.error("Exception in getQuestionsByModule : {} ", e.getMessage());
		}
		return output;
	}

	public Integer copyAnswerToNewVersion(Integer questionnaireAnswerHeaderId, Integer newQuestionnaireId, Integer moduleCode) {	
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String storedProcedureName = Constants.COI_MODULE_CODE.equals(moduleCode) ? "COI_UPD_ANS_TO_NEW_QUEST_VERSN"
				: "UPD_ANS_TO_NEW_QUEST_VERSN";
		return (Integer) session.createStoredProcedureCall(storedProcedureName)
		        .registerStoredProcedureParameter("AV_QUESTIONNAIRE_ID", Integer.class, ParameterMode.IN)
		        .setParameter("AV_QUESTIONNAIRE_ID", newQuestionnaireId)
		        .registerStoredProcedureParameter("AV_QNR_ANS_HEADER_ID", Integer.class, ParameterMode.IN)
		        .setParameter("AV_QNR_ANS_HEADER_ID", questionnaireAnswerHeaderId)
		        .registerStoredProcedureParameter("AV_UPDATE_USER", String.class, ParameterMode.IN)
		        .setParameter("AV_UPDATE_USER", AuthenticatedUser.getLoginUserName())
		        .getSingleResult();
	}

	public synchronized Integer getNextQuestionnaireAnswerTableId() {
		Integer columnId = null;
		try {
			columnId = getMaxColumnValue("GET_MAX_QUESTIONNAIRE_ANSWER_TABLE_ID", QUESTIONNAIRE_ANSWER_TABLE_ID);
			updateMaxColumnValue("UPDATE_MAX_QUESTIONNAIRE_ANSWER_TABLE_ID", QUESTIONNAIRE_ANSWER_TABLE_ID, columnId);
			return columnId;
		} catch (Exception e) {
			logger.error("Exception in getNextQuestionnaireAnswerTableId : {} ", e.getMessage());
			return 0;
		}
	}

	public List<QuestTableAnswer> getQuestTableAnswers(Integer questionnaireAnsHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<QuestTableAnswer> query = builder.createQuery(QuestTableAnswer.class);
		Root<QuestTableAnswer> rootQuestTableAnswer = query.from(QuestTableAnswer.class);
		Predicate predicateQuestionnaireAnsHeaderId = builder.equal(rootQuestTableAnswer.get("questAnsHeaderId"), questionnaireAnsHeaderId);
		query.where(builder.and(predicateQuestionnaireAnsHeaderId));
		return session.createQuery(query).getResultList();
	}

	public void saveQuestTableAnswers(QuestTableAnswer questTableAnswer) {
		try {
			hibernateTemplate.saveOrUpdate(questTableAnswer);
		} catch (Exception e) {
			logger.info("Error ocuured in saveQuestTableAnswers {}", e.getMessage());
			throw new ApplicationException("Error in saveQuestTableAnswers", e, Constants.JAVA_ERROR);
		}
	}

	public Integer insertQuestionnaireAnswerHeader(Integer moduleCode, Integer moduleSubItemCode, String moduleItemKey,
			String moduleSubItemKey, String updateUser, Integer questionnaireId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer result = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call INSERT_QUESTIONNAIRE_ANS_HEADER(?,?,?,?,?,?,?)}");
				statement.setInt(1, questionnaireId);
				statement.setInt(2, moduleCode);
				statement.setInt(3, moduleSubItemCode);
				statement.setString(4, moduleItemKey);
				statement.setString(5, moduleSubItemKey);
				statement.setString(6, "N");
				statement.setString(7, updateUser);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call INSERT_QUESTIONNAIRE_ANS_HEADER (?,?,?,?,?,?,?)}");
				statement.setInt(1, questionnaireId);
				statement.setInt(2, moduleCode);
				statement.setInt(3, moduleSubItemCode);
				statement.setString(4, moduleItemKey);
				statement.setString(5, moduleSubItemKey);
				statement.setString(6, "N");
				statement.setString(7, updateUser);
				statement.registerOutParameter(8, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(6);
			}
			while (resultSet.next()) {
				result = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			throw new ApplicationException("error occured in canApproveRouting",e, "canApproveRouting");
		}
		return result;
	}

	public Integer insertQuestionnaireAnswer(Integer answerHeaderId, Integer question, String answer,
			String lookUpCode, Integer answerNumber,String explanation, String updateUser) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer result = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call INSERT_QUESTIONNAIRE_ANSWERS(?,?,?,?,?,?,?)}");
				statement.setInt(1, answerHeaderId);
				statement.setInt(2, question);
				statement.setInt(3, answerNumber);
				statement.setString(4, answer);
				statement.setString(5, lookUpCode);
				statement.setString(6, explanation);
				statement.setString(7, updateUser);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call INSERT_QUESTIONNAIRE_ANSWERS (?,?,?,?,?,?,?)}");
				statement.setInt(1, answerHeaderId);
				statement.setInt(2, question);
				statement.setInt(3, answerNumber);
				statement.setString(4, answer);
				statement.setString(5, lookUpCode);
				statement.setString(6, explanation);
				statement.setString(7, updateUser);
				statement.registerOutParameter(8, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(6);
			}
			while (resultSet.next()) {
				result = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			throw new ApplicationException("error occured in canApproveRouting",e, "canApproveRouting");
		}
		return result;
	}

	public Object getQuestionnaireById(Integer quesNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		NativeQuery query = session.createSQLQuery("SELECT * FROM QUEST_HEADER WHERE QUESTIONNAIRE_NUMBER = ? AND " +
				"VERSION_NUMBER = (SELECT MAX(VERSION_NUMBER) FROM QUEST_HEADER WHERE  QUESTIONNAIRE_NUMBER = ? AND IS_FINAL = 'Y')");
		return query.setParameter(1, quesNumber).setParameter(2, quesNumber).getSingleResult();
	}

	public List<HashMap<String, Object>> getBusinessRulesForQuestionnaire() throws Exception {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			output = dbEngine.executeQuery(inParam, "GET_BUSINESS_RULES_FOR_QUESTIONNAIRE");
		} catch (Exception e) {
			logger.error("Exception in getBusinessRulesForQuestionnaire : {} ", e.getMessage());

		}
		return output;
	}

	public Boolean checkInCompleteQuestionnaireExists(String moduleItemKey, String subModuleItemKey, Integer moduleCode, Integer subModuleCode) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<>();
		Integer count = 0;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<MODULE_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, moduleItemKey));
			inParam.add(new Parameter("<<MODULE_SUB_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, subModuleItemKey));
			inParam.add(new Parameter(MODULE_ITEM_CODE, DBEngineConstants.TYPE_INTEGER, moduleCode));
			inParam.add(new Parameter(MODULE_SUB_ITEM_CODE, DBEngineConstants.TYPE_INTEGER, subModuleCode));
			output = dbEngine.executeQuery(inParam, "GET_IS_QUESTIONNAIRES_COMPLETED");
			for (HashMap<String, Object> hmRules : output) {
				count = Integer.parseInt(hmRules.get("COUNT").toString());
			}
		} catch (Exception e) {
			logger.error("checkInCompleteQuestionnaireExists {}" , e.getMessage());
		}
		return count > 0 ? Boolean.TRUE : Boolean.FALSE;
	}

	public Integer getModuleByQuestAnswerHeaderId(Integer questionnaireAnswerHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<QuestAnswerHeader> rootQuestAnswerHeader = query.from(QuestAnswerHeader.class);
		query.select(rootQuestAnswerHeader.get("moduleItemCode"))
				.where(builder.equal(rootQuestAnswerHeader.get("questAnsHeaderId"), questionnaireAnswerHeaderId));
		return session.createQuery(query).uniqueResult();
	}

	public List<CoiQuestTableAnswer> getCoiQuestTableAnswers(Integer questionnaireAnsHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiQuestTableAnswer> query = builder.createQuery(CoiQuestTableAnswer.class);
		Root<CoiQuestTableAnswer> rootCoiQuestTableAnswer = query.from(CoiQuestTableAnswer.class);
		Predicate predicateQuestionnaireAnsHeaderId = builder.equal(rootCoiQuestTableAnswer.get("questAnsHeaderId"), questionnaireAnsHeaderId);
		query.where(builder.and(predicateQuestionnaireAnsHeaderId));
		return session.createQuery(query).getResultList();
	}

	public void saveCoiQuestTableAnswers(CoiQuestTableAnswer questTableAnswer) {
		try {
			hibernateTemplate.saveOrUpdate(questTableAnswer);
		} catch (Exception e) {
			logger.info("Error ocuured in saveQuestTableAnswers {}", e.getMessage());
			throw new ApplicationException("Error in saveCoiQuestTableAnswers", e, Constants.JAVA_ERROR);
		}
	}

}
