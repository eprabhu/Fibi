package com.polus.fibicomp.businessrule.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Restrictions;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.businessrule.dto.WorkFlowResultDto;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowAttachment;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "businessRuleDao")
public class BusinessRuleDaoImpl implements BusinessRuleDao {

	protected static Logger logger = LogManager.getLogger(BusinessRuleDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private CommonDao commonDao;

	@Override
	public Integer buildWorkFlow(EvaluateValidationRuleVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer result = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call RULE_EVALUATE_WORKFLOW (?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setString(4, vo.getLogginPersonId());
				statement.setString(5, vo.getUpdateUser());
				statement.setString(6, vo.getSubModuleItemKey());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call RULE_EVALUATE_WORKFLOW (?,?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setString(4, vo.getLogginPersonId());
				statement.setString(5, vo.getUpdateUser());
				statement.setString(6, vo.getSubModuleItemKey());
				statement.registerOutParameter(7, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(7);
			}
			while (resultSet.next()) {
				result = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			throw new ApplicationException("error occured in buildWorkFlow",e, Constants.DB_PROC_ERROR);
		}
		return result;
	}

	@Override
	public List<WorkFlowResultDto> evaluateValidationRule(EvaluateValidationRuleVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<WorkFlowResultDto> workFlowResultList = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		// boolean result = false;
		// List<HashMap<String, Object>> workflow = new ArrayList<HashMap<String, Object>>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_rule_evaluate_validation(?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setString(4, vo.getUpdateUser());
				statement.setString(5, vo.getLogginPersonId());
				statement.setString(6, vo.getSubModuleItemKey());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call get_rule_evaluate_validation (?,?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setString(4, vo.getUpdateUser());
				statement.setString(5, vo.getLogginPersonId());
				statement.setString(6, vo.getSubModuleItemKey());
				statement.registerOutParameter(7, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(7);
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					WorkFlowResultDto workflowDto = new WorkFlowResultDto();
					workflowDto.setValidationType((String) resultSet.getString("VALIDATION_TYPE"));
					workflowDto.setValidationMessage((String) resultSet.getString("VALIDATION_MESSAGE"));
					workFlowResultList.add(workflowDto);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return workFlowResultList;
	}

	@Override
	public List<String> evaluateNotificationRule(EvaluateValidationRuleVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<String> notifications = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_rule_evaluate_notification(?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setString(4, vo.getUpdateUser());
				statement.setString(5, vo.getLogginPersonId());
				statement.setString(6, vo.getSubModuleItemKey());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call get_rule_evaluate_notification (?,?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setString(4, vo.getUpdateUser());
				statement.setString(5, vo.getLogginPersonId());
				statement.setString(6, vo.getSubModuleItemKey());
				statement.registerOutParameter(7, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(7);
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					notifications.add(resultSet.getString("NOTIFICATION_ID"));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
			throw new ApplicationException("Error occurred in evaluateNotificationRule", e, Constants.DB_PROC_ERROR);
		}
		return notifications;
	}

	@Override
	public String ruleEvaluate(EvaluateValidationRuleVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<HashMap<String, Object>> workflow = new ArrayList<HashMap<String, Object>>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call rule_evaluation(?,?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setInt(4, vo.getRuleId());
				statement.setString(5, vo.getUpdateUser());
				statement.setString(6, vo.getLogginPersonId());
				statement.setString(7, vo.getSubModuleItemKey());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call rule_evaluation (?,?,?,?,?,?,?,?)}");
				statement.setInt(1, vo.getModuleCode());
				statement.setInt(2, vo.getSubModuleCode());
				statement.setString(3, vo.getModuleItemKey());
				statement.setInt(4, vo.getRuleId());
				statement.setString(5, vo.getUpdateUser());
				statement.setString(6, vo.getLogginPersonId());
				statement.setString(7, vo.getSubModuleItemKey());
				statement.registerOutParameter(8, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(8);
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					HashMap<String, Object> detailsField = new HashMap<String, Object>();
					detailsField.put("RESULT", resultSet.getString(1));
					workflow.add(detailsField);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(workflow);
	}

	@Override
	public Integer canApproveRouting(String moduleItemKey, String loginPersonId, Integer moduleCode, String subModuleItemKey, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer result = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call RULE_EVAL_CHK_CURRENT_APPROVER(?,?,?,?,?)}");
				statement.setString(1, moduleItemKey);
				statement.setInt(2, moduleCode);
				statement.setString(3, loginPersonId);
				statement.setInt(4, subModuleCode);
				statement.setString(5, subModuleItemKey);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call RULE_EVAL_CHK_CURRENT_APPROVER (?,?,?,?,?,?)}");
				statement.setString(1, moduleItemKey);
				statement.setInt(2, moduleCode);
				statement.setString(3, loginPersonId);
				statement.setInt(4, subModuleCode);
				statement.setString(5, subModuleItemKey);
				statement.registerOutParameter(6, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(6);
			}
			while (resultSet.next()) {
				result = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			throw new ApplicationException("error occured in canApproveRouting",e, Constants.DB_PROC_ERROR);
		}
		return result;
	}

	@Override
	public String getWorkFlowRouteLog(String module_item_id, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<HashMap<String, Object>> workflow = new ArrayList<HashMap<String, Object>>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_workflow_log(?,?)}");
				statement.setString(1, module_item_id);
				statement.setInt(2, moduleCode);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call get_workflow_log (?,?,?)}");
				statement.setString(1, module_item_id);
				statement.setInt(2, moduleCode);
				statement.registerOutParameter(3, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(3);
			}
			while (resultSet.next()) {
				HashMap<String, Object> detailsField = new HashMap<String, Object>();
				detailsField.put("workflowDetailId", resultSet.getString("WORKFLOW_DETAIL_ID"));
				detailsField.put("mapId", resultSet.getString("MAP_ID"));
				detailsField.put("mapName", resultSet.getString("WORKFLOWMAP_DESCRIPTION"));
				detailsField.put("mapNumber", resultSet.getInt("MAP_NUMBER"));
				detailsField.put("approvalStopNumber", resultSet.getInt("APPROVAL_STOP_NUMBER"));
				detailsField.put("approverNumber", resultSet.getInt("APPROVER_NUMBER"));
				detailsField.put("primaryApproverFlag", resultSet.getString("PRIMARY_APPROVER_FLAG"));
				detailsField.put("approverPersonName", resultSet.getString("APPROVER_PERSON_NAME"));
				detailsField.put("approverPersonId", resultSet.getString("APPROVER_PERSON_ID"));
				detailsField.put("approvalStatus", resultSet.getString("APPROVAL_STATUS"));
				detailsField.put("approvalStatusDescription", resultSet.getString("DESCRIPTION"));
				detailsField.put("approvalComment", resultSet.getString("APPROVAL_COMMENT"));
				detailsField.put("approvalDate", resultSet.getString("APPROVAL_DATE"));
				detailsField.put("emailAddress", resultSet.getString("EMAIL_ADDRESS"));
				detailsField.put("updateUser", resultSet.getString("UPDATE_USER"));
				detailsField.put("updateTimeStamp", resultSet.getString("UPDATE_TIMESTAMP"));
				detailsField.put("roleTypeCode", resultSet.getString("ROLE_TYPE_CODE"));
				detailsField.put("workflowRoleType", resultSet.getString("ROLE_TYPE_DESCRIPTION"));
				workflow.add(detailsField);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(workflow);
	}

	@Override
	public Workflow getWorkFlow(String moduleItemKey, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		// Workflow workflow = new Workflow();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_workflow_log(?,?)}");
				statement.setString(1, moduleItemKey);
				statement.setInt(2, moduleCode);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call get_workflow_log (?,?,?)}");
				statement.setString(1, moduleItemKey);
				statement.setInt(2, moduleCode);
				statement.registerOutParameter(3, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(3);
			}
			while (resultSet.next()) {
				// workflow.set;
				return new Workflow();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public String workflowfinalApproval(String moduleItemKey, String personId, Integer moduleCode, String subModuleItemKey, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String result = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call is_final_approval(?,?,?,?,?)}");
				statement.setString(1, moduleItemKey);
				statement.setString(2, personId);
				statement.setInt(3, moduleCode);
				statement.setInt(4, subModuleCode);
				statement.setString(5, subModuleItemKey);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call is_final_approval (?,?,?,?,?,?)}");
				statement.setString(1, moduleItemKey);
				statement.setString(2, personId);
				statement.setInt(3, moduleCode);
				statement.setInt(4, subModuleCode);
				statement.setString(5, subModuleItemKey);
				statement.registerOutParameter(6, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(6);
			}
			while (resultSet.next()) {
				result = resultSet.getString(1);
			}
		} catch (Exception e) {
			throw new ApplicationException("error occured in workflowfinalApproval",e, Constants.DB_FN_ERROR);
		}
		return result;
	}

	@Override
	public WorkflowAttachment saveWorkflowAttachment(WorkflowAttachment workflowAttachments) {
		if (workflowAttachments != null) {
			hibernateTemplate.saveOrUpdate(workflowAttachments);
		}
		return workflowAttachments;
	}

	@Override
	public String getEmailIdByName(String negotiatorId) {
		Person persons = new Person();
		persons = hibernateTemplate.get(Person.class, negotiatorId);
		return persons.getEmailAddress();
	}

	@Override
	public String savOrUpdateWorkflowDetails(WorkflowDetail workflowDetail) {
		// TODO Auto-generated method stub
		return null;
	}

	@SuppressWarnings({ "unchecked", "deprecation" })
	@Override
	public List<Workflow> getWorkflow(String moduleCode, String moduleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Criteria criteria = session.createCriteria(Workflow.class);
		criteria.add(Restrictions.like("moduleCode", moduleCode, MatchMode.EXACT));
		criteria.add(Restrictions.like("moduleItemId", moduleItemKey, MatchMode.EXACT));
		List<Workflow> workflowResults = criteria.list();
		return workflowResults;
	}

	@Override
	public String workflowApprove(String moduleItemKey, String moduleCode, String logginPersonId, String updatedUser,
			String actionType, String approverComment, Integer subModuleCode, String subModuleItemKey, Integer mapId, Integer mapNumber, Integer approverStopNumber, Integer approverNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = "0";
		try {
			String functionName = "FN_WORKFLOW_APPROVE";
			String functionCall = "{ ? = call " + functionName + "(?,?,?,?,?,?,?,?,?,?,?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, moduleItemKey);
			statement.setInt(3, Integer.parseInt(moduleCode));
			statement.setString(4, logginPersonId);
			statement.setString(5, updatedUser);
			statement.setString(6, approverComment);
			statement.setString(7, actionType);
			statement.setInt(8, subModuleCode);
			statement.setString(9, subModuleItemKey);
			statement.setInt(10, mapId == null ? 0 : mapId );
			statement.setInt(11, mapNumber == null ? 0 : mapNumber);
			statement.setInt(12, approverStopNumber == null ? 0 : approverStopNumber);
			statement.setInt(13, approverNumber == null ? 0 : approverNumber);
			statement.execute();
			result = statement.getString(1);
		if (result == null) {
			throw new ApplicationException("Approval is failed due to some reason", Constants.DB_FN_ERROR);
		}
		} catch (SQLException e) {
			throw new ApplicationException("error in workflowApprove", e, Constants.DB_FN_ERROR);
		}
		return result;
	}

	@Override
	public WorkflowAttachment fetchAttachmentById(Integer attachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("deprecation")
		Criteria criteria = session.createCriteria(WorkflowAttachment.class);
		criteria.add(Restrictions.like("attachmentId", attachmentId));
		@SuppressWarnings("unchecked")
		List<WorkflowAttachment> Attachment = criteria.list();
		return Attachment.get(0);
	}

	@Override
	public List<HashMap<String, Object>> getRoleNameAndMapName(Integer moduleCode, String moduleItemKey, Integer subModuleCode, String subModuleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<HashMap<String, Object>> details = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_WORKFLOW_MISSING_ROLE_TYPE (?,?,?,?)}");
				statement.setInt(1, moduleCode);
				statement.setInt(2, subModuleCode);
				statement.setString(3, moduleItemKey);
				statement.setString(4, subModuleItemKey);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call GET_WORKFLOW_MISSING_ROLE_TYPE (?,?,?,?,?)}");
				statement.setInt(1, moduleCode);
				statement.setInt(2, subModuleCode);
				statement.setString(3, moduleItemKey);
				statement.setString(4, subModuleItemKey);
				statement.registerOutParameter(5, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(5);
			}
			while (resultSet.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put("ROLE_TYPE", resultSet.getString("ROLE_TYPE"));
				detailsField.put("MAP_NAME", resultSet.getString("MAP_NAME"));
				details.add(detailsField);
			}

		} catch (Exception e) {
			e.printStackTrace();
			throw new ApplicationException("Error occurred in getRoleNameAndMapName", e, Constants.DB_PROC_ERROR);
		}
		return details;
	}

}
