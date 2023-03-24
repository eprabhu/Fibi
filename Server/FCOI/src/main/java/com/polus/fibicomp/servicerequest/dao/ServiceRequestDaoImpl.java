package com.polus.fibicomp.servicerequest.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.agreements.pojo.AgreementTypeTemplate;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.pojo.NotificationLog;
import com.polus.fibicomp.pojo.Module;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestActionLog;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestActionType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestAttachment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestComment;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestPriority;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestProcessFlow;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestProject;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestReporterChangeHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestRoleType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatus;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestStatusHistory;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestWatcher;

import oracle.jdbc.OracleTypes;

@SuppressWarnings("unused")
@Transactional
@Service("serviceRequestDao")
public class ServiceRequestDaoImpl implements ServiceRequestDao {

	protected static Logger logger = LogManager.getLogger(ServiceRequestDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	private static final String SR_HEADER_ID = "serviceRequestId";
	private static final String UPDATE_TIMESTAMP = "updateTimestamp";

	@Override
	public ServiceRequest saveOrUpdateServiceRequest(ServiceRequest serviceRequest) {
		try {
			hibernateTemplate.saveOrUpdate(serviceRequest);
		} catch (Exception e) {
			throw new ApplicationException("Error occurred in saveOrUpdateServiceRequest", e, Constants.JAVA_ERROR);
		}
		return serviceRequest;
	}

	@Override
	public ServiceRequestActionLog saveActionLog(ServiceRequestActionLog serviceRequestActionLog) {
		try {
			hibernateTemplate.save(serviceRequestActionLog);
		} catch (Exception e) {
			throw new ApplicationException("Error occurred in saveActionLog", e, Constants.JAVA_ERROR);
		}
		return serviceRequestActionLog;
	}

	@Override
	public ServiceRequest fetchServiceRequestById(Integer serviceRequestId) {
		return hibernateTemplate.get(ServiceRequest.class, serviceRequestId);
	}

	@Override
	public List<ServiceRequestType> getServiceRequestTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestType> query = builder.createQuery(ServiceRequestType.class);
		Root<ServiceRequestType> serviceRequestType = query.from(ServiceRequestType.class);
		query.where(builder.equal(serviceRequestType.get("activeFlag"), "Y"));
		query.orderBy(builder.asc(serviceRequestType.get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ServiceRequestActionType> getServiceRequestActionTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestActionType> query = builder.createQuery(ServiceRequestActionType.class);
		Root<ServiceRequestActionType> serviceRequestActionType = query.from(ServiceRequestActionType.class);
		query.orderBy(builder.asc(serviceRequestActionType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ServiceRequestStatus fetchStatusByStatusCode(Integer statusCode) {
		return hibernateTemplate.get(ServiceRequestStatus.class, statusCode);
	}

	@Override
	public ServiceRequestRoleType fetchUserRoleByRoleCode(String roleCode) {
		return hibernateTemplate.get(ServiceRequestRoleType.class, roleCode);
	}

	@Override
	public ServiceRequestAttachment getAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(ServiceRequestAttachment.class, attachmentId);
	}

	@Override
	public ServiceRequest deleteServiceRequest(ServiceRequest serviceRequest) {
		hibernateTemplate.delete(serviceRequest);
		return serviceRequest;
	}

	@Override
	public void deleteServiceRequestComment(ServiceRequestComment serviceRequestComment) {
		hibernateTemplate.delete(serviceRequestComment);
	}

	@Override
	public Module getServiceRequestModuleDetails(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Module> query = builder.createQuery(Module.class);
		Root<Module> serviceRequestModule = query.from(Module.class);
		Predicate predicate1 = builder.equal(serviceRequestModule.get("moduleCode"), moduleCode);
		query.where(builder.and(predicate1));		
		return session.createQuery(query).getResultList().get(0);
	}

	@Override
	public List<ServiceRequestAttachment> getAttachmentsBasedOnActionLog(Integer actionLogId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestAttachment> query = builder.createQuery(ServiceRequestAttachment.class);
		Root<ServiceRequestAttachment> serviceRequestAttachment = query.from(ServiceRequestAttachment.class);
		Predicate predicate1 = builder.equal(serviceRequestAttachment.get("actionLogId"), actionLogId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ServiceRequestComment> getCommentBasedOnActionLog(Integer actionLogId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestComment> query = builder.createQuery(ServiceRequestComment.class);
		Root<ServiceRequestComment> serviceRequestComment = query.from(ServiceRequestComment.class);
		Predicate predicate1 = builder.equal(serviceRequestComment.get("actionLogId"), actionLogId);
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ServiceRequestActionLog> fetchActionLogByServiceRequestId(Integer serviceRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestActionLog> query = builder.createQuery(ServiceRequestActionLog.class);
		Root<ServiceRequestActionLog> rootActionLog = query.from(ServiceRequestActionLog.class);
		Predicate predicateOne = builder.equal(rootActionLog.get("serviceRequestId"), serviceRequestId);
		query.where(builder.and(predicateOne));
		query.orderBy(builder.desc(rootActionLog.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ServiceRequestStatusHistory fetchstatusHistoryByActionLogId(Integer actionLogId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestStatusHistory> query = builder.createQuery(ServiceRequestStatusHistory.class);
		Root<ServiceRequestStatusHistory> rootstatusHistory = query.from(ServiceRequestStatusHistory.class);
		Predicate predicateOne = builder.equal(rootstatusHistory.get("actionLogId"), actionLogId);
		query.where(builder.and(predicateOne));
		try {
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public ServiceRequestHistory fetchServiceRequestHistoryByActionLogId(Integer actionLogId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestHistory> query = builder.createQuery(ServiceRequestHistory.class);
		Root<ServiceRequestHistory> rootServiceRequestHistory = query.from(ServiceRequestHistory.class);
		Predicate predicateOne = builder.equal(rootServiceRequestHistory.get("actionLogId"), actionLogId);
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public ServiceRequestProcessFlow getProcessFlowByActionLogId(Integer actionLogId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestProcessFlow> query = builder.createQuery(ServiceRequestProcessFlow.class);
		Root<ServiceRequestProcessFlow> rootProcessFlow = query.from(ServiceRequestProcessFlow.class);
		Predicate predicateOne = builder.equal(rootProcessFlow.get("actionLogId"), actionLogId);
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public void deleteActionLog(ServiceRequestActionLog serviceRequestActionLog) {
		hibernateTemplate.delete(serviceRequestActionLog);
	}

	@Override
	public ServiceRequestActionLog fetchActionLogById(Integer actionLogId) {
		return hibernateTemplate.get(ServiceRequestActionLog.class, actionLogId);
	}

	@Override
	public ServiceRequestType fetchServiceRequestTypeById(String typeCode) {
		return hibernateTemplate.get(ServiceRequestType.class, typeCode);
	}

	@Override
	public ServiceRequest fetchServiceRequestByModuleCodeAndKey(Integer moduleCode, String moduleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequest> query = builder.createQuery(ServiceRequest.class);
		Root<ServiceRequest> rootServiceRequest = query.from(ServiceRequest.class);
		Predicate predicateOne = builder.equal(rootServiceRequest.get("moduleCode"), moduleCode);
		Predicate predicateTwo = builder.equal(rootServiceRequest.get("moduleItemKey"), moduleItemKey);
		query.where(builder.and(predicateOne, predicateTwo));
		try {
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public ServiceRequestComment saveOrUpdateServiceRequestComment(ServiceRequestComment serviceRequestComment) {
		try {
			hibernateTemplate.saveOrUpdate(serviceRequestComment);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return serviceRequestComment;
	}

	@Override
	public ServiceRequest fetchServiceRequestByOriginatedAward(Integer moduleCode, String originatedAwardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequest> query = builder.createQuery(ServiceRequest.class);
		Root<ServiceRequest> rootServiceRequest = query.from(ServiceRequest.class);
		Predicate predicateOne = builder.equal(rootServiceRequest.get("moduleCode"), moduleCode);
		Predicate predicateTwo = builder.equal(rootServiceRequest.get("originatingModuleItemKey"), originatedAwardId);
		query.where(builder.and(predicateOne, predicateTwo));
		try {
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<ServiceRequestType> getServiceRequestTypesBasedOnModuleCode(Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestType> query = builder.createQuery(ServiceRequestType.class);
		Root<ServiceRequestType> serviceRequestType = query.from(ServiceRequestType.class);
		Predicate predicateOne = builder.equal(serviceRequestType.get("moduleCode"), moduleCode);
		Predicate predicateTwo = builder.equal(serviceRequestType.get("activeFlag"), "Y");
		query.where(builder.and(predicateOne, predicateTwo));
		query.orderBy(builder.asc(serviceRequestType.get("sortOrder")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ServiceRequestAttachment> fetchServiceRequestAttachmentBasedOnSRId(Integer serviceRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestAttachment> query = builder.createQuery(ServiceRequestAttachment.class);
		Root<ServiceRequestAttachment> rootServiceRequestAttachment = query.from(ServiceRequestAttachment.class);
		query.where(builder.equal(rootServiceRequestAttachment.get(SR_HEADER_ID), serviceRequestId));
		query.orderBy(builder.desc(rootServiceRequestAttachment.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ServiceRequestAttachment saveOrUpdateServiceRequestAttachment(ServiceRequestAttachment serviceRequestAttachment) {
		try {
			hibernateTemplate.saveOrUpdate(serviceRequestAttachment);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return serviceRequestAttachment;
	}


	@Override
	public ServiceRequestAttachment fetchServiceRequestAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(ServiceRequestAttachment.class, attachmentId);
	}

	@Override
	public ServiceRequestAttachment deleteServiceRequestAttachment(ServiceRequestAttachment serviceRequestAttachment) {
		try {
			hibernateTemplate.delete(serviceRequestAttachment);
		} catch (Exception e) {
			logger.error("exception in deleteProposalAttachment: {} ", e.getMessage());
		}
		return serviceRequestAttachment;
	}

	@Override
	public ServiceRequestStatusHistory saveOrUpdateServiceRequestStatusHistory(ServiceRequestStatusHistory serviceRequestStatusHistory) {
		try {
			hibernateTemplate.saveOrUpdate(serviceRequestStatusHistory);
		} catch (Exception e) {
			throw new ApplicationException("Error occurred in saveOrUpdateServiceRequestStatusHistory", e, Constants.JAVA_ERROR);
		}
		return serviceRequestStatusHistory;
	}

	@Override
	public ServiceRequestHistory saveServiceRequestHistory(ServiceRequestHistory serviceRequestHistory) {
		try {
			hibernateTemplate.save(serviceRequestHistory);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return serviceRequestHistory;
	}

	@Override
	public List<ServiceRequestActionLog> fetchActionLogByServiceRequestIdAndActionTypes(Integer serviceRequestId, List<Integer> actionTypeCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestActionLog> query = builder.createQuery(ServiceRequestActionLog.class);
		Root<ServiceRequestActionLog> rootActionLog = query.from(ServiceRequestActionLog.class);
		Predicate predicateOne = builder.equal(rootActionLog.get("serviceRequestId"), serviceRequestId);
		Predicate predicateTwo = rootActionLog.get("actionTypeCode").in(actionTypeCodes);
		query.where(builder.and(predicateOne, predicateTwo));
		query.orderBy(builder.desc(rootActionLog.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ServiceRequestComment> getCommentBasedOnActionLogIdIds(List<Integer> commentIds, Boolean isViewPrivateComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestComment> query = builder.createQuery(ServiceRequestComment.class);
		Root<ServiceRequestComment> serviceRequestComment = query.from(ServiceRequestComment.class);
		Predicate predicateUser = builder.equal(serviceRequestComment.get("updateUser"), AuthenticatedUser.getLoginUserName());
		Predicate predicateIsPrivate = builder.equal(serviceRequestComment.get("isPrivateComment"), true);
		Predicate predicateActionLogIds = serviceRequestComment.get("actionLogId").in(commentIds);
		Predicate privateComments = builder.and(predicateIsPrivate, predicateActionLogIds);
		Predicate publicComments = builder.and(builder.not(predicateIsPrivate), predicateActionLogIds);
		if (Boolean.FALSE.equals(isViewPrivateComment)) {
			privateComments = builder.and(privateComments, predicateUser);
		}
		query.where(builder.or(privateComments, publicComments));
		query.orderBy(builder.desc(serviceRequestComment.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ServiceRequestAttachment> getAttachmentBasedOnActionLogIdIds(List<Integer> attachmentIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestAttachment> query = builder.createQuery(ServiceRequestAttachment.class);
		Root<ServiceRequestAttachment> serviceRequestComment = query.from(ServiceRequestAttachment.class);
		query.where(builder.and(serviceRequestComment.get("actionLogId").in(attachmentIds)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ServiceRequestProcessFlow saveOrUpdateServiceRequestProcessFlow(ServiceRequestProcessFlow serviceRequestProcessFlow) {
		try {
			hibernateTemplate.saveOrUpdate(serviceRequestProcessFlow);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return serviceRequestProcessFlow;
	}

	@Override
	public ServiceRequestProject saveOrUpdateServiceRequestProject(ServiceRequestProject serviceRequestProject) {
		try {
			hibernateTemplate.saveOrUpdate(serviceRequestProject);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return serviceRequestProject;
	}

	@Override
	public void updateSRStatusAndAdminGroupId(Integer serviceRequestId, Integer statusCode, Integer adminGroupId) {																										  
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ServiceRequest> criteriaUpdate = cb.createCriteriaUpdate(ServiceRequest.class);
		Root<ServiceRequest> serviceRequestRoot = criteriaUpdate.from(ServiceRequest.class);
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("statusCode", statusCode);
		criteriaUpdate.set("adminGroupId", adminGroupId);
		criteriaUpdate.where(cb.equal(serviceRequestRoot.get("serviceRequestId"), serviceRequestId)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public ServiceRequestStatus getServiceRequestStatus(Integer statusCode) {
		return hibernateTemplate.get(ServiceRequestStatus.class, statusCode);
	}

	@Override
	public ServiceRequestWatcher saveServiceRequestWatcher(ServiceRequestWatcher serviceRequestWatcher) {
		try {
			hibernateTemplate.save(serviceRequestWatcher);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return serviceRequestWatcher;
	}

	@Override
	public void updateStatusHistoryEndTime(Integer serviceRequestId) {																										  
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ServiceRequestStatusHistory> criteriaUpdate = cb.createCriteriaUpdate(ServiceRequestStatusHistory.class);
		Root<ServiceRequestStatusHistory> serviceRequestRoot = criteriaUpdate.from(ServiceRequestStatusHistory.class);
		criteriaUpdate.set("actionEndTime", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		Predicate predicateOne = cb.equal(serviceRequestRoot.get("serviceRequestId"), serviceRequestId);
		Predicate predicateTwo = serviceRequestRoot.get("actionEndTime").isNull();
		criteriaUpdate.where(cb.and(predicateOne, predicateTwo)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public List<ServiceRequestStatusHistory> fetchstatusHistoryBySRHeaderId(Integer serviceRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ServiceRequestStatusHistory> query = builder.createQuery(ServiceRequestStatusHistory.class);
			Root<ServiceRequestStatusHistory> rootstatusHistory = query.from(ServiceRequestStatusHistory.class);
			Predicate predicateOne = builder.equal(rootstatusHistory.get("serviceRequestId"), serviceRequestId);
			query.where(builder.and(predicateOne));
			query.orderBy(builder.asc(rootstatusHistory.get("actionStartTime")));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public List<ServiceRequestWatcher> fetchSRWatchersBySRHeaderId(Integer serviceRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ServiceRequestWatcher> query = builder.createQuery(ServiceRequestWatcher.class);
			Root<ServiceRequestWatcher> rootstatusHistory = query.from(ServiceRequestWatcher.class);
			Predicate predicateOne = builder.equal(rootstatusHistory.get("serviceRequestId"), serviceRequestId);
			query.where(builder.and(predicateOne));
			query.orderBy(builder.asc(rootstatusHistory.get("updateTimestamp")));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public ServiceRequestWatcher fetchServiceRequestWatcherById(Integer watcherId) {
		return hibernateTemplate.get(ServiceRequestWatcher.class, watcherId);
	}

	@Override
	public ServiceRequestWatcher deleteServiceRequestWatcher(ServiceRequestWatcher serviceRequestWatcher) {
		hibernateTemplate.delete(serviceRequestWatcher);
		return serviceRequestWatcher;
	}

	@Override
	public List<ServiceRequestPriority> getServiceRequestPriority() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequestPriority> query = builder.createQuery(ServiceRequestPriority.class);
		Root<ServiceRequestPriority> serviceRequestPriority = query.from(ServiceRequestPriority.class);
		query.where(builder.equal(serviceRequestPriority.get("isActive"), true));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ServiceRequestActionType fetchServiceRequestActionTypeById(Integer actionTypeCode) {
		return hibernateTemplate.get(ServiceRequestActionType.class, actionTypeCode);
	}

	@Override
	public Integer fetchAdminGroupIdBasedOnSRType(String typeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> criteria = builder.createQuery(Integer.class);
		Root<ServiceRequestType> root = criteria.from(ServiceRequestType.class);
		criteria.select(root.get("adminGroupId"));
		criteria.where(builder.and(builder.equal(root.get("typeCode"), typeCode)));
		return session.createQuery(criteria).getSingleResult();
	}

	@Override
	public ServiceRequestReporterChangeHistory saveOrUpdateSRReporterChange(
			ServiceRequestReporterChangeHistory serviceRequestReporterChangeHistory) {
		try {
			hibernateTemplate.save(serviceRequestReporterChangeHistory);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return serviceRequestReporterChangeHistory;
	}

	@Override
	public void updateSRProject(String previousModuleItemKey, String moduleItemKey, Integer moduleCode, Integer serviceRequestId) {																										  
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ServiceRequestProject> criteriaUpdate = cb.createCriteriaUpdate(ServiceRequestProject.class);
		Root<ServiceRequestProject> serviceRequestRoot = criteriaUpdate.from(ServiceRequestProject.class);
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		if (moduleCode != null) {
			criteriaUpdate.set("moduleCode", moduleCode);
		}
		criteriaUpdate.set("moduleItemKey", moduleItemKey);
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(cb.and(
				cb.equal(serviceRequestRoot.get("serviceRequestId"), serviceRequestId),
				cb.equal(serviceRequestRoot.get("moduleItemKey"), previousModuleItemKey))); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public ServiceRequestPriority fetchSRPriorityById(Integer priorityId) {
		return hibernateTemplate.get(ServiceRequestPriority.class, priorityId);
	}

	@Override
	public List<ServiceRequestHistory> fetchServiceRequestHistoryByActionLogIds(List<String> actionLogIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<ServiceRequestHistory> srHistories = new ArrayList<>(); 
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_SERVICE_REQUEST_HISTORY(?)}");
				statement.setString(1, String.join(",", actionLogIds));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_SERVICE_REQUEST_HISTORY";
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setString(1, String.join(",", actionLogIds));
				statement.registerOutParameter(2, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(2);
			}
			while (resultSet != null && resultSet.next()) {
				ServiceRequestHistory srHistory = new ServiceRequestHistory();
				srHistory.setServiceRequestHistoryId((resultSet.getInt("SR_HEADER_HISTORY_ID")));
				srHistory.setActionLogId((resultSet.getInt("ACTION_LOG_ID")));
				srHistory.setServiceRequestId((resultSet.getInt("SR_HEADER_ID")));
				srHistory.setAssigneePersonName((resultSet.getString("ASSIGNEE_PERSON_NAME")));
				srHistory.setPreviousAssigneePersonName((resultSet.getString("PREV_ASSIGNEE_PERSON_NAME")));
				srHistory.setPreviousDescription((resultSet.getString("PREV_DESCRIPTION")));
				srHistory.setDescription((resultSet.getString("DESCRIPTION")));
				srHistory.setModuleCodeDescription((resultSet.getString("MODULE_CODE_DESC")));
				srHistory.setPreviousModuleCodeDescription((resultSet.getString("PREV_MODULE_CODE_DESC")));
				srHistory.setModuleItemKey((resultSet.getString("MODULE_ITEM_KEY")));
				srHistory.setPreviousModuleItemKey((resultSet.getString("PREV_MODULE_ITEM_KEY")));
				srHistory.setPreviousReporterPersonName((resultSet.getString("PREV_REPORTER_PERSON_NAME")));
				srHistory.setReporterPersonName((resultSet.getString("REPORTER_PERSON_NAME")));
				srHistory.setPreviousSubject((resultSet.getString("PREV_SUBJECT")));
				srHistory.setSubject((resultSet.getString("SUBJECT")));
				srHistory.setPreviousSrType((resultSet.getString("PREV_TYPE_DESC")));
				srHistory.setSrType((resultSet.getString("TYPE_DESC")));
				srHistory.setsRPriority((resultSet.getString("PRIORITY_DESC")));
				srHistory.setPreviousSrPriority((resultSet.getString("PREV_PRIORITY_DESC")));
				srHistory.setAdminGroupName((resultSet.getString("ADMIN_GROUP_DESC")));
				srHistory.setPreviousAdminGroupName((resultSet.getString("PREV_ADMIN_GROUP_DESC")));
				srHistory.setUnitName((resultSet.getString("UNIT_NAME")));
				srHistory.setPreviousUnitName((resultSet.getString("PREV_UNIT_NAME")));	
				if (resultSet.getDate("UPDATE_TIMESTAMP") != null) {
					srHistory.setUpdateTimestamp((resultSet.getTimestamp("UPDATE_TIMESTAMP")));
				}
				srHistory.setUpdateUser((resultSet.getString("UPDATE_USER")));
				srHistory.setTitle((resultSet.getString("TITLE")));
				srHistory.setPreviousTitle((resultSet.getString("PREV_TITLE")));
				srHistories.add(srHistory);
			}
			return srHistories;
		} catch (Exception e) {
			logger.error("Exception in fetchServiceRequestHistoryByActionLogIds : {}", e.getMessage());
			e.printStackTrace();
			return new ArrayList<>();
		}	
	}

	@Override
	public List<String> getSRWatcherIds(Integer serviceRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<String> query = builder.createQuery(String.class);
			Root<ServiceRequestWatcher> rootServiceRequestWatcher = query.from(ServiceRequestWatcher.class);
			query.where(builder.equal(rootServiceRequestWatcher.get("serviceRequestId"), serviceRequestId));
			query.select(rootServiceRequestWatcher.get("watcherPersonId"));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			e.printStackTrace();
			return new ArrayList<>();
		}
	}

	@Override
	public List<Integer> getPrivateCommentActionLogIds(Integer serviceRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
			Root<ServiceRequestComment> rootServiceRequestComment = query.from(ServiceRequestComment.class);
			Predicate predicateOne = builder.equal(rootServiceRequestComment.get("serviceRequestId"), serviceRequestId);
			Predicate predicateTwo = builder.equal(rootServiceRequestComment.get("isPrivateComment"), Boolean.TRUE);
			query.where(builder.and(predicateOne, predicateTwo));
			query.select(rootServiceRequestComment.get("actionLogId"));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			e.printStackTrace();
			return new ArrayList<>();
		}
	}

	@Override
	public String getPreviousServiceRequestStatus(Integer serviceRequestId, Integer statusCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select serviceRequestStatus.description from ServiceRequestStatusHistory where statusHistoryId in( ");
			hqlQuery.append("select MAX(statusHistoryId) FROM ServiceRequestStatusHistory where serviceRequestId = :serviceRequestId AND statusCode <> :statusCode)");
			javax.persistence.Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("serviceRequestId", serviceRequestId);
			query.setParameter("statusCode", statusCode);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			logger.error("Exception in getPreviousServiceRequestStatus : {}", e.getMessage());
			return "";
		}
	}

	@Override
	public Integer getDocumentIdOfSRAttachment(Integer serviceRequestId) {
		Integer documentId = 0;
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
			Root<ServiceRequestAttachment> rootAward = query.from(ServiceRequestAttachment.class);
			Predicate predicateOne = builder.equal(rootAward.get("serviceRequestId"), serviceRequestId);
			query.select(builder.max(rootAward.get("documentId")));
			query.where(builder.and(predicateOne));
			documentId = session.createQuery(query).getSingleResult();
			if (documentId != null) {
				return documentId;
			}
		} catch (Exception e) {
			return 0;
		}
		return 0;
	}
}
