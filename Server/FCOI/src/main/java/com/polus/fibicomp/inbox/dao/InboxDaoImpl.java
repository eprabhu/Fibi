package com.polus.fibicomp.inbox.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.amazonaws.auth.policy.Statement;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.pojo.Inbox;
import com.polus.fibicomp.inbox.vo.InboxVO;
import com.polus.fibicomp.security.AuthenticatedUser;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "InboxDao")
public class InboxDaoImpl implements InboxDao {

	protected static Logger logger = LogManager.getLogger(InboxDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private CommonDao commonDao;

	private static final String MESSAGE_TYPE_CODE = "messageTypeCode";
	private static final String SUBMODULE_ITEM_KEY = "subModuleItemKey";
	private static final String MODULE_ITEM_KEY = "moduleItemKey";
	private static final String MODULE_CODE = "moduleCode";
	private static final String SUB_MODULE_CODE = "subModuleCode";
	private static final String ARRIVAL_DATE = "arrivalDate";
	private static final String OPENED_FLAG = "openedFlag";
	private static final String TO_PERSON_ID = "toPersonId";

	@Override
	public void markReadMessage(InboxVO inboxVO) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<Inbox> query = builder.createCriteriaUpdate(Inbox.class);
		Root<Inbox> root = query.from(Inbox.class);
		Predicate predicateModuleCode = builder.equal(root.get(MODULE_CODE), inboxVO.getModuleCode());
		Predicate predicateModuleItemKey = builder.equal(root.get(MODULE_ITEM_KEY), inboxVO.getModuleItemKey());
		Predicate predicateToPersonId = builder.equal(root.get(TO_PERSON_ID), inboxVO.getToPersonId());
		Predicate predicateMessageTypeCode = builder.equal(root.get(MESSAGE_TYPE_CODE), inboxVO.getMessageTypeCode());
		Predicate predicateSubModuleCode = builder.equal(root.get(SUB_MODULE_CODE), inboxVO.getModuleCode());
		Predicate predicateSubModuleItemKey = builder.equal(root.get(SUBMODULE_ITEM_KEY), inboxVO.getModuleItemKey());
		query.set(root.get(OPENED_FLAG), Constants.YES).where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateToPersonId, predicateMessageTypeCode, predicateSubModuleCode, predicateSubModuleItemKey));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public List<Inbox> showInbox(InboxVO vo) {
		String isViewAll = vo.getIsViewAll();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder inboxBuilder = session.getCriteriaBuilder();
		CriteriaQuery<Inbox> inboxQuery = inboxBuilder.createQuery(Inbox.class);
		Root<Inbox> rootInbox = inboxQuery.from(Inbox.class);
		Predicate openFlag;
		Predicate personId = inboxBuilder.equal(rootInbox.get(TO_PERSON_ID), vo.getToPersonId());
		if (vo.isProcessed()) {
			openFlag = inboxBuilder.equal(rootInbox.get(OPENED_FLAG), Constants.YES);
		} else {
			openFlag = inboxBuilder.equal(rootInbox.get(OPENED_FLAG), Constants.NO);
		}
		if (vo.getModuleCode() != null && vo.getFromDate() != null && vo.getToDate() != null) {
			Predicate moduleCode = inboxBuilder.equal(rootInbox.get(MODULE_CODE), vo.getModuleCode());
			Predicate arrivalStartDate = inboxBuilder.lessThanOrEqualTo(rootInbox.get(ARRIVAL_DATE), vo.getToDate());
			Predicate arrivalEndDate = inboxBuilder.greaterThanOrEqualTo(rootInbox.get(ARRIVAL_DATE), vo.getFromDate());
			inboxQuery.where(inboxBuilder.and(personId, openFlag, moduleCode, arrivalStartDate, arrivalEndDate));
		} else if (vo.getModuleCode() != null) {
			Predicate predicate3 = inboxBuilder.equal(rootInbox.get(MODULE_CODE), vo.getModuleCode());
			inboxQuery.where(inboxBuilder.and(personId, openFlag, predicate3));
		} else if (vo.getFromDate() != null && vo.getToDate() != null) {
			Predicate arrivalStartDate = inboxBuilder.lessThanOrEqualTo(rootInbox.get(ARRIVAL_DATE), vo.getToDate());
			Predicate arrivalEndDate = inboxBuilder.greaterThanOrEqualTo(rootInbox.get(ARRIVAL_DATE), vo.getFromDate());
			inboxQuery.where(inboxBuilder.and(personId, openFlag, arrivalStartDate, arrivalEndDate));
		}  
		else {
			Predicate inSixMonths = inboxBuilder.greaterThanOrEqualTo(rootInbox.get("updateTimeStamp"),  Timestamp.valueOf(LocalDateTime.now().minusMonths(6)));
			inboxQuery.where(inboxBuilder.and(personId, openFlag, inSixMonths));
		}
		inboxQuery.orderBy(inboxBuilder.desc(rootInbox.get("updateTimeStamp")));
		if ("N".equals(isViewAll)) {
			return session.createQuery(inboxQuery).list();
		} else {
			return session.createQuery(inboxQuery).setMaxResults(5).list();
		}
	}

	@Override
	public void saveToInbox(Inbox inbox) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call ADD_TO_INBOX (?,?,?,?,?,?,?,?,?)}");
				statement.setInt(1, inbox.getModuleCode());
				statement.setString(2, inbox.getModuleItemKey());
				statement.setString(3, inbox.getToPersonId());
				statement.setString(4, inbox.getSubjectType());
				statement.setString(5, inbox.getUserMessage());
				statement.setString(6, AuthenticatedUser.getLoginUserName() != null ? AuthenticatedUser.getLoginUserName() : inbox.getUpdateUser());
				statement.setString(7, inbox.getMessageTypeCode());
				statement.setInt(8, inbox.getSubModuleCode());
				statement.setString(9, inbox.getSubModuleItemKey());
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call ADD_TO_INBOX (?,?,?,?,?,?,?,?,?,?)}");
				statement.setInt(1, inbox.getModuleCode());
				statement.setString(2, inbox.getModuleItemKey());
				statement.setString(3, inbox.getToPersonId());
				statement.setString(4, inbox.getSubjectType());
				statement.setString(5, inbox.getUserMessage());
				statement.setString(6, AuthenticatedUser.getLoginUserName());
				statement.setString(7, inbox.getMessageTypeCode());
				statement.setInt(8, inbox.getSubModuleCode());
				statement.setString(9, inbox.getSubModuleItemKey());
				statement.registerOutParameter(10, OracleTypes.CURSOR);
				statement.execute();
			}
		} catch (Exception e) {
			logger.error("Error occured while saving action item");
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Error occured while closing callable statement for inbox entry");
			}
		}
	}

	@Override
	public void markReadMessage(Integer moduleCode, String moduleItemKey, String personId, String messageTypeCode, String subModuleItemKey, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<Inbox> query = builder.createCriteriaUpdate(Inbox.class);
		Root<Inbox> root = query.from(Inbox.class);
		Predicate predicateModuleCode = builder.equal(root.get(MODULE_CODE), moduleCode);
		Predicate predicateModuleItemKey = builder.equal(root.get(MODULE_ITEM_KEY), moduleItemKey);
		Predicate predicateMessageTypeCode = builder.equal(root.get(MESSAGE_TYPE_CODE), messageTypeCode);
		Predicate predicateSubModuleCode = builder.equal(root.get(SUB_MODULE_CODE), subModuleCode);
		Predicate predicateSubModuleItemKey = builder.equal(root.get(SUBMODULE_ITEM_KEY), subModuleItemKey);
		query.set(root.get(OPENED_FLAG), Constants.YES).where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateMessageTypeCode, predicateSubModuleCode, predicateSubModuleItemKey));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void removeFromInbox(Integer moduleItemKey, Integer subModuleItemKey, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<Inbox> query = builder.createCriteriaUpdate(Inbox.class);
		Root<Inbox> root = query.from(Inbox.class);
		Predicate predicateModuleCode = builder.equal(root.get(MODULE_CODE), moduleCode);
		Predicate predicateModuleItemKey = builder.equal(root.get(MODULE_ITEM_KEY), moduleItemKey.toString());
		Predicate predicateSubModuleItemKey = builder.equal(root.get(SUBMODULE_ITEM_KEY), subModuleItemKey.toString());
		query.set(root.get(OPENED_FLAG), Constants.YES).where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateSubModuleItemKey));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void markAsReadBasedOnParams(Integer moduleCode, String moduleItemKey, String messageTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<Inbox> query = builder.createCriteriaUpdate(Inbox.class);
		Root<Inbox> root = query.from(Inbox.class);
		Predicate predicateModuleCode = builder.equal(root.get(MODULE_CODE), moduleCode);
		Predicate predicateModuleItemKey = builder.equal(root.get(MODULE_ITEM_KEY), moduleItemKey);
		Predicate predicateMessageTypeCode = builder.equal(root.get(MESSAGE_TYPE_CODE), messageTypeCode);
		query.set(root.get(OPENED_FLAG), Constants.YES).where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateMessageTypeCode));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void removeTaskMessageFromInbox(Integer moduleItemKey, Integer subModuleItemKey, Integer moduleCode, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<Inbox> query = builder.createCriteriaUpdate(Inbox.class);
		Root<Inbox> root = query.from(Inbox.class);
		Predicate predicateModuleCode = builder.equal(root.get(MODULE_CODE), moduleCode);
		Predicate predicateModuleItemKey = builder.equal(root.get(MODULE_ITEM_KEY), moduleItemKey.toString());
		Predicate predicateSubModuleItemKey = builder.equal(root.get(SUBMODULE_ITEM_KEY), subModuleItemKey.toString());
		Predicate predicateSubModuleCode = builder.equal(root.get(SUB_MODULE_CODE), subModuleCode);
		query.set(root.get(OPENED_FLAG), Constants.YES).where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateSubModuleItemKey, predicateSubModuleCode));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteMessageFromInboxByParams(String subModuleItemKey, Integer moduleCode, String moduleItemId, Integer awardTaskSubModuleCode, String messageTypeAssignTask, String oldAssigneePersonId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String deleteOldAssigneeInbox = "delete from Inbox where moduleCode =:moduleCode AND moduleItemKey =:moduleItemKey AND subModuleItemKey =:subModuleItemKey AND subModuleCode =:subModuleCode AND messageTypeCode =:messageTypeCode AND toPersonId=:toPersonId";
			Query query = session.createQuery(deleteOldAssigneeInbox);
			query.setParameter(MODULE_CODE, moduleCode);
			query.setParameter(MODULE_ITEM_KEY, moduleItemId);
			query.setParameter(SUBMODULE_ITEM_KEY, subModuleItemKey);
			query.setParameter(SUB_MODULE_CODE, awardTaskSubModuleCode);
			query.setParameter(MESSAGE_TYPE_CODE, messageTypeAssignTask);
			query.setParameter(TO_PERSON_ID, oldAssigneePersonId);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in deleteOldAssigneeInbox :{}", e.getMessage());
		}
	}

	@Override
	public void markAsExpiredFromActionList(Integer moduleCode, String moduleItemKey, Integer submoduleCode, String submoduleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<Inbox> query = builder.createCriteriaUpdate(Inbox.class);
		Root<Inbox> root = query.from(Inbox.class);
		Predicate predicateModuleCode = builder.equal(root.get(MODULE_CODE), moduleCode);
		Predicate predicateModuleItemKey = builder.equal(root.get(MODULE_ITEM_KEY), moduleItemKey);
		Predicate predicateOpendeFlag = builder.equal(root.get(OPENED_FLAG), Constants.NO);
		if (submoduleCode != null && submoduleItemKey != null) {
			Predicate predicateSubModuleCode = builder.equal(root.get(SUB_MODULE_CODE), submoduleCode);
			Predicate predicateSubModuleItemKey = builder.equal(root.get(SUBMODULE_ITEM_KEY), submoduleItemKey);
			query.set(root.get(OPENED_FLAG), Constants.EXPIRED).where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateSubModuleCode, predicateSubModuleItemKey, predicateOpendeFlag));
		} else {
			query.set(root.get(OPENED_FLAG), Constants.EXPIRED).where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateOpendeFlag));
		}
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void updateMessageInboxByParam(String subModuleItemKey, Integer moduleCode, String messageTypeCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String updateMessage = "update Inbox set openedFlag ='N', arrivalDate =:updateTime where moduleCode =:moduleCode AND subModuleItemKey =:subModuleItemKey  AND messageTypeCode =:messageTypeCode";
			Query query = session.createQuery(updateMessage);
			query.setParameter(MODULE_CODE, moduleCode);
			query.setParameter(SUBMODULE_ITEM_KEY, subModuleItemKey);
			query.setParameter(MESSAGE_TYPE_CODE, messageTypeCode);
			query.setParameter("updateTime", commonDao.getCurrentTimestamp());
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in updateMessageInboxByParam :{}", e.getMessage());
		}
	}

	@Override
	public void saveBannerEntriesToActionList(Inbox actionList) {
		hibernateTemplate.saveOrUpdate(actionList);
	}

}
