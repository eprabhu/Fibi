package com.polus.fibicomp.integration.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
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

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.pojo.Publication;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.integration.pojo.AwardFeed;
import com.polus.fibicomp.integration.pojo.AwardHoursLogRT;
import com.polus.fibicomp.integration.pojo.ExpenseTrackerRT;
import com.polus.fibicomp.integration.pojo.FeedAwardDetail;
import com.polus.fibicomp.integration.pojo.TempFeedSDC;
import com.polus.fibicomp.task.pojo.TaskAttachment;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "integrationDao")
public class IntegrationDaoImpl implements IntegrationDao{

	protected static Logger logger = LogManager.getLogger(IntegrationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Override
	public void addAwardHoursLogRT(AwardHoursLogRT awardHoursLogRT) {
		try {
			hibernateTemplate.saveOrUpdate(awardHoursLogRT);
		} catch (Exception e) {
			logger.error("Exception in addAwardHoursLogRT : {}", e.getMessage());
		}
	}

	@Override
	public void addExpenseTrackerRT(ExpenseTrackerRT expenseTrackerRT) {
		try {
			hibernateTemplate.saveOrUpdate(expenseTrackerRT);
		} catch (Exception e) {
			logger.error("Exception in addExpenseTrackerRT : {}", e.getMessage());
		}
	}

	@Override
	public void deleteAllAwardHoursLogRT() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardHoursLogRT> query = builder.createCriteriaDelete(AwardHoursLogRT.class);
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteAllExpenseTrackerRT() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<ExpenseTrackerRT> query = builder.createCriteriaDelete(ExpenseTrackerRT.class);
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteAllPublications() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<Publication> query = builder.createCriteriaDelete(Publication.class);
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void addPublication(Publication publication) {
		try {
			hibernateTemplate.saveOrUpdate(publication);
		} catch (Exception e) {
			logger.error("Exception in addPublication : {}", e.getMessage());
		}
	}

	@Override
	public void saveSAPAwardDetails(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SAP_AWARD_FEED (?,?,?,?)}");
				statement.setInt(1, awardId);
				statement.setString(2, awardNumber);
				statement.setInt(3, sequenceNumber);
				statement.setString(4, updateUser);
				statement.execute();
				statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call SAP_AWARD_FEED (?,?,?,?,?)}");
				statement.setInt(1, awardId);
				statement.setString(2, awardNumber);
				statement.setInt(3, sequenceNumber);
				statement.setString(4, updateUser);
				statement.registerOutParameter(5, OracleTypes.CURSOR);
				statement.execute();
				statement.getObject(5);
			}
		} catch (Exception e) {
			throw new ApplicationException("error occured saveSAPAwardDetails", e, Constants.DB_PROC_ERROR);
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				throw new ApplicationException("error occured saveSAPAwardDetails", e, Constants.DB_PROC_ERROR);
			}
		}
	}

	@Override
	public List<AwardFeed> getTempUsers() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardFeed> query = builder.createQuery(AwardFeed.class);
		query.from(AwardFeed.class);
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveFeededAwardDetails(FeedAwardDetail feedAwardDetail) {
		try {
			hibernateTemplate.saveOrUpdate(feedAwardDetail);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public String fetchInstuctionsByTaskTypeCode(String taskTypeCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
	 		String hqlQuery = "SELECT T.instruction FROM TaskType T WHERE T.taskTypeCode = :taskTypeCode";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("taskTypeCode", taskTypeCode);
	        return (String) query.getSingleResult();
		} catch (Exception e) {
			logger.error("error occured in fetchInstuctionsByTaskTypeCode : ", e.getMessage());
			return null;
		}
	}

	@Override
	public List<TempFeedSDC> getSDCFeeds() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TempFeedSDC> query = builder.createQuery(TempFeedSDC.class);
		query.from(TempFeedSDC.class);
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<TaskAttachment> getAllTaskAttachments(Integer taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TaskAttachment> query = builder.createQuery(TaskAttachment.class);
		Root<TaskAttachment> rootAttachment = query.from(TaskAttachment.class);
		query.where(builder.equal(rootAttachment.get("task").get("taskId"), taskId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String getAccontNumberByAwardId(Integer awardId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT T.accountNumber FROM Award T WHERE T.awardId = :awardId";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("awardId", awardId);
			return (String) query.getSingleResult();
		} catch (Exception e) {
			return null;

		}
	}

	@Override
	public String checkForTheSAPFeedResponse(String awardNumber, Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			statement = connection.prepareCall("{ ? = call FN_CHK_SAP_AWARD_FEED_RESPONSE (?,?)}");
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, awardNumber);
			statement.setInt(3, awardId);
			statement.execute();
			return statement.getString(1);
		} catch (Exception e) {
			logger.error("Exception in saveSAPAwardFeedDetails : {}", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in saveSAPAwardFeedDetails : {}", e.getMessage());
			}
		}
		return "FALSE";
	}

}
