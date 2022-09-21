package com.polus.fibicomp.fastintegration.dao;

import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.ParameterMode;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.hibernate.procedure.ProcedureCall;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseFile;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseTransactionsRT;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseTransactionsRTLog;
import com.polus.fibicomp.fastintegration.pojo.AwardRevenueFile;
import com.polus.fibicomp.fastintegration.pojo.AwardRevenueTransactionsRT;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeedBatchFiles;
import com.polus.fibicomp.fastintegration.pojo.SapFeedProbGrantCodeReport;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFmBudget;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFundedPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantBudMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplProjectDef;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsoPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsorClass;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplWbs;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;

import oracle.jdbc.internal.OracleTypes;

@Transactional
@Service(value = "fastIntegrationDao")
public class FastIntegrationDaoImpl implements FastIntegrationDao {

	protected static Logger logger = LogManager.getLogger(FastIntegrationDaoImpl.class.getName());

	@Autowired
	public HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private CommonDao commonDao;

	@Override
	public List<SapFeedTmplFundedPrgm> getSapFeedTmplFundedPrgmByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplFundedPrgm> query = builder.createQuery(SapFeedTmplFundedPrgm.class);
			Root<SapFeedTmplFundedPrgm> sapFeedTmplFundedPrgm = query.from(SapFeedTmplFundedPrgm.class);
			Predicate predicate1 = builder.equal(sapFeedTmplFundedPrgm.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<SapFeedTmplGrantBudMaster> getSapFeedTmplGrantBudMasterByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplGrantBudMaster> query = builder.createQuery(SapFeedTmplGrantBudMaster.class);
			Root<SapFeedTmplGrantBudMaster> sapFeedTmplGrantBudMaster = query.from(SapFeedTmplGrantBudMaster.class);
			Predicate predicate1 = builder.equal(sapFeedTmplGrantBudMaster.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<SapFeedTmplGrantMaster> getSapFeedTmplGrantMasterByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplGrantMaster> query = builder.createQuery(SapFeedTmplGrantMaster.class);
			Root<SapFeedTmplGrantMaster> sapFeedTmplGrantMaster = query.from(SapFeedTmplGrantMaster.class);
			Predicate predicate1 = builder.equal(sapFeedTmplGrantMaster.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<SapFeedTmplProjectDef> getSapFeedTmplProjectDefByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplProjectDef> query = builder.createQuery(SapFeedTmplProjectDef.class);
			Root<SapFeedTmplProjectDef> sapFeedTmplProjectDef = query.from(SapFeedTmplProjectDef.class);
			Predicate predicate1 = builder.equal(sapFeedTmplProjectDef.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<SapFeedTmplSponsoPrgm> getSapFeedTmplSponsoPrgmByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplSponsoPrgm> query = builder.createQuery(SapFeedTmplSponsoPrgm.class);
			Root<SapFeedTmplSponsoPrgm> sapFeedTmplSponsoPrgm = query.from(SapFeedTmplSponsoPrgm.class);
			Predicate predicate1 = builder.equal(sapFeedTmplSponsoPrgm.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<SapFeedTmplSponsorClass> getSapFeedTmplSponsorClassByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplSponsorClass> query = builder.createQuery(SapFeedTmplSponsorClass.class);
			Root<SapFeedTmplSponsorClass> sapFeedTmplSponsorClass = query.from(SapFeedTmplSponsorClass.class);
			Predicate predicate1 = builder.equal(sapFeedTmplSponsorClass.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<SapFeedTmplWbs> getSapFeedTmplWbsByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplWbs> query = builder.createQuery(SapFeedTmplWbs.class);
			Root<SapFeedTmplWbs> sapFeedTmplWbs = query.from(SapFeedTmplWbs.class);
			Predicate predicate1 = builder.equal(sapFeedTmplWbs.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public Integer generateBatchId(List<String> feedIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SAP_FEED_GENERATE_BATCH(?)}");
				statement.setString(1, feedIds.isEmpty() ? null : String.join(",", feedIds));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "SAP_FEED_GENERATE_BATCH";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, feedIds.isEmpty() ? null : String.join(",", feedIds));
				statement.executeUpdate();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet != null && resultSet.next()) {
				if (resultSet.getString("BATCH_ID") != null) {
					return Integer.parseInt(resultSet.getString("BATCH_ID"));
				} else {
					return null;
				}
			}
		} catch (SQLException e) {
			logger.info("Error ocuured in generateWBSNumber : {}", e);
		}
		return null;
	}

	@Override
	public void saveOrUpdateAwardFeedBatchFiles(SapAwardFeedBatchFiles sapAwardFeedBatchFiles) {
		try {
			hibernateTemplate.saveOrUpdate(sapAwardFeedBatchFiles);
		} catch (Exception e) {
			logger.info("Error ocuured in saveOrUpdateAwardFeedBatchFiles : {}", e);
		}
	}

	@Override
	public void updateSponsorClass(SapFeedTmplSponsorClass sapFeedTmplSponsorClass) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplSponsorClass set feedStatus = :feedStatus"
				+ " ,errorMessage = :errorMessage" + " where batchId = :batchId and sponsorClass =:sponsorClass");
		query.setParameter("batchId", sapFeedTmplSponsorClass.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplSponsorClass.getFeedStatus());
		query.setParameter("sponsorClass", sapFeedTmplSponsorClass.getSponsorClass());
		query.setParameter("errorMessage", sapFeedTmplSponsorClass.getErrorMessage());
		query.executeUpdate();
	}

	@Override
	public void updateSponsoPrgm(SapFeedTmplSponsoPrgm sapFeedTmplSponsoPrgm) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplSponsoPrgm set feedStatus = :feedStatus"
				+ " ,errorMessage = :errorMessage" + " where batchId = :batchId and sponsorProgram =:sponsorProgram");
		query.setParameter("batchId", sapFeedTmplSponsoPrgm.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplSponsoPrgm.getFeedStatus());
		query.setParameter("sponsorProgram", sapFeedTmplSponsoPrgm.getSponsorProgram());
		query.setParameter("errorMessage", sapFeedTmplSponsoPrgm.getErrorMessage());
		query.executeUpdate();
	}

	@Override
	public void updateFundedPrgm(SapFeedTmplFundedPrgm sapFeedTmplFundedPrgm) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplFundedPrgm set feedStatus = :feedStatus"
				+ " ,errorMessage = :errorMessage" + " where batchId = :batchId and fundedProgram =:fundedProgram");
		query.setParameter("batchId", sapFeedTmplFundedPrgm.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplFundedPrgm.getFeedStatus());
		query.setParameter("fundedProgram", sapFeedTmplFundedPrgm.getFundedProgram());
		query.setParameter("errorMessage", sapFeedTmplFundedPrgm.getErrorMessage());
		query.executeUpdate();
	}

	@Override
	public void updateGrantMaster(SapFeedTmplGrantMaster sapFeedTmplGrantMaster) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplGrantMaster set feedStatus = :feedStatus"
				+ " ,errorMessage = :errorMessage" + " where batchId = :batchId and grantCode =:grantCode");
		query.setParameter("batchId", sapFeedTmplGrantMaster.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplGrantMaster.getFeedStatus());
		query.setParameter("errorMessage", sapFeedTmplGrantMaster.getErrorMessage());
		query.setParameter("grantCode", sapFeedTmplGrantMaster.getGrantCode());
		query.executeUpdate();
	}

	@Override
	public void updateProjectDef(SapFeedTmplProjectDef sapFeedTmplProjectDef) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery(
				"update SapFeedTmplProjectDef set feedStatus = :feedStatus" + " ,errorMessage = :errorMessage"
						+ " where batchId = :batchId and projectDefinition =:projectDefinition");
		query.setParameter("batchId", sapFeedTmplProjectDef.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplProjectDef.getFeedStatus());
		query.setParameter("projectDefinition", sapFeedTmplProjectDef.getProjectDefinition());
		query.setParameter("errorMessage", sapFeedTmplProjectDef.getErrorMessage());
		query.executeUpdate();
	}

	@Override
	public void updateGrantBudMaster(SapFeedTmplGrantBudMaster sapFeedTmplGrantBudMaster) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplGrantBudMaster set feedStatus = :feedStatus"
				+ ",errorMessage = :errorMessage" + " where batchId = :batchId and sponsorProgram =:sponsorProgram and grantCode =:grantCode and fundCode =:fundCode and sponsorClass =:sponsorClass and process =:process");
		query.setParameter("batchId", sapFeedTmplGrantBudMaster.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplGrantBudMaster.getFeedStatus());
		query.setParameter("sponsorProgram", sapFeedTmplGrantBudMaster.getSponsorProgram());
		query.setParameter("errorMessage", sapFeedTmplGrantBudMaster.getErrorMessage());
		query.setParameter("grantCode", sapFeedTmplGrantBudMaster.getGrantCode());
		query.setParameter("fundCode", sapFeedTmplGrantBudMaster.getFundCode());
		query.setParameter("sponsorClass", sapFeedTmplGrantBudMaster.getSponsorClass());
		query.setParameter("process",sapFeedTmplGrantBudMaster.getProcess());
		query.executeUpdate();
	}

	@Override
	public void updateWBS(SapFeedTmplWbs sapFeedTmplWbs) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplWbs set feedStatus = :feedStatus"
				+ " ,errorMessage = :errorMessage" + " where batchId = :batchId and wbsElement =:wbsElement");
		query.setParameter("batchId", sapFeedTmplWbs.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplWbs.getFeedStatus());
		query.setParameter("wbsElement", sapFeedTmplWbs.getWbsElement());
		query.setParameter("errorMessage", sapFeedTmplWbs.getErrorMessage());
		query.executeUpdate();
	}

	@Override
	public List<SapAwardFeed> getAllfeedId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapAwardFeed> query = builder.createQuery(SapAwardFeed.class);
			Root<SapAwardFeed> rootSapAwardFeed = query.from(SapAwardFeed.class);
			Predicate predicate1 = builder.equal(rootSapAwardFeed.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public void updateFeedStatus(SapAwardFeed sapAwardFeedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session
				.createQuery("update SapAwardFeed set feedStatus = :feedStatus" + " where feedId = :feedId");
		query.setParameter("feedId", sapAwardFeedId.getFeedId());
		query.setParameter("feedStatus", sapAwardFeedId.getFeedStatus());
		query.executeUpdate();
	}

	@Override
	public List<SapFeedTmplFmBudget> getSapFeedTmplFmBudgetByBatchId(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapFeedTmplFmBudget> query = builder.createQuery(SapFeedTmplFmBudget.class);
			Root<SapFeedTmplFmBudget> sapFeedTmplGrantBudMaster = query.from(SapFeedTmplFmBudget.class);
			Predicate predicate1 = builder.equal(sapFeedTmplGrantBudMaster.get("batchId"), batchId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<String> fetchAllFeedStatus(Integer feedId,Integer batchId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		List<String> feedStatus = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			output = dbEngine.executeQuery(inParam, "get_all_feed_id");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmRules : output) {
					feedStatus.add((String) hmRules.get("FEED_STATUS"));
					}	
				}
			return feedStatus;
           } catch (Exception e) {
        	  e.printStackTrace();
			logger.error("Exception in fetchAllFeedStatus " + e.getMessage());
			return null;
		}		
	}

	@Override
	public String sapAwardUpdateHoldStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SAP_AWARD_UPDATE_HOLD_STATUS ()}");
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call SAP_AWARD_UPDATE_HOLD_STATUS (?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			if(resultSet != null ) {
				while(resultSet.next()) {
					return resultSet.getString("AWARD_ID_LIST");
				}	
			}
		} catch (Exception e) {
			logger.error("Exception in sapAwardUpdateHoldStatus : {}", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in sapAwardUpdateHoldStatus : {}", e.getMessage());
			}
		}
		return null;
	}

	@Override
	public void updateFmBudgets(SapFeedTmplFmBudget sapFeedTmplFmBudget) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplFmBudget set feedStatus = :feedStatus"
				+ " ,errorMessage = :errorMessage" + " where batchId = :batchId and fundedProgram =:fundedProgram");
		query.setParameter("batchId", sapFeedTmplFmBudget.getBatchId());
		query.setParameter("feedStatus", sapFeedTmplFmBudget.getFeedStatus());
		query.setParameter("errorMessage", sapFeedTmplFmBudget.getErrorMessage());
		query.setParameter("fundedProgram", sapFeedTmplFmBudget.getFundedProgram());
		query.executeUpdate();
	}

	@Override
	public AwardExpenseTransactionsRT saveExpenseTransactionRT(AwardExpenseTransactionsRT awardExpenseTransactionsRT) throws Exception {
		hibernateTemplate.save(awardExpenseTransactionsRT);
		return awardExpenseTransactionsRT;
	}

	@Override
	public AwardExpenseTransactionsRTLog saveAwardExpenseTransactionsRTLog(AwardExpenseTransactionsRTLog awardExpenseTransactionsRTLog) {
		hibernateTemplate.save(awardExpenseTransactionsRTLog);
		return awardExpenseTransactionsRTLog;
	}

	@Override
	public void deleteAllExpenseTransactionRT() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardExpenseTransactionsRT> query = builder.createCriteriaDelete(AwardExpenseTransactionsRT.class);
		query.from(AwardExpenseTransactionsRT.class);
		session.createQuery(query).executeUpdate();
	}

	@Override
	public AwardExpenseFile saveAwardExpenseFiles(AwardExpenseFile awardExpenseFiles) {
		hibernateTemplate.save(awardExpenseFiles);
		return awardExpenseFiles;
	}

	@Override
	public void awardExpenseTrackerRefresh(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call AWARD_EXPENSE_TRACKER_REFRESH (?)}");
				statement.setInt(1, fileId);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call AWARD_EXPENSE_TRACKER_REFRESH (?,?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, fileId);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception in awardExpenseTrackerRefresh : {}", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in awardExpenseTrackerRefresh : {}", e.getMessage());
			}
		}
	}

	@Override
	public void awardExpenseTrackerSync(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call AWARD_EXPENSE_TRACKER_SYNC (?)}");
				statement.setInt(1, fileId);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call AWARD_EXPENSE_TRACKER_SYNC (?,?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, fileId);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception in awardExpenseTrackerSync : {}", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in awardExpenseTrackerSync : {}", e.getMessage());
			}
		}
	}

	@Override
	public void awardExpenseTrackerPrevalidation() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call AWARD_EXP_TRACKR_PREVALIDATION ()}");
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call AWARD_EXP_TRACKR_PREVALIDATION (?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception in awardExpenseTrackerPrevalidation : {}", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in awardExpenseTrackerPrevalidation : {}", e.getMessage());
			}
		}
	}

	@Override
	public List<AwardExpenseTransactionsRT> getawardExpenseTransactionRTByFileId(Integer fileId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardExpenseTransactionsRT> query = builder.createQuery(AwardExpenseTransactionsRT.class);
			Root<AwardExpenseTransactionsRT> awardExpenseTransactionsRT = query.from(AwardExpenseTransactionsRT.class);
			Predicate predicate1 = builder.equal(awardExpenseTransactionsRT.get("fileId"), fileId);
			query.where(builder.and(predicate1));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public Integer getLatestAwardExpenseFile() {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String maxHQLQuery = "SELECT MAX(FILE_ID) FROM AWARD_EXPENSE_FILES";
		Query query = session.createSQLQuery(maxHQLQuery);
		return Integer.parseInt(query.getSingleResult().toString());
		} catch (Exception e) {
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> downloadExpenseTrackerReport() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("GET_AWARD_EXPENSE_TRACKER_REPORT").getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> downloadExpenseTrackerReportLevel2() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("GET_AWARD_EXPENSE_TRACKING_L2_REPORT").getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> downloadExpenseTrackerRepoertLevelOne() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("GET_AWARD_EXPENSE_TRACKING_L1_REPORT").getResultList();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Set<String> getawardAwardNumbersByFileId(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Set<String> accountNumbers = new HashSet();
 		String awardNumber = "SELECT distinct accountNumber FROM AwardExpenseTransactionsRT WHERE fileId = :fileId";
		Query query = session.createQuery(awardNumber);
		query.setParameter("fileId", fileId);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			accountNumbers.addAll(query.getResultList());
		}	
        return accountNumbers;
	}

	@Override
	public void sapFeedBudgetReport(Integer batchId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		ProcedureCall procedure = session.createStoredProcedureCall("SAP_FEED_REPORT");
		procedure.registerParameter( 1, Integer.class, ParameterMode.IN ).bindValue( batchId );
		procedure.execute();
	}

	@Override
	public void sapFeedReport(Integer batchId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		ProcedureCall procedure = session.createStoredProcedureCall("SAP_FEED_BUDGET_REPORT");
		procedure.registerParameter( 1, Integer.class, ParameterMode.IN ).bindValue( batchId );
		procedure.execute();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> sapReport(Integer batchId, String businessArea) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if(businessArea != null) {
		Query query = session.createSQLQuery("SELECT BATCH_ID,AWARD_NUMBER,ACCOUNT_NUMBER,SEQUENCE_NUMBER,VARIATION_TYPE,ACCOUNT_TYPE,TITLE,"
 				+ "CUR_START_DATE,PREV_START_DATE,CUR_END_DATE,PREV_END_DATE,IO_CODE,COST_ELEMENT_DESCRIPTION,FUND_CODE, "
 				+ "CUR_PI_NAME,PREV_PI_NAME,L1_WBS_DESCRIPTION,L2_WBS_DESCRIPTION,BUSINESS_AREA,GRANT_CODE,PROFIT_CENTER FROM SAP_FEED_REPORT WHERE BATCH_ID =:batchId and BUSINESS_AREA IN(:businessArea) ");
		query.setParameter("batchId", batchId);
		query.setParameter("businessArea", businessArea);
		return query.getResultList();
		} else {
		Query queryForNtuBaCode = session.createSQLQuery("SELECT BATCH_ID,AWARD_NUMBER,ACCOUNT_NUMBER,SEQUENCE_NUMBER,VARIATION_TYPE,ACCOUNT_TYPE,TITLE,"
	 				+ "CUR_START_DATE,PREV_START_DATE,CUR_END_DATE,PREV_END_DATE,IO_CODE,COST_ELEMENT_DESCRIPTION,FUND_CODE, "
	 				+ "CUR_PI_NAME,PREV_PI_NAME,L1_WBS_DESCRIPTION,L2_WBS_DESCRIPTION,BUSINESS_AREA,GRANT_CODE,PROFIT_CENTER FROM SAP_FEED_REPORT WHERE BATCH_ID =:batchId and BUSINESS_AREA NOT IN('A640','A630','A650') ");
		queryForNtuBaCode.setParameter("batchId", batchId);
		return queryForNtuBaCode.getResultList();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> exportSapFeedBudgetReport(Integer batchId, String businessArea) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if(businessArea != null) {
		Query query = session.createSQLQuery("SELECT BATCH_ID,AWARD_NUMBER,ACCOUNT_NUMBER,SEQUENCE_NUMBER,VARIATION_TYPE,ACCOUNT_TYPE,IO_CODE,\r\n" + 
				"FUND_CODE,PROCESS,BUDGET_AMOUNT,CUR_LINE_ITEM_COST,PREV_LINE_ITEM_COST,BUSINESS_AREA,GRANT_CODE,PROFIT_CENTER FROM SAP_FEED_BUDGET_REPORT WHERE BATCH_ID = :batchId and BUSINESS_AREA IN(:businessArea)");
		query.setParameter("batchId", batchId);
		query.setParameter("businessArea", businessArea);
		return query.getResultList();
		} else {
			Query queryForNtuBaCode = session.createSQLQuery("SELECT BATCH_ID,AWARD_NUMBER,ACCOUNT_NUMBER,SEQUENCE_NUMBER,VARIATION_TYPE,ACCOUNT_TYPE,IO_CODE,\r\n" + 
					"FUND_CODE,PROCESS,BUDGET_AMOUNT,CUR_LINE_ITEM_COST,PREV_LINE_ITEM_COST,BUSINESS_AREA,GRANT_CODE,PROFIT_CENTER FROM SAP_FEED_BUDGET_REPORT WHERE BATCH_ID = :batchId and  BUSINESS_AREA NOT IN('A640','A630','A650')");
			queryForNtuBaCode.setParameter("batchId", batchId);
			return queryForNtuBaCode.getResultList();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> exportDataNotFeeded(Integer batchId, String businessArea) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if(businessArea != null) {
		Query query = session.createSQLQuery("SELECT 	\r\n" + 
				"    T1.AWARD_NUMBER, \r\n" + 
				"    T1.ACCOUNT_NUMBER, \r\n" + 
				"    T6.DESCRIPTION AS  ACCOUNT_TYPE, \r\n" + 
				"    T1.LEAD_UNIT_NUMBER, \r\n" + 
				"    T3.UNIT_NAME AS LEAD_UNIT_NAME, \r\n" + 
				"    T1.TITLE AS AWARD_TITLE,    \r\n" + 
				"    T4.FULL_NAME AS PI_NAME,     \r\n" + 
				"    T1.SPONSOR_CODE, \r\n" + 
				"    T5.SPONSOR_NAME, \r\n" + 
				"    T1.SPONSOR_AWARD_NUMBER, \r\n" + 
				"    T1.BEGIN_DATE AS START_DATE,\r\n" + 
				"    T1.FINAL_EXPIRATION_DATE AS END_DATE,\r\n" + 
				"    T9.DESCRIPTION AS VARIATION_TYPE, \r\n" + 
				"    T8.SUBJECT  AS VARIATION_SUBJECT,\r\n" + 
				"    T10.SYSTEM_COMMENT,\r\n" + 
				"    T1.UPDATE_TIMESTAMP,\r\n" + 
				"    T1.SUBMISSION_DATE ,\r\n" + 
				"    T10.BUSINESS_AREA\r\n" + 
				"FROM AWARD T1  \r\n" + 
				"INNER JOIN AWARD_PERSONS T2 ON T1.AWARD_ID = T2.AWARD_ID \r\n" + 
				"INNER JOIN UNIT T3 ON T1.LEAD_UNIT_NUMBER = T3.UNIT_NUMBER    \r\n" + 
				"INNER JOIN PERSON T4 ON T4.PERSON_ID = T2.PERSON_ID \r\n" + 
				"INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE \r\n" + 
				"INNER JOIN ACCOUNT_TYPE T6 ON T6.ACCOUNT_TYPE_CODE = T1.ACCOUNT_TYPE_CODE\r\n" + 
				"INNER JOIN AWARD_DOCUMENT_TYPE T7 ON T7.AWARD_DOCUMENT_TYPE_CODE = T1.AWARD_DOCUMENT_TYPE_CODE \r\n" + 
				"LEFT OUTER JOIN  SR_HEADER T8 ON T8.MODULE_CODE = 1 AND T8.MODULE_ITEM_KEY IN (SELECT  MAX(AWARD_ID) FROM AWARD WHERE AWARD_ID < T1.AWARD_ID AND AWARD_NUMBER = T1.AWARD_NUMBER) \r\n" + 
				"LEFT OUTER JOIN SR_TYPE T9 ON T9.TYPE_CODE = T8.TYPE_CODE \r\n" + 
				"INNER JOIN SAP_AWARD_FEED T10 ON T1.AWARD_ID = T10.AWARD_ID\r\n" + 
				"WHERE T2.PI_FLAG = 'Y' \r\n" + 
				"AND T10.BATCH_ID = :batchId AND T10.FEED_STATUS = 'N' and T10.BUSINESS_AREA IN(:businessArea)");
		query.setParameter("batchId", batchId);
		query.setParameter("businessArea", businessArea);
		return query.getResultList();
	} else {
		Query queryBasedOnbaCode = session.createSQLQuery("   SELECT \r\n" + 
				"				    T1.AWARD_NUMBER, \r\n" + 
				"				    T1.ACCOUNT_NUMBER, \r\n" + 
				"				    T6.DESCRIPTION AS  ACCOUNT_TYPE, \r\n" + 
				"				    T1.LEAD_UNIT_NUMBER, \r\n" + 
				"				    T3.UNIT_NAME AS LEAD_UNIT_NAME, \r\n" + 
				"				    T1.TITLE AS AWARD_TITLE,   \r\n" + 
				"				    T4.FULL_NAME AS PI_NAME,    \r\n" + 
				"				    T1.SPONSOR_CODE, \r\n" + 
				"				    T5.SPONSOR_NAME, \r\n" + 
				"				    T1.SPONSOR_AWARD_NUMBER, \r\n" + 
				"				    T1.BEGIN_DATE AS START_DATE,\r\n" + 
				"				    T1.FINAL_EXPIRATION_DATE AS END_DATE, \r\n" + 
				"				    T9.DESCRIPTION AS VARIATION_TYPE,  \r\n" + 
				"				    T8.SUBJECT  AS VARIATION_SUBJECT,\r\n" + 
				"				    T10.SYSTEM_COMMENT,\r\n" + 
				"				    T1.UPDATE_TIMESTAMP,\r\n" + 
				"				    T1.SUBMISSION_DATE,\r\n" + 
				"                   T10.BUSINESS_AREA\r\n" + 
				"				FROM AWARD T1  \r\n" + 
				"				INNER JOIN AWARD_PERSONS T2 ON T1.AWARD_ID = T2.AWARD_ID \r\n" + 
				"				INNER JOIN UNIT T3 ON T1.LEAD_UNIT_NUMBER = T3.UNIT_NUMBER    \r\n" + 
				"				INNER JOIN PERSON T4 ON T4.PERSON_ID = T2.PERSON_ID \r\n" + 
				"				INNER JOIN SPONSOR T5 ON T1.SPONSOR_CODE = T5.SPONSOR_CODE \r\n" + 
				"				INNER JOIN ACCOUNT_TYPE T6 ON T6.ACCOUNT_TYPE_CODE = T1.ACCOUNT_TYPE_CODE\r\n" + 
				"				INNER JOIN AWARD_DOCUMENT_TYPE T7 ON T7.AWARD_DOCUMENT_TYPE_CODE = T1.AWARD_DOCUMENT_TYPE_CODE \r\n" + 
				"				LEFT OUTER JOIN  SR_HEADER T8 ON T8.MODULE_CODE = 1 AND T8.MODULE_ITEM_KEY IN (SELECT  MAX(AWARD_ID) FROM AWARD WHERE AWARD_ID < T1.AWARD_ID AND AWARD_NUMBER = T1.AWARD_NUMBER) \r\n" + 
				"				LEFT OUTER JOIN SR_TYPE T9 ON T9.TYPE_CODE = T8.TYPE_CODE \r\n" + 
				"				INNER JOIN SAP_AWARD_FEED T10 ON T1.AWARD_ID = T10.AWARD_ID\r\n" + 
				"				WHERE \r\n" + 
				"                T2.PI_FLAG = 'Y' AND T10.BATCH_ID =:batchId AND T10.FEED_STATUS = 'N' and T10.BUSINESS_AREA NOT IN('A640','A630','A650')");
		queryBasedOnbaCode.setParameter("batchId", batchId);
		return queryBasedOnbaCode.getResultList();
	 }
	}

	@Override
	public List<Integer> getSapAwardCount(Integer batchId) {
		ArrayList<HashMap<String, Object>> sapAwardCountoutput = new ArrayList<HashMap<String, Object>>();
		List<Integer> feedAwardCount = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			sapAwardCountoutput = dbEngine.executeQuery(inParam, "get_sap_award_feed_count");
			if (sapAwardCountoutput != null && !sapAwardCountoutput.isEmpty()) {
				for (HashMap<String, Object> hmRules : sapAwardCountoutput) {
					feedAwardCount.add(Integer.parseInt(hmRules.get("AWARDCOUNT").toString()));
					feedAwardCount.add(Integer.parseInt(hmRules.get("AWARDSUCCESS").toString()));
					feedAwardCount.add(Integer.parseInt(hmRules.get("AWARDERROR").toString()));
					feedAwardCount.add(Integer.parseInt(hmRules.get("AWARDPENDING").toString()));
					feedAwardCount.add(Integer.parseInt(hmRules.get("AWARDPENDINGERROR").toString()));
					}	
				}
			return feedAwardCount;
           } catch (Exception e) {
        	  e.printStackTrace();
			logger.error("Exception in getSapAwardCount " + e.getMessage());
			return null;
		}
	}

	@Override
	public AwardRevenueFile saveAwardRevenueFiles(AwardRevenueFile awardRevenueFiles) {
		hibernateTemplate.save(awardRevenueFiles);
		return awardRevenueFiles;
	}

	@Override
	public AwardRevenueTransactionsRT saveRevenueTransactionRT(AwardRevenueTransactionsRT awardRevenueTransactionsRT) {
		hibernateTemplate.save(awardRevenueTransactionsRT);
		return awardRevenueTransactionsRT;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Set<String> getRevenueAwardNumbersByFileId(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Set<String> accountNumbers = new HashSet();
 		String awardNumber = "SELECT distinct accountNumber FROM AwardRevenueTransactionsRT WHERE fileId = :fileId";
		Query query = session.createQuery(awardNumber);
		query.setParameter("fileId", fileId);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			accountNumbers.addAll(query.getResultList());
		}	
        return accountNumbers;
	}

	@Override
	public void awardRevenueTrackerRefresh(Integer fileId, EmailContent emailContent) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call AWARD_REVENUE_TRACKER_REFRESH (?)}");
				statement.setInt(1, fileId);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call AWARD_REVENUE_TRACKER_REFRESH (?,?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, fileId);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception in awardRevenueTrackerRefresh : {}", e.getMessage());
			emailContent.getError().append("Error in awardRevenueTrackerRefresh : ").append(e).append("<br/>");
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in awardRevenueTrackerRefresh : {}", e.getMessage());
			}
		}
	}

	@Override
	public void awardRevenueTrackerSync(Integer fileId, EmailContent emailContent) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call AWARD_REVENUE_TRACKER_SYNC (?)}");
				statement.setInt(1, fileId);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call AWARD_REVENUE_TRACKER_SYNC (?,?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, fileId);
				statement.execute();
				statement.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception in awardRevenueTrackerSync : {}", e.getMessage());
			emailContent.getError().append("Error in awardRevenueTrackerSync : ").append(e).append("<br/>");
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (SQLException e) {
				logger.error("Exception in awardRevenueTrackerSync : {}", e.getMessage());
			}
		}
	}

	@Override
	public void deleteAllRevenueTransactionRT() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AwardRevenueTransactionsRT> awardRevenueTransactionsRT = builder.createCriteriaDelete(AwardRevenueTransactionsRT.class);
		awardRevenueTransactionsRT.from(AwardRevenueTransactionsRT.class);
		session.createQuery(awardRevenueTransactionsRT).executeUpdate();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> downloadRevenueTransaction() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("GET_AWARD_REVENUE_TRACKER_REPORT").getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> downloadRevenueRepoertLevelOne() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("GET_AWARD_REVENUE_TRACKING_L1_REPORT").getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> downloadRevenueReportLevelTwo() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		return session.createStoredProcedureCall("GET_AWARD_REVENUE_TRACKING_L2_REPORT").getResultList();
	}

	@Override
	public void syncExpenseDataSet() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		session.createStoredProcedureCall("EXPENSE_DATA_SYNC").execute();
	}

	@Override
	public List<SapFeedProbGrantCodeReport> fetchSapFeedProbGrantCodeReport(Integer batchId, String baCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedProbGrantCodeReport> query = builder.createQuery(SapFeedProbGrantCodeReport.class);
		Root<SapFeedProbGrantCodeReport> rootsapFeedProbGrantCodeReport = query.from(SapFeedProbGrantCodeReport.class);
		Predicate predicateBatchId = builder.equal(rootsapFeedProbGrantCodeReport.get("batchId"), batchId);
		Predicate predicateBaCode = builder.equal(rootsapFeedProbGrantCodeReport.get("businessArea"), baCode);
		if (baCode != null) {
			query.where(builder.and(predicateBatchId, predicateBaCode));
		} else {
			Predicate predicateBusinessAreas = builder.not(rootsapFeedProbGrantCodeReport.get("businessArea").in(Constants.NIE_BUSINESS_AREA_CODE, Constants.LKC_BUSINESS_AREA_CODE, Constants.RSIS_BUSINESS_AREA_CODE));
			query.where(builder.and(predicateBatchId, predicateBusinessAreas));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public SapAwardFeed getSapAwardFeedDetails(Integer feedId) {
		return hibernateTemplate.get(SapAwardFeed.class, feedId);
	}

	@SuppressWarnings({ "unchecked", "rawtypes", "deprecation" })
	@Override
	public List<AwardBudgetDetail> getSapAwardBudgetDetailsByAwardId(Integer awardId) {
		List<AwardBudgetDetail> awardBudgetDetails = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String query = "SELECT T2.INTERNAL_ORDER_CODE, T2.LINE_ITEM_COST FROM AWARD_BUDGET_HEADER T1  LEFT JOIN AWARD_BUDGET_DETAIL T2 ON T2.BUDGET_HEADER_ID = T1.BUDGET_HEADER_ID WHERE T1.AWARD_ID ="
					+ awardId;
			SQLQuery sqlQuery = session.createSQLQuery(query);
			List<Object[]> entities = sqlQuery.list();
			for (Object[] entity : entities) {
				AwardBudgetDetail awardBudgetDetail = new AwardBudgetDetail();
				if (entity[0] != null) {
					awardBudgetDetail.setInternalOrderCode(entity[0].toString());
				}
				if (entity[1] != null) {
					awardBudgetDetail.setLineItemCost((BigDecimal) entity[1]);
				}
				awardBudgetDetails.add(awardBudgetDetail);
			}
		} catch (Exception e) {
			logger.error("Exception in getSapAwardBudgetDetailsByAwardId {}", e.getMessage());
		}
		return awardBudgetDetails;
	}

	@Override
	public Integer getProblematicGrantCodeReport(Integer batchId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer result = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SAP_FEED_PROBLEMATIC_GRANT_CODE_REPORT (?)}");
				statement.setInt(1, batchId);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call SAP_FEED_PROBLEMATIC_GRANT_CODE_REPORT (?,?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, batchId);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				result = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			logger.error("Exception in awardRevenueTrackerRefresh : {}", e.getMessage());
		} 
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> exportSoftLaunchAndNoFeedReport(Integer batchId, String feedStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = null;
		if (feedStatus.equals("Y")) {
			query = session.createSQLQuery("SELECT AWARD_NUMBER,ACCOUNT_NUMBER,IS_100_PCT_INTERNAL,ACCOUNT_TYPE,LEAD_UNIT_NUMBER,LEAD_UNIT_NAME,AWARD_TITLE,PI_NAME,SPONSOR_CODE,SPONSOR_NAME,SPONSOR_AWARD_NUMBER,VARIATION_TYPE,VARIATION_SUBJECT,SUBMISSION_DATE FROM SAP_NO_FEED_SOFT_LAUNCH_REPORT WHERE FEED_STATUS <>:feedStatus AND BATCH_ID =:batchId");
		} else {
			query = session.createSQLQuery("SELECT AWARD_NUMBER,ACCOUNT_NUMBER,IS_100_PCT_INTERNAL,ACCOUNT_TYPE,LEAD_UNIT_NUMBER,LEAD_UNIT_NAME,AWARD_TITLE,PI_NAME,SPONSOR_CODE,SPONSOR_NAME,SPONSOR_AWARD_NUMBER,VARIATION_TYPE,VARIATION_SUBJECT,SUBMISSION_DATE FROM SAP_NO_FEED_SOFT_LAUNCH_REPORT WHERE FEED_STATUS =:feedStatus AND BATCH_ID =:batchId");
		}
		query.setParameter("batchId", batchId);
		query.setParameter("feedStatus", Constants.NO);
		return query.getResultList();
	}

	@Override
	public void updateStatusForDataLoadingInRT(Integer fileId, String isSuccess, String fileType, Integer totalRowCount) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		if (fileType.equals("E")) {
			CriteriaUpdate<AwardExpenseFile> criteriaUpdate = cb.createCriteriaUpdate(AwardExpenseFile.class);
			Root<AwardExpenseFile> root = criteriaUpdate.from(AwardExpenseFile.class);
			criteriaUpdate.set("insertedInRT", isSuccess);
			criteriaUpdate.set("insertedRows", totalRowCount);
			criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
			criteriaUpdate.where(cb.equal(root.get("fileId"),fileId)); 		
			session.createQuery(criteriaUpdate).executeUpdate();
		} else {
			CriteriaUpdate<AwardRevenueFile> criteriaUpdate = cb.createCriteriaUpdate(AwardRevenueFile.class);
			Root<AwardRevenueFile> root = criteriaUpdate.from(AwardRevenueFile.class);
			criteriaUpdate.set("insertedInRT", isSuccess);
			criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
			criteriaUpdate.set("insertedRows", totalRowCount);
			criteriaUpdate.where(cb.equal(root.get("fileId"),fileId)); 		
			session.createQuery(criteriaUpdate).executeUpdate();
		}
	}

	@Override
	public void updateStatusForFileInSystem(Integer fileId, String isSuccess, String fileType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		if (fileType.equals("E")) {
			CriteriaUpdate<AwardExpenseFile> criteriaUpdate = cb.createCriteriaUpdate(AwardExpenseFile.class);
			Root<AwardExpenseFile> root = criteriaUpdate.from(AwardExpenseFile.class);
			criteriaUpdate.set("systemArchived", isSuccess);
			criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
			criteriaUpdate.where(cb.equal(root.get("fileId"),fileId)); 		
			session.createQuery(criteriaUpdate).executeUpdate();
		} else {
			CriteriaUpdate<AwardRevenueFile> criteriaUpdate = cb.createCriteriaUpdate(AwardRevenueFile.class);
			Root<AwardRevenueFile> root = criteriaUpdate.from(AwardRevenueFile.class);
			criteriaUpdate.set("systemArchived", isSuccess);
			criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
			criteriaUpdate.where(cb.equal(root.get("fileId"),fileId)); 		
			session.createQuery(criteriaUpdate).executeUpdate();
		}
	}

	@Override
	public void updateStatusForFileInRemote(Integer fileId, String isSuccess, String fileType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		if (fileType.equals("E")) {
			CriteriaUpdate<AwardExpenseFile> criteriaUpdate = cb.createCriteriaUpdate(AwardExpenseFile.class);
			Root<AwardExpenseFile> root = criteriaUpdate.from(AwardExpenseFile.class);
			criteriaUpdate.set("remoteArchived", isSuccess);
			criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
			criteriaUpdate.where(cb.equal(root.get("fileId"),fileId)); 		
			session.createQuery(criteriaUpdate).executeUpdate();
		} else {
			CriteriaUpdate<AwardRevenueFile> criteriaUpdate = cb.createCriteriaUpdate(AwardRevenueFile.class);
			Root<AwardRevenueFile> root = criteriaUpdate.from(AwardRevenueFile.class);
			criteriaUpdate.set("remoteArchived", isSuccess);
			criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
			criteriaUpdate.where(cb.equal(root.get("fileId"),fileId)); 		
			session.createQuery(criteriaUpdate).executeUpdate();
		}
	}

	@Override
	public void sapFeedSoftLaunchReport(Integer batchId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		ProcedureCall procedure = session.createStoredProcedureCall("SAP_FEED_SOFT_LAUNCH_NO_REPORT");
		procedure.registerParameter( 1, Integer.class, ParameterMode.IN ).bindValue( batchId );
		procedure.execute();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getSoftLaunchSummaryReport(Integer batchId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		ProcedureCall procedure = session.createStoredProcedureCall("SAP_FEED_SOFT_LAUNCH_COUNT");
		procedure.registerParameter( 1, Integer.class, ParameterMode.IN ).bindValue( batchId );
		return procedure.getResultList();
	}

	@Override
	public Integer getExpenseTransactionCount(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaQuery<Long> query = cb.createQuery(Long.class);
		Root<AwardExpenseTransactionsRT> root = query.from(AwardExpenseTransactionsRT.class);
		query.select(cb.count(root));
		query.where(cb.equal(root.get("fileId"), fileId));
		Long count = session.createQuery(query).getSingleResult();
		return count.intValue();
	}

	@Override
	public Integer getRevenueTransactionCount(Integer fileId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaQuery<Long> query = cb.createQuery(Long.class);
		Root<AwardRevenueTransactionsRT> root = query.from(AwardRevenueTransactionsRT.class);
		query.select(cb.count(root));
		query.where(cb.equal(root.get("fileId"), fileId));
		Long count = session.createQuery(query).getSingleResult();
		return count.intValue();
	}

	@Override
	public void updateSapAwardFeedBatch(Integer batchId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session
				.createQuery("update SapAwardFeedBatch set responseTimestamp = utc_timestamp() where batchId = :batchId");
		query.setParameter("batchId", batchId);
		query.executeUpdate();
	}

	@Override
	public void updateClaimWithRevenueData(EmailContent emailContent) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			session.createStoredProcedureCall("UPDATE_CLAIM_WITH_REVENUE_DATA").execute();
		} catch (Exception e) {
    		emailContent.getError().append("Error in updateClaimWithRevenueData : ").append(e).append("<br/>");
		}
  }
  
	public List<SapAwardFeed> getAllFeedAndNonFeeds(Integer batchId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapAwardFeed> query = builder.createQuery(SapAwardFeed.class);
			Root<SapAwardFeed> rootSapAwardFeed = query.from(SapAwardFeed.class);
			Predicate predicateBatchId = builder.equal(rootSapAwardFeed.get("batchId"), batchId);
			Predicate predicateFeedStatus = rootSapAwardFeed.get("feedStatus").in(Constants.SAP_FEED_STATUS_FEED, Constants.SAP_FEED_STATUS_NONE_TO_FEED);
			query.where(builder.and(predicateBatchId, predicateFeedStatus));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public void updateFeedStatusForError(SapAwardFeed sapAwardfeed) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session
				.createQuery("update SapAwardFeed set feedStatus = :feedStatus,userActionCode = null ,userComment = null" + " where feedId = :feedId");
		query.setParameter("feedId", sapAwardfeed.getFeedId());
		query.setParameter("feedStatus", sapAwardfeed.getFeedStatus());
		query.executeUpdate();
	}


	@Override
	public List<String> fetchAllBudgetFeedStatus(Integer feedId,Integer batchId) {
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		List<String> feedStatus = new ArrayList<>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			inParam.add(new Parameter("<<FEED_ID>>", DBEngineConstants.TYPE_INTEGER, feedId));
			inParam.add(new Parameter("<<BATCH_ID>>", DBEngineConstants.TYPE_INTEGER, batchId));
			output = dbEngine.executeQuery(inParam, "get_all_budget_feed_id");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmRules : output) {
					feedStatus.add((String) hmRules.get("FEED_STATUS"));
					}	
				}
			return feedStatus;
           } catch (Exception e) {
        	  e.printStackTrace();
			logger.error("Exception in fetchAllFeedStatus {}", e.getMessage());
			return new ArrayList<>();
		}		
	}

}
