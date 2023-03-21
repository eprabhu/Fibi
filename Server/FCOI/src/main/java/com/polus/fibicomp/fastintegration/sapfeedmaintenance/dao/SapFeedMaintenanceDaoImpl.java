package com.polus.fibicomp.fastintegration.sapfeedmaintenance.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;
import com.polus.fibicomp.fastintegration.pojo.SapFeedStatus;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFmBudget;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFundedPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantBudMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplProjectDef;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsoPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsorClass;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplWbs;
import com.polus.fibicomp.fastintegration.pojo.SapFeedType;
import com.polus.fibicomp.fastintegration.pojo.SapFeedUserAction;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "sapFeedMaintenanceDao")
public class SapFeedMaintenanceDaoImpl implements SapFeedMaintenanceDao {

	protected static Logger logger = LogManager.getLogger(SapFeedMaintenanceDaoImpl.class.getName());

	@Autowired
	public HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	CommonDao commonDao;
	private static final String USER_COMMENT = "userComment";
    private static final String USER_ACTION_CODE = "userActionCode";
	private static final String FEED_ID = "feedId";
    private static final String IS_ACTIVE = "isActive";

	@Override
	public Integer getLatesSapAwardFeedBatchId() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT MAX(batchId) FROM SapAwardFeedBatch";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		return query.uniqueResult();
	}

	@Override
	public List<SapAwardFeed> getSapFeedMaintenanceBatchDetail(SapFeedMaintenanceVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<SapAwardFeed> sapAwardFeeds = new ArrayList<>();
		String property1 = vo.getProperty1() == null ? null : vo.getProperty1().toString();
		String property2 = vo.getProperty2() == null ? null : vo.getProperty2().toString();
		String property3 = vo.getProperty3();
		List<String> property4 = vo.getProperty4();
		Timestamp property5 = vo.getProperty5();
		Timestamp property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SAP_FEED_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4 == null || property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.setInt(8, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage()  - 1));
				statement.setString(9, setSapFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(10, (vo.getPageNumber() == null ? 0 : vo.getPageNumber()));
				statement.setString(11, vo.getTabName());
				statement.setBoolean(12, vo.getIsDownload());
				statement.setString(13, vo.getIsAdvanceSearch());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SAP_FEED_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.setInt(8, (vo.getCurrentPage() == null ? 0 : vo.getCurrentPage()  - 1));
				statement.setString(9, setSapFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(10, (vo.getPageNumber() == null ? 0 : vo.getPageNumber()));
				statement.setString(11, vo.getTabName());
				statement.setBoolean(12, vo.getIsDownload());
				statement.setString(13, vo.getIsAdvanceSearch());
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet != null && resultSet.next()) {
				if (vo.getTabName().equals("BATCH_DETAIL") || vo.getTabName().equals("PENDING_FEEDS")) {
					SapAwardFeed sapAwardFeed = new SapAwardFeed();
					sapAwardFeed.setFeedId(resultSet.getInt("FEED_ID"));
					sapAwardFeed.setBatchId(resultSet.getString("BATCH_ID") != null ? Integer.parseInt(resultSet.getString("BATCH_ID")) : null);
					sapAwardFeed.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
					sapAwardFeed.setAccountNumber(resultSet.getString("ACCOUNT_NUMBER"));
					sapAwardFeed.setFeedType(resultSet.getString("FEED_TYPE"));
					sapAwardFeed.setFeedStatus(resultSet.getString("FEED_STATUS"));
					sapAwardFeed.setUserActionCode(resultSet.getString("USER_ACTION_CODE"));
					sapAwardFeed.setUserComment(resultSet.getString("USER_COMMENT"));
					sapAwardFeed.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
					sapAwardFeed.setUpdateUserFullName(resultSet.getString("UPDATE_USER_NAME"));
					sapAwardFeed.setAwardId(resultSet.getInt("AWARD_ID"));
					sapAwardFeed.setFeedTypeDesc(resultSet.getString("FEED_TYPE_DESC"));
					sapAwardFeed.setFeedStatusDesc(resultSet.getString("FEED_STATUS_DESC"));
					sapAwardFeed.setUserActionDesc(resultSet.getString("USER_ACTION_CODE_DESC"));
					sapAwardFeed.setSequenceNumber(resultSet.getInt("SEQUENCE_NUMBER"));
					sapAwardFeed.setUpdateUser(resultSet.getString("UPDATE_USER"));
					sapAwardFeed.setPiName(resultSet.getString("PI_NAME"));
					sapAwardFeeds.add(sapAwardFeed);
				} else {
					SapAwardFeed sapAwardFeed = new SapAwardFeed();
					sapAwardFeed.setBatchId(resultSet.getString("BATCH_ID") != null ? Integer.parseInt(resultSet.getString("BATCH_ID")) : null);
					sapAwardFeed.setTotalAwards(resultSet.getInt("TOTAL_AWARDS"));
					sapAwardFeed.setTotalErrorAwards(resultSet.getInt("TOTAL_ERROR_AWARDS"));
					sapAwardFeed.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
					sapAwardFeed.setUpdateUserFullName(resultSet.getString("FULL_NAME"));
					sapAwardFeed.setResponseTimestamp(resultSet.getTimestamp("RESPONSE_TIMESTAMP"));
					sapAwardFeeds.add(sapAwardFeed);
				}
			}
		} catch (Exception e) {
			logger.error("Exception in advancedSearchResult {}", e.getMessage());
			e.printStackTrace();
		}
		return sapAwardFeeds;
	}

	private String setSapFeedSortOrder(String order, String sortBy) {
		String sortOrder = null;
		if (sortBy != null && !sortBy.isEmpty()) {	
			switch (sortBy) {
			case "feedId":
				sortOrder = " ORDER BY T.FEED_ID "+ order;
				break;
			case "batchId":
				sortOrder = " ORDER BY T.BATCH_ID "+ order;
				break;
			case "claimNumber":
				sortOrder = " ORDER BY T.CLAIM_NUMBER  "+ order;
				break;
			case "userAction":
				sortOrder = " ORDER BY T.USER_ACTION "+ order;
				break;
			case "feedStatus":
				sortOrder = " ORDER BY T.FEED_STATUS "+ order;
				break;
			case "feedType":
				sortOrder = " ORDER BY T.FEED_TYPE "+ order;
				break;
			case "updateTimeStamp":
				sortOrder = " ORDER BY T.UPDATE_TIMESTAMP "+ order;
				break;
			case "updateUser":
				sortOrder = " ORDER BY T.UPDATE_USER_NAME "+ order;
				break;
			case "accountNumber":
				sortOrder = " ORDER BY T.ACCOUNT_NUMBER "+ order;
				break;
			case "piName":
				sortOrder = " ORDER BY T.PI_NAME "+ order;
				break;				
			case "awardNumber":
				sortOrder = " ORDER BY T.AWARD_NUMBER "+ order;
				break;
			case "createTimeStamp":
				sortOrder =  " ORDER BY T.CREATE_TIMESTAMP "+ order;
				break;
			case "responseTimeStamp":
				sortOrder = " ORDER BY T.RESPONSE_TIMESTAMP "+ order;
				break;
			case "errorCount":
				sortOrder = " ORDER BY T.TOTAL_ERROR_AWARDS "+ order;
				break;
			case "totalAwards":
				sortOrder = " ORDER BY T.TOTAL_AWARDS "+ order;
				break;
			case "feedTypeDesc":
				sortOrder = " ORDER BY T.FEED_TYPE_DESC "+ order;
				break;
			case "feedStatusDesc":
				sortOrder = " ORDER BY T.FEED_STATUS_DESC "+ order;
				break;
			case "userActionDesc":
				sortOrder = " ORDER BY T.USER_ACTION_CODE_DESC "+ order;
				break;				
			default:
				break;
			}
		}
		return sortOrder;
	}

	@Override
	public List<SapFeedStatus> getSapAwardFeedStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedStatus> query = builder.createQuery(SapFeedStatus.class);
		Root<SapFeedStatus> rootSapFeedStatus = query.from(SapFeedStatus.class);
		query.where(builder.and(builder.equal(rootSapFeedStatus.get(IS_ACTIVE), Constants.YES)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public SapFeedStatus getSapFeedStatusByCode(String feedStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedStatus> query = builder.createQuery(SapFeedStatus.class);
		Root<SapFeedStatus> rootSapFeedStatus = query.from(SapFeedStatus.class);
		query.where(builder.and(builder.equal(rootSapFeedStatus.get("feedStatusCode"), feedStatusCode)));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public SapFeedType getSapFeedTypeByCode(String feedTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedType> query = builder.createQuery(SapFeedType.class);
		Root<SapFeedType> rootSapFeedType = query.from(SapFeedType.class);
		query.where(builder.and(builder.equal(rootSapFeedType.get("feedTypeCode"), feedTypeCode)));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public List<SapFeedTmplFmBudget> getSapFeedTmplFmBudgetbyFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplFmBudget> query = builder.createQuery(SapFeedTmplFmBudget.class);
		Root<SapFeedTmplFmBudget> rootSapFeedTmplFmBudget = query.from(SapFeedTmplFmBudget.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplFmBudget.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapFeedTmplFundedPrgm> getSapFeedTmplFundedPrgmByFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplFundedPrgm> query = builder.createQuery(SapFeedTmplFundedPrgm.class);
		Root<SapFeedTmplFundedPrgm> rootSapFeedTmplFundedPrgm = query.from(SapFeedTmplFundedPrgm.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplFundedPrgm.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapFeedTmplGrantBudMaster> getSapFeedTmplGrantBudMasterByFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplGrantBudMaster> query = builder.createQuery(SapFeedTmplGrantBudMaster.class);
		Root<SapFeedTmplGrantBudMaster> rootSapFeedTmplGrantBudMaster = query.from(SapFeedTmplGrantBudMaster.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplGrantBudMaster.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapFeedTmplGrantMaster> getSapFeedTmplGrantMasterByFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplGrantMaster> query = builder.createQuery(SapFeedTmplGrantMaster.class);
		Root<SapFeedTmplGrantMaster> rootSapFeedTmplGrantMaster = query.from(SapFeedTmplGrantMaster.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplGrantMaster.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapFeedTmplProjectDef> getSapFeedTmplProjectDefByFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplProjectDef> query = builder.createQuery(SapFeedTmplProjectDef.class);
		Root<SapFeedTmplProjectDef> rootSapFeedTmplProjectDef = query.from(SapFeedTmplProjectDef.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplProjectDef.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapFeedTmplSponsoPrgm> getSapFeedTmplSponsoPrgmByFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplSponsoPrgm> query = builder.createQuery(SapFeedTmplSponsoPrgm.class);
		Root<SapFeedTmplSponsoPrgm> rootSapFeedTmplSponsoPrgm = query.from(SapFeedTmplSponsoPrgm.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplSponsoPrgm.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapFeedTmplSponsorClass> getSapFeedTmplSponsorClasseByFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplSponsorClass> query = builder.createQuery(SapFeedTmplSponsorClass.class);
		Root<SapFeedTmplSponsorClass> rootSapFeedTmplSponsorClass = query.from(SapFeedTmplSponsorClass.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplSponsorClass.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapFeedTmplWbs> getSapFeedTmplWbsByFeedId(Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedTmplWbs> query = builder.createQuery(SapFeedTmplWbs.class);
		Root<SapFeedTmplWbs> rootSapFeedTmplWbs = query.from(SapFeedTmplWbs.class);
		query.where(builder.and(builder.equal(rootSapFeedTmplWbs.get(FEED_ID), feedId)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SapAwardFeed> getBatchHistoryDetail(SapFeedMaintenanceVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<SapAwardFeed> sapAwardFeeds = new ArrayList<>();
		String property1 = vo.getProperty1() == null ? null : vo.getProperty1().toString();
		String property2 = vo.getProperty2() == null ? null : vo.getProperty2().toString();
		String property3 = vo.getProperty3();
		List<String> property4 = vo.getProperty4();
		Timestamp property5 = vo.getProperty5();
		Timestamp property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SAP_FEED_BATCH_HISTORY(?,?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4 == null || property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.setInt(8, vo.getCurrentPage() != null ? vo.getCurrentPage():1);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "SAP_FEED_BATCH_HISTORY";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.setInt(8, vo.getCurrentPage() != null ? vo.getCurrentPage():1);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet != null && resultSet.next()) {
				SapAwardFeed sapAwardFeed = new SapAwardFeed();
				sapAwardFeed.setBatchId(resultSet.getInt("BATCH_ID"));
				sapAwardFeed.setTotalAwards(resultSet.getInt("TOTAL_AWARDS"));
				sapAwardFeed.setTotalErrorAwards(resultSet.getInt("TOTAL_ERROR_AWARDS"));
				sapAwardFeed.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
				sapAwardFeed.setUpdateUserFullName(resultSet.getString("FULL_NAME"));
				sapAwardFeed.setResponseTimestamp(resultSet.getTimestamp("RESPONSE_TIMESTAMP"));
				sapAwardFeeds.add(sapAwardFeed);
			}
		} catch (SQLException e) {
			logger.error("Error in getBatchHistoryDetail {}", e.getMessage());
		}
		return sapAwardFeeds;
	}

	@Override
	public void updateSapAwardFeedById(String userComment, String userActionCode, List<Integer> feedIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapAwardFeed set userComment = :userComment , feedStatus = 'R', userActionCode =:userActionCode, updateTimeStamp = utc_timestamp(), updateUser =:updateUser where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(USER_ACTION_CODE, userActionCode);
		query.setParameter(FEED_ID, feedIds);
		query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
		query.executeUpdate();
	}

	@Override
	public List<SapAwardFeed> getSapAwardFeeds(List<Integer> feedIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapAwardFeed> query = builder.createQuery(SapAwardFeed.class);
		Root<SapAwardFeed> rootSapAwardFeed = query.from(SapAwardFeed.class);
		query.where(builder.and(rootSapAwardFeed.get(FEED_ID).in(feedIds)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void updateSapAwardFeedByStatus(String status, String changeStatus, String userComment, String userActionCode,  List<Integer> feedIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapAwardFeed set userComment = :userComment , feedStatus =:changeStatus , userActionCode =:userActionCode, updateTimeStamp = utc_timestamp(), updateUser =:updateUser where feedId in (:feedId) and feedStatus =:status");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter("changeStatus", changeStatus);
		query.setParameter(USER_ACTION_CODE, userActionCode);
		query.setParameter(FEED_ID, feedIds);
		query.setParameter("status", status);
		query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
		query.executeUpdate();
	}

	@Override
	public void updateSapAwardFeedUserAction(String userActionCode, String userComment, Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapAwardFeed set userActionCode =:userActionCode, userComment = :userComment, updateTimeStamp = utc_timestamp(), updateUser =:updateUser where feedId = :feedId");
		query.setParameter(USER_ACTION_CODE, userActionCode);
		query.setParameter(FEED_ID, feedId);
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
		query.executeUpdate();
	}

	@Override
	public Integer reInterfaceSapAwardFeed(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser,
			String feedType) {
		Integer result = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SAP_FEED_REINTERFACE (?,?,?,?,?)}");
				statement.setInt(1, awardId);
				statement.setString(2, awardNumber);
				statement.setInt(3, sequenceNumber);
				statement.setString(4, updateUser);
				statement.setString(5, feedType);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				statement = connection.prepareCall("{call SAP_FEED_REINTERFACE (?,?,?,?,?,?)}");
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, awardId);
				statement.setString(3, awardNumber);
				statement.setInt(4, sequenceNumber);
				statement.setString(5, updateUser);
				statement.setString(6, feedType);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet != null && resultSet.next()) {
				result = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			logger.error("Exception in awardRevenueTrackerRefresh : {}", e.getMessage());
		}
		return result; 
	}

	@Override
	public void updateSponsorClass(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplSponsorClass set  feedStatus = 'S', userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public void updateSponsoPrgm(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplSponsoPrgm set  feedStatus = 'S', userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public void updateFundedPrgm(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplFundedPrgm set  feedStatus = 'S' , userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public void updateGrantMaster(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplGrantMaster set  feedStatus = 'S', userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public void updateProjectDef(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplProjectDef set  feedStatus = 'S', userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public void updateGrantBudMaster(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplGrantBudMaster set  feedStatus = 'S', userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public void updateWBS(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplWbs set  feedStatus = 'S', userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public void updateFmBudgets(List<Integer> feedIds, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapFeedTmplFmBudget set  feedStatus = 'S', userComment = :userComment where feedId in (:feedId) and feedStatus ='E'");
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter(FEED_ID, feedIds);
		query.executeUpdate();
	}

	@Override
	public AwardBudgetHeader getAwardBudgetHeaderByParam(String awardNumber, Integer sequenceNumber) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append(" FROM AwardBudgetHeader WHERE versionNumber = (select MAX(versionNumber) FROM AwardBudgetHeader");
			hqlQuery.append(" where awardNumber=:awardNumber and sequenceNumber =:sequenceNumber) and awardNumber=:awardNumber and sequenceNumber =:sequenceNumber");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardNumber", awardNumber);
			query.setParameter("sequenceNumber", sequenceNumber);
			return (AwardBudgetHeader) query.getSingleResult();
		} catch (Exception e) {
			logger.error("Exception in getAwardBudgetHeaderByParam : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public ServiceRequestType getServiceRequestTypeByAwardId(Integer awardId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append(" FROM ServiceRequestType where typeCode = (select awardVariationTypeCode from  Award WHERE awardId = :awardId)");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardId", awardId);
			return (ServiceRequestType) query.getSingleResult();
		} catch (Exception e) {
			logger.error("Exception in getServiceRequestTypeByAwardId : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public List<SapFeedUserAction> getSapFeedUserAction() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedUserAction> query = builder.createQuery(SapFeedUserAction.class);
		Root<SapFeedUserAction> rootSapFeedUserAction = query.from(SapFeedUserAction.class);
		query.where(builder.and(builder.equal(rootSapFeedUserAction.get(IS_ACTIVE), Constants.YES)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public SapFeedUserAction getSapFeedUserActionByCode(String userActionCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapFeedUserAction> query = builder.createQuery(SapFeedUserAction.class);
		Root<SapFeedUserAction> rootSapFeedUserAction = query.from(SapFeedUserAction.class);
		Predicate predicateIsActive = builder.equal(rootSapFeedUserAction.get(IS_ACTIVE), Constants.YES);
		Predicate predicateUserActionCode = builder.equal(rootSapFeedUserAction.get(USER_ACTION_CODE), userActionCode);
		query.where(builder.and(predicateIsActive, predicateUserActionCode));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public List<SapAwardFeed> getAllConcurrentFeedIds(Integer feedId, List<String> feedStatuses) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapAwardFeed> mainQuery = builder.createQuery(SapAwardFeed.class);
		Root<SapAwardFeed> rootSapAwardFeed = mainQuery.from(SapAwardFeed.class);
		Predicate predicateBatchId = rootSapAwardFeed.get("batchId").isNull();
		Predicate predicateFeed = rootSapAwardFeed.get("feedStatus").in(feedStatuses);
		Subquery<String> subQueryTwo = mainQuery.subquery(String.class);
		Root<SapAwardFeed> rootSubSapAwardFeed = subQueryTwo.from(SapAwardFeed.class);
		Predicate predicateFeedId = builder.equal(rootSubSapAwardFeed.get("feedId"), feedId);
		Predicate predicateFeedStatus = builder.equal(rootSubSapAwardFeed.get("feedStatus"), feedStatuses.get(0));
		subQueryTwo.select(rootSubSapAwardFeed.get("awardNumber"));
		subQueryTwo.where(builder.and(predicateFeedId, predicateFeedStatus));
		Predicate predicateJoin = builder.and(predicateBatchId, predicateFeed, builder.in(rootSapAwardFeed.get("awardNumber")).value(subQueryTwo));
		mainQuery.where(builder.and(predicateJoin)); 																			 
		return session.createQuery(mainQuery).getResultList();
	}

	@Override
	public Integer getSapFeedMaintenanceBatchDetailCount(SapFeedMaintenanceVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String property1 = vo.getProperty1() == null ? null : vo.getProperty1().toString();
		String property2 = vo.getProperty2() == null ? null : vo.getProperty2().toString();
		String property3 = vo.getProperty3();
		List<String> property4 = vo.getProperty4();
		Timestamp property5 = vo.getProperty5();
		Timestamp property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		Integer count = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SAP_FEED_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4 == null || property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.setInt(8, vo.getCurrentPage() != null ? vo.getCurrentPage() : 1);
				statement.setString(9, setSapFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(10, (vo.getPageNumber() == null ? 0 : vo.getPageNumber()));
				statement.setString(11, vo.getTabName());
				statement.setBoolean(12, true);
				statement.setString(13, vo.getIsAdvanceSearch());
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SAP_FEED_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.setInt(8, vo.getCurrentPage() != null ? vo.getCurrentPage() : 1);
				statement.setString(9, setSapFeedSortOrder(vo.getReverse(), vo.getSortBy()));
				statement.setInt(10, (vo.getPageNumber() == null ? 0 : vo.getPageNumber()));
				statement.setString(11, vo.getTabName());
				statement.setBoolean(12, true);
				statement.setString(13, vo.getIsAdvanceSearch());
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet != null && resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			logger.error("Exception in advancedSearchResult {}",  e.getMessage());
			e.printStackTrace();
		}
		return count;
	}

	@Override
	public Integer getBatchHistoryDetailCount(SapFeedMaintenanceVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String property1 = vo.getProperty1() == null ? null : vo.getProperty1().toString();
		String property2 = vo.getProperty2() == null ? null : vo.getProperty2().toString();
		String property3 = vo.getProperty3();
		List<String> property4 = vo.getProperty4();
		Timestamp property5 = vo.getProperty5();
		Timestamp property6 = vo.getProperty6();
		String property7 = vo.getProperty7();
		Integer count = 0;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call SAP_FEED_BATCH_HISTORY_COUNT(?,?,?,?,?,?,?)}");
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4 == null || property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "SAP_FEED_BATCH_HISTORY_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(1, property1);
				statement.setTimestamp(2, property5);
				statement.setTimestamp(3, property6);
				statement.setString(4, property2);
				statement.setString(5, property3);
				statement.setString(6, property4.isEmpty() ? null : String.join(",", property4));
				statement.setString(7, property7);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet != null && resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getBatchHistoryDetail {}", e.getMessage());
		}
		return count;
	}
}
