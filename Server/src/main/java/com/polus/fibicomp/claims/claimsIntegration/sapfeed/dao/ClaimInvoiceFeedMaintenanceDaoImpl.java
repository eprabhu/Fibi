package com.polus.fibicomp.claims.claimsIntegration.sapfeed.dao;

import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dto.ClaimInvoiceFeedDto;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedBatch;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.security.AuthenticatedUser;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

@Transactional
@Service(value = "claimInvoiceFeedMaintenanceDao")
public class ClaimInvoiceFeedMaintenanceDaoImpl implements ClaimInvoiceFeedMaintenanceDao {

    @Value("${oracledb}")
    private String oracledb;

    protected static Logger logger = LogManager.getLogger(ClaimInvoiceFeedMaintenanceDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

	private static final String USER_COMMENT = "userComment";
    private static final String USER_ACTION_CODE = "userActionCode";
	private static final String FEED_ID = "feedId";

    @Override
    public SapClaimFeedBatch getClaimLatestBatchDashboard() {
        try {
            Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
            Query query = session.createQuery("from SapClaimFeedBatch where batchId in (select max(batchId) from SapClaimFeedBatch)");
            return (SapClaimFeedBatch) query.getSingleResult();
        } catch (Exception e) {
            logger.info("exception in getClaimLatestBatchDashboard : " + e);
            return new SapClaimFeedBatch();
        }
    }

    @Override
    public List<ClaimInvoiceFeedDto> getSapClaimFeedDashBoard(SapFeedMaintenanceVO maintenanceVO) {
    	Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        ResultSet resultSet = null;
        List<ClaimInvoiceFeedDto> claimInvoiceFeedList = new ArrayList<>();
        try {
            statement = connection.prepareCall("{call GET_SAP_CLAIM_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
            statement.setString(1, maintenanceVO.getBatchId() != null ? String.valueOf(maintenanceVO.getBatchId()) : null);
            statement.setString(2, maintenanceVO.getProperty10());
            statement.setString(3, maintenanceVO.getProperty11());
            statement.setString(4, maintenanceVO.getProperty7());
            statement.setString(5, maintenanceVO.getProperty8());
            statement.setString(6, maintenanceVO.getProperty4().isEmpty() ? null : String.join(",", maintenanceVO.getProperty4()));
            statement.setString(7, maintenanceVO.getProperty9());
            statement.setString(8, setInvoiceSortOrder(maintenanceVO.getReverse(), maintenanceVO.getSortBy()));
            statement.setInt(9, (maintenanceVO.getCurrentPage() == null ? 0 : maintenanceVO.getCurrentPage()  - 1));
            statement.setInt(10, (maintenanceVO.getPageNumber() == null ? 0 : maintenanceVO.getPageNumber()));
            statement.setString(11, maintenanceVO.getTabName());
            statement.setBoolean(12, maintenanceVO.getIsDownload());
            statement.setString(13, maintenanceVO.getIsAdvanceSearch());
            statement.execute();
            resultSet = statement.getResultSet();
            while (resultSet.next()) {
        		ClaimInvoiceFeedDto claimInvoiceFeedDto = new ClaimInvoiceFeedDto();
            	if(!maintenanceVO.getTabName().equals("BATCH_HISTORY")) {
                    claimInvoiceFeedDto.setAccountNumber(resultSet.getString("ACCOUNT_NUMBER"));
                    claimInvoiceFeedDto.setClaimNumber(resultSet.getString("CLAIM_NUMBER"));
                    claimInvoiceFeedDto.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
                    claimInvoiceFeedDto.setFeedId(resultSet.getInt("FEED_ID"));
                    claimInvoiceFeedDto.setFeedType(resultSet.getString("FEED_TYPE"));
                    claimInvoiceFeedDto.setFeedStatus(resultSet.getString("FEED_STATUS"));
                    claimInvoiceFeedDto.setUserAction(resultSet.getString("USER_ACTION"));
                    claimInvoiceFeedDto.setBatchId(resultSet.getInt("BATCH_ID"));
                    claimInvoiceFeedDto.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
                    claimInvoiceFeedDto.setUpdateUser(resultSet.getString("UPDATE_USER_NAME"));
                    claimInvoiceFeedDto.setFeedStatusCode(resultSet.getString("FEED_STATUS_CODE"));
                    claimInvoiceFeedDto.setFeedTypeCode(resultSet.getString("FEED_TYPE_CODE"));
                    claimInvoiceFeedDto.setUserActionCode(resultSet.getString("USER_ACTION_CODE"));
                    claimInvoiceFeedDto.setClaimId(resultSet.getInt("CLAIM_ID"));
                    claimInvoiceFeedDto.setUserActionComment(resultSet.getString("USER_COMMENT"));
                    claimInvoiceFeedDto.setInvoiceId(resultSet.getInt("INVOICE_ID"));
                    claimInvoiceFeedDto.setCanRevision(resultSet.getBoolean("CAN_REVISION"));
            	} else {
            		claimInvoiceFeedDto.setBatchId(resultSet.getInt("BATCH_ID"));
                    claimInvoiceFeedDto.setCreateTimeStamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
                    claimInvoiceFeedDto.setResponseTimeStamp(resultSet.getTimestamp("RESPONSE_TIMESTAMP"));
                    claimInvoiceFeedDto.setCountOfRecords(resultSet.getInt("NO_OF_RECORDS"));
                    claimInvoiceFeedDto.setErrorCount(resultSet.getInt("TOTAL_ERROR_INVOICE"));
            	}
                claimInvoiceFeedList.add(claimInvoiceFeedDto);             
            }
            maintenanceVO.setTotalCount(getSapClaimFeedDashBoardCount(maintenanceVO));
        }catch (Exception e) {
            logger.error("Error in getSapClaimFeedDashBoard {}", e.getMessage());
        }
        return claimInvoiceFeedList;
    }
    
    private String setInvoiceSortOrder(String order, String sortBy) {
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
				sortOrder = " ORDER BY T.TOTAL_ERROR_INVOICE "+ order;
				break;
			case "countOfRecords":
				sortOrder = " ORDER BY T.NO_OF_RECORDS "+ order;
				break;
			default:
				break;
			}
		}
		return sortOrder;
	}
    
    private Integer getSapClaimFeedDashBoardCount(SapFeedMaintenanceVO maintenanceVO) {
    	Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        SessionImpl sessionImpl = (SessionImpl) session;
        Connection connection = sessionImpl.connection();
        CallableStatement statement = null;
        ResultSet resultSet = null;
        Integer count = 0;
        try {
            statement = connection.prepareCall("{call GET_SAP_CLAIM_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
            statement.setString(1, maintenanceVO.getBatchId() != null ? String.valueOf(maintenanceVO.getBatchId()) : null);
            statement.setString(2, maintenanceVO.getProperty10());
            statement.setString(3, maintenanceVO.getProperty11());
            statement.setString(4, maintenanceVO.getProperty7());
            statement.setString(5, maintenanceVO.getProperty8());
            statement.setString(6, maintenanceVO.getProperty4().isEmpty() ? null : String.join(",", maintenanceVO.getProperty4()));
            statement.setString(7, maintenanceVO.getProperty9());
            statement.setString(8, setInvoiceSortOrder(maintenanceVO.getReverse(), maintenanceVO.getSortBy()));
            statement.setInt(9, 0);
            statement.setInt(10, 0);
            statement.setString(11, maintenanceVO.getTabName());
            statement.setBoolean(12, true);
            statement.setString(13, maintenanceVO.getIsAdvanceSearch());
            statement.execute();
            resultSet = statement.getResultSet();
            while (resultSet.next()) {
            	count = Integer.parseInt(resultSet.getString(1));              
            }
        }catch (Exception e) {
            logger.error("Error in getSapClaimFeedDashBoard {}", e.getMessage());
        }
        return count;
    }		

    @Override
    public SapClaimFeedBatch getSapFeedClaimBatchById(Integer batchId) {
        return hibernateTemplate.execute(session -> {
            CriteriaBuilder builder = session.getCriteriaBuilder();
            CriteriaQuery<SapClaimFeedBatch> criteria = builder.createQuery(SapClaimFeedBatch.class);
            Root<SapClaimFeedBatch> root = criteria.from(SapClaimFeedBatch.class);
            criteria.where(builder.equal(root.get("batchId"),batchId));
            return session.createQuery(criteria).getSingleResult();
        });
    }

	@Override
	public Integer getSapClaimBatchCount(Integer batchId) {
		try {
	        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
	        Query query = session.createQuery("select count(1) from SapClaimFeed where batchId =:batchId and feedStatus = 'E' ");
			query.setParameter("batchId", batchId);
	        return ((Long) query.getSingleResult()).intValue();
		} catch (Exception e) {
	        logger.info("exception in getSapClaimBatchCount : " + e);
	        return 0;
		}
	}

	@Override
	public List<ClaimInvoiceLog> loadClaimInvoiceLog(Integer invoiceId, Integer batchId) {
		return hibernateTemplate.execute(session -> {
            CriteriaBuilder builder = session.getCriteriaBuilder();
            CriteriaQuery<ClaimInvoiceLog> criteria = builder.createQuery(ClaimInvoiceLog.class);
            Root<ClaimInvoiceLog> root = criteria.from(ClaimInvoiceLog.class);
        	Predicate predicateInvoiceId = builder.equal(root.get("invoiceId"),invoiceId);
    		Predicate predicateBatchId = builder.equal(root.get("batchId"),batchId);
    		criteria.where(builder.and(predicateInvoiceId, predicateBatchId));
    		criteria.orderBy(builder.asc(root.get("claimInvoiceLogId")));
            return session.createQuery(criteria).getResultList();
	    });
	}

	@Override
	public void updateSapClaimFeedUserAction(String userActionCode, String userComment, Integer feedId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createQuery("update SapClaimFeed set userActionCode =:userActionCode, userComment = :userComment, updateTimeStamp =:updateTimeStamp, updateUser =:updateUser where feedId = :feedId");
		query.setParameter(USER_ACTION_CODE, userActionCode);
		query.setParameter(FEED_ID, feedId);
		query.setParameter(USER_COMMENT, userComment);
		query.setParameter("updateTimeStamp", commonDao.getCurrentTimestamp());
		query.setParameter("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		query.executeUpdate();
	}

	@Override
	public void updateClaimInvoiceLog(Integer invoiceLogId, String outputDocNumber, String fiscalyear) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ClaimInvoiceLog> criteriaUpdate = cb.createCriteriaUpdate(ClaimInvoiceLog.class);
		Root<ClaimInvoiceLog> root = criteriaUpdate.from(ClaimInvoiceLog.class);
		criteriaUpdate.set("outputDocumentNumber", outputDocNumber);	
		criteriaUpdate.set("fiscalYear", fiscalyear);	
		criteriaUpdate.set("status", "S");
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(cb.equal(root.get("claimInvoiceLogId"),invoiceLogId));
		session.createQuery(criteriaUpdate).executeUpdate();
		
	}

	@Override
	public void updateSapClaimFeed(Integer feedId, String userAction, String userComment) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<SapClaimFeed> criteriaUpdate = cb.createCriteriaUpdate(SapClaimFeed.class);
		Root<SapClaimFeed> root = criteriaUpdate.from(SapClaimFeed.class);
		criteriaUpdate.set("feedStatus", "R");
		criteriaUpdate.set("userActionCode", userAction);
		criteriaUpdate.set("userComment", userComment);
		criteriaUpdate.set("noFeedFlag", false);
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(cb.equal(root.get("feedId"),feedId));
		session.createQuery(criteriaUpdate).executeUpdate();	
	}

	@Override
	public List<SapClaimFeed> getClaimFeedByIds(List<Integer> feedIds) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();						
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapClaimFeed> criteria = builder.createQuery(SapClaimFeed.class);
			Root<SapClaimFeed> root = criteria.from(SapClaimFeed.class);	
			criteria.where(root.get("feedId").in(feedIds));
			return session.createQuery(criteria).getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}		
	}

	@Override
	public List<SapClaimFeedResponseMessage> getSapClaimErrorMsg(List<Integer> claimInvoiceLogIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();						
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SapClaimFeedResponseMessage> criteria = builder.createQuery(SapClaimFeedResponseMessage.class);
		Root<SapClaimFeedResponseMessage> root = criteria.from(SapClaimFeedResponseMessage.class);	
		criteria.where(root.get("claimInvoiceLogId").in(claimInvoiceLogIds));
		return session.createQuery(criteria).getResultList();
	}
}
