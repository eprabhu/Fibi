package com.polus.fibicomp.claims.claimsIntegration.sapfeed.dao;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.ParameterMode;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedBatch;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@Service(value = "claimInvoiceIntegrationDao")
@Transactional
public class ClaimInvoiceIntegrationDaoImpl implements ClaimInvoiceIntegrationDao{
	
	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Autowired
	private CommonDao commonDao;

    protected static Logger logger = LogManager.getLogger(ClaimInvoiceIntegrationDaoImpl.class.getName());

	@Override
	public List<ClaimInvoiceLog> getClaimInvoiceLogByBatchId(Integer batchId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimInvoiceLog> criteria = builder.createQuery(ClaimInvoiceLog.class);
			Root<ClaimInvoiceLog> root = criteria.from(ClaimInvoiceLog.class);				
			criteria.where(builder.equal(root.get("batchId"),batchId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void saveOrUpdateClaimInvoiceLog(ClaimInvoiceLog claimInvoiceLog) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ClaimInvoiceLog> criteriaUpdate = cb.createCriteriaUpdate(ClaimInvoiceLog.class);
		Root<ClaimInvoiceLog> root = criteriaUpdate.from(ClaimInvoiceLog.class);
		criteriaUpdate.set("outputDocumentNumber", claimInvoiceLog.getOutputDocumentNumber());	
		criteriaUpdate.set("status", claimInvoiceLog.getStatus());		
		criteriaUpdate.set("fiscalYear", claimInvoiceLog.getFiscalYear());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		Predicate invoiceId = cb.equal(root.get("claimInvoiceLogId"),claimInvoiceLog.getClaimInvoiceLogId());
		Predicate claimNumber = cb.equal(root.get("claimNumber"),claimInvoiceLog.getClaimNumber());
		criteriaUpdate.where(cb.and(invoiceId, claimNumber));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public void saveOrUpdateClaimSapResponseMsg(SapClaimFeedResponseMessage mesg) {
		hibernateTemplate.saveOrUpdate(mesg);				
	}

	@Override
	public Integer generateInvoiceBatchId(List<String> feedIds, IntegrationReportVO vo) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			return (Integer) session.createStoredProcedureCall("SAP_CLAIM_INVOICE_LOG_FEED").
					registerStoredProcedureParameter("AV_FEED_ID ", String.class, ParameterMode.IN).setParameter("AV_FEED_ID ", feedIds != null && !feedIds.isEmpty() ? String.join(",", feedIds) : "0").getSingleResult();
		} catch (Exception e) {
			logger.error("Error in generateInvoiceBatchId {}", e.getMessage());
			vo.getEmailContent().getError().append("Error in generateInvoiceBatchId : ").append(e).append("<br/>");
		}
		return null;		
	}

	@Override
	public void updateClaimStatus(String claimNumber, String status) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<Claim> criteriaUpdate = cb.createCriteriaUpdate(Claim.class);
		Root<Claim> root = criteriaUpdate.from(Claim.class);
		criteriaUpdate.set("claimStatusCode", status.equals("E") ? Constants.CLAIM_STATUS_CODE_INVOICE_NOT_GENERATED : Constants.CLAIM_STATUS_CODE_INVOICE_GENERATED);	
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("claimNumber"),claimNumber));
		session.createQuery(criteriaUpdate).executeUpdate();	
	}

	@Override
	public void updateClaimSapFeedStatus(String claimNumber, Integer batchId, String userActionCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<SapClaimFeed> criteriaUpdate = cb.createCriteriaUpdate(SapClaimFeed.class);
		Root<SapClaimFeed> root = criteriaUpdate.from(SapClaimFeed.class);
		criteriaUpdate.set("feedStatus", "E");
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		if(userActionCode != null)
			criteriaUpdate.set("userActionCode", userActionCode);
		Predicate predicateClaimNumber = cb.equal(root.get("claimNumber"),claimNumber);
		Predicate predicateBatchId = cb.equal(root.get("batchId"),batchId);
		criteriaUpdate.where(cb.and(predicateClaimNumber, predicateBatchId));
		session.createQuery(criteriaUpdate).executeUpdate();			
	}

	@Override
	public void updateClaimBatch(String batchId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<SapClaimFeedBatch> criteriaUpdate = cb.createCriteriaUpdate(SapClaimFeedBatch.class);
		Root<SapClaimFeedBatch> root = criteriaUpdate.from(SapClaimFeedBatch.class);
		criteriaUpdate.set("responseTimestamp", commonDao.getCurrentTimestamp());	
		criteriaUpdate.where(cb.equal(root.get("batchId"),batchId));
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public void updateFeedStatusToSuccess(String batchId, String userActionCode, List<String> errorClaimNumbers) {		
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<SapClaimFeed> criteriaUpdate = cb.createCriteriaUpdate(SapClaimFeed.class);
		Root<SapClaimFeed> root = criteriaUpdate.from(SapClaimFeed.class);
		criteriaUpdate.set("feedStatus", "R");	
		if(userActionCode != null)
			criteriaUpdate.set("userActionCode", userActionCode);	
		Predicate predicateBatchId = cb.equal(root.get("batchId"),batchId);
		Predicate predicateStatus = cb.equal(root.get("feedStatus"),"F");
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName() == null ? "quickstart" : AuthenticatedUser.getLoginUserName());
		Predicate predicateClaimNumber = null;
		if(errorClaimNumbers != null && !errorClaimNumbers.isEmpty()) {
			predicateClaimNumber = cb.not(root.get("claimNumber").in(errorClaimNumbers));
			criteriaUpdate.where(cb.and(predicateStatus, predicateBatchId, predicateClaimNumber));
		} else {
			criteriaUpdate.where(cb.and(predicateStatus, predicateBatchId));
		}
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<SapClaimFeed> getClaimInvoiceBatchByBatchId(Integer batchId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<SapClaimFeed> criteria = builder.createQuery(SapClaimFeed.class);
			Root<SapClaimFeed> root = criteria.from(SapClaimFeed.class);				
			criteria.where(builder.equal(root.get("batchId"),batchId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Integer> getSapClaimCount(Integer batchId) {
		List<Integer> feedAwardCount = new ArrayList<>();
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append(" SELECT  COUNT(CASE WHEN BATCH_ID = :batchId THEN 0 END) AS CLAIMCOUNT, ");
			hqlQuery.append(" COUNT(CASE WHEN FEED_STATUS_CODE ='R' AND  BATCH_ID = :batchId THEN 0 END) AS CLAIMSUCCESS,");
			hqlQuery.append(" COUNT(CASE WHEN FEED_STATUS_CODE ='E' AND  BATCH_ID = :batchId THEN 0 END) AS CLAIMERROR, ");
			hqlQuery.append(" COUNT(CASE WHEN FEED_STATUS_CODE ='F' AND  BATCH_ID = :batchId THEN 0 END) AS CLAIMPENDING FROM SAP_CLAIM_FEED");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createNativeQuery(hqlQuery.toString());
			query.setParameter("batchId",batchId);
			List<Object[]>  sapClaimCountoutput = query.getResultList();
			if (sapClaimCountoutput != null && !sapClaimCountoutput.isEmpty()) {
				for (Object[] entity : sapClaimCountoutput) {
					feedAwardCount.add(Integer.parseInt(entity[0].toString()));
					feedAwardCount.add(Integer.parseInt(entity[1].toString()));
					feedAwardCount.add(Integer.parseInt(entity[2].toString()));
					feedAwardCount.add(Integer.parseInt(entity[3].toString()));
					}	
				}
			return feedAwardCount;
		} catch (Exception e) {
			logger.error("Error in getSapClaimCount {}", e.getMessage());
			return Collections.emptyList();          
		}		
	}

	@Override
	public ClaimInvoiceLog getClaimInvoiceLogById(Integer claimInvoiceLogId) {
		return hibernateTemplate.get(ClaimInvoiceLog.class, claimInvoiceLogId);
	}
}
