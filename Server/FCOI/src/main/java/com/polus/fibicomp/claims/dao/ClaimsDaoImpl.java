package com.polus.fibicomp.claims.dao;

import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.persistence.ParameterMode;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import com.polus.fibicomp.claims.pojo.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.annotations.QueryHints;
import org.hibernate.internal.SessionImpl;
import org.hibernate.procedure.ProcedureCall;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.view.CustomDataView;

import oracle.jdbc.OracleTypes;

@Service(value = "claimsDao")
@Transactional
public class ClaimsDaoImpl implements ClaimsDao {

	protected static Logger logger = LogManager.getLogger(ClaimsDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;
	
	@Autowired
	private ExcelityService excelityService;
	
	@Override
	public Claim insertClaim(Claim claim, String acType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String procedureName = "INSERT_CLAIM_DETAILS";
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call INSERT_CLAIM_DETAILS(?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, claim.getAwardId().toString());
				statement.setString(2, claim.getTitle());
				statement.setDate(3, new java.sql.Date(claim.getStartDate().getTime()));
				statement.setDate(4, new java.sql.Date(claim.getEndDate().getTime()));
				statement.setString(5, claim.getCreateUser());
				statement.setString(6, claim.getDuration());
				statement.setString(7, acType);
				statement.setBigDecimal(8, claim.getTotalAmount());
				statement.setDate(9, claim.getClaimSubmissionDate() != null ? new java.sql.Date(claim.getClaimSubmissionDate().getTime()) : null);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, claim.getAwardId().toString());
				statement.setString(3, claim.getTitle());
				statement.setDate(4, new java.sql.Date(claim.getStartDate().getTime()));
				statement.setDate(5, new java.sql.Date(claim.getEndDate().getTime()));	
				statement.setString(6, claim.getCreateUser());
				statement.setString(7, claim.getDuration());
				statement.setString(8, acType);
				statement.setBigDecimal(9, claim.getTotalAmount());
				statement.setDate(10, claim.getClaimSubmissionDate() != null ? new java.sql.Date(claim.getClaimSubmissionDate().getTime()) : null);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				claim.setClaimId(resultSet.getInt("LI_CLAIM_ID"));
				claim.setClaimNumber(resultSet.getString("LS_CLAIM"));
			}	
		}catch (Exception e) {
			logger.error("Error in insertClaim {}", e.getMessage());
		}
		return claim;
	}

	@Override
	public void saveOrUpdateClaim(Claim claim) {
		hibernateTemplate.saveOrUpdate(claim);
	}

	@Override
	public Claim getClaim(Integer claimId) {
		return hibernateTemplate.get(Claim.class, claimId);
	}

	@Override
	public ClaimStatus getClaimStatusByStatusCode(String claimStatusCode) {
		return hibernateTemplate.get(ClaimStatus.class, claimStatusCode);
	}

	@Override
	public void updateClaimDetailByParams(Integer claimId, String claimStatusCode, Boolean isSubmit, String updateUser, Date funderApprovalDate) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<Claim> criteriaUpdate = cb.createCriteriaUpdate(Claim.class);
		Root<Claim> root = criteriaUpdate.from(Claim.class);
		criteriaUpdate.set("claimStatusCode", claimStatusCode);
		if(funderApprovalDate != null) {
			criteriaUpdate.set("funderApprovalDate", funderApprovalDate);
		}
		criteriaUpdate.where(cb.equal(root.get("claimId"), claimId));
		session.createQuery(criteriaUpdate).executeUpdate();
		updateClaimActionLog(claimId, claimStatusCode, null);
	}

	@Override
	public void updateClaimActionLog(Integer claimId, String claimStatusCode, String acType) {
		if(acType != null || !claimStatusCode.equals(Constants.CLAIM_STATUS_CODE_PENDING)) {
			ClaimActionLog actionLog = new ClaimActionLog();
			actionLog.setClaimId(claimId);
			actionLog.setClaimStatusCode(claimStatusCode);
			hibernateTemplate.saveOrUpdate(actionLog);
		}
	}

	@Override
	public ClaimSummaryDetails getClaimSummaryDetailByExpenseTransactionId(Integer awardExpenseTransactionId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimSummaryDetails> criteria = builder.createQuery(ClaimSummaryDetails.class);
			Root<ClaimSummaryDetails> protocolRoot = criteria.from(ClaimSummaryDetails.class);
			Predicate expense = builder.equal(protocolRoot.get("awardExpenseTransId"),awardExpenseTransactionId);
			criteria.where(builder.and(expense));
			List<ClaimSummaryDetails> summaryDetails = session.createQuery(criteria).getResultList();
			if(!summaryDetails.isEmpty()){
				return summaryDetails.get(0);
			}
			return null;
		});
	}

	@Override
	public String getClaimStatusCodeByClaimId(Integer claimId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> criteria = builder.createQuery(String.class);
		Root<Claim> root = criteria.from(Claim.class);
		criteria.select(root.get("claimStatusCode"));
		criteria.where(builder.and(builder.equal(root.get("claimId"), claimId)));
		return session.createQuery(criteria).getSingleResult();
	}
	
	@Override
	public List<ClaimSummary> loadClaimSummary(Integer claimId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ClaimSummary> criteria = builder.createQuery(ClaimSummary.class);
		Root<ClaimSummary> protocolRoot = criteria.from(ClaimSummary.class);				
		criteria.where(builder.and(builder.equal(protocolRoot.get("claimId"), claimId)));
		return session.createQuery(criteria).getResultList();		
	}

	@Override
	public List<ClaimSummaryDetails> loadClaimDetailBreakDown(Integer claimId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimSummaryDetails> criteria = builder.createQuery(ClaimSummaryDetails.class);
			Root<ClaimSummaryDetails> root = criteria.from(ClaimSummaryDetails.class);				
			Predicate predicateClaimId = builder.equal(root.get("claimId"),claimId);
			criteria.where(builder.and(predicateClaimId));
			criteria.orderBy(builder.asc(root.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void updateClaimBreakDown(ClaimSummaryDetails claimSummaryDetail) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ClaimSummaryDetails> criteriaUpdate = cb.createCriteriaUpdate(ClaimSummaryDetails.class);
		Root<ClaimSummaryDetails> root = criteriaUpdate.from(ClaimSummaryDetails.class);
		criteriaUpdate.set("adjustedTotal", claimSummaryDetail.getAdjustedTotal());
		criteriaUpdate.set("totalAmount", claimSummaryDetail.getTotalAmount());
		criteriaUpdate.set("levelOfSupportPercentage", claimSummaryDetail.getLevelOfSupportPercentage());
		criteriaUpdate.set("description", claimSummaryDetail.getDescription());
		criteriaUpdate.set("deliveryDate", claimSummaryDetail.getDeliveryDate());
		criteriaUpdate.set("expenseCaped", claimSummaryDetail.getExpenseCaped());
		criteriaUpdate.set("descriptionOfEquipment", claimSummaryDetail.getDescriptionOfEquipment());
		criteriaUpdate.set("unitPrice", claimSummaryDetail.getUnitPrice());
		criteriaUpdate.set("updateUser", claimSummaryDetail.getUpdateUser());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("claimDetailsId"), claimSummaryDetail.getClaimDetailsId()));		 		
		session.createQuery(criteriaUpdate).executeUpdate();	
	}

	@Override
	public BigDecimal getOverheadPercentage(Integer claimId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<BigDecimal> criteria = builder.createQuery(BigDecimal.class);
			Root<Claim> root = criteria.from(Claim.class);
			Predicate predicateClaimId = builder.equal(root.get("claimId"),claimId);
			criteria.select(root.get("overHeadPercentage"));
			criteria.where(builder.and(predicateClaimId));
			BigDecimal overHead = session.createQuery(criteria).getSingleResult();
			return overHead == null ? BigDecimal.ZERO : overHead;
		} catch (Exception e) {
			return BigDecimal.ZERO;
		}
	}

	@Override
	public void saveOrUpdateClaimOverHead(Claim claim) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<Claim> criteriaUpdate = cb.createCriteriaUpdate(Claim.class);
		Root<Claim> root = criteriaUpdate.from(Claim.class);
		criteriaUpdate.set("overHeadPercentage", claim.getOverHeadPercentage());
		criteriaUpdate.set("updateUser", claim.getUpdateUser());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("claimId"), claim.getClaimId()));		 		
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ClaimSummaryDetails> getPrevExcludedClaimSummaryDetails(String awardNumber, String internalOrderCode,
			Integer claimId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select t1 from ClaimSummaryDetails t1 where t1.claimId <> :claimId and t1.claimId <:claimId and t1.awardExpenseTransaction.internalOrderCode =:internalOrderCode and t1.awardExpenseTransaction.awardNumber =:awardNumber ");
		hqlQuery.append("and t1.isExcludedFlag = 'Y' and t1.claimDetailsId not in (select t2.prevExcludedSummaryDetId from ClaimSummaryDetails t2 where t2.prevExcludedSummaryDetId is not null)");
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("awardNumber",awardNumber);
		queryClaim.setParameter("internalOrderCode",internalOrderCode);
		queryClaim.setParameter("claimId",claimId);	
		return queryClaim.getResultList();
	}

	@Override
	public void saveOrUpdateClaimAttachment(ClaimAttachment claimAttachment) {
		hibernateTemplate.saveOrUpdate(claimAttachment);	
	}

	@Override
	public Integer generateDocumentId() {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
			Root<ClaimAttachment> root = query.from(ClaimAttachment.class);
			query.select(builder.max(root.get("documentId")));
			Integer documentId =  session.createQuery(query).getSingleResult();
			return documentId == null ? 0 : documentId;
		}catch (Exception e) {
			return 0;
		}	
	}

	@Override
	public void deleteProtocolAttachment(ClaimAttachment claimAttachment) {
		hibernateTemplate.delete(claimAttachment);	
	}

	@Override
	public void archiveOldAttachmentVersion(Integer documentId, Integer claimId, String updateUser, Integer versionNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ClaimAttachment> criteriaUpdate = cb.createCriteriaUpdate(ClaimAttachment.class);
		Root<ClaimAttachment> root = criteriaUpdate.from(ClaimAttachment.class);
		criteriaUpdate.set("documentStatusCode", "2");
		criteriaUpdate.set("updateUser", updateUser);
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		Predicate predicateClaim = cb.equal(root.get("claimId"), claimId);
		Predicate predicateDocument = cb.equal(root.get("documentId"), documentId);
		Predicate predicateVersion = cb.equal(root.get("versionNumber") ,versionNumber);
		criteriaUpdate.where(cb.and(predicateClaim, predicateDocument, predicateVersion)); 		
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public List<ClaimAttachment> loadClaimAttachments(Integer claimId, Boolean isPersonHasPermission) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimAttachment> criteria = builder.createQuery(ClaimAttachment.class);
			Root<ClaimAttachment> root = criteria.from(ClaimAttachment.class);				
			Predicate predicateClaim = builder.equal(root.get("claimId"),claimId);
			Predicate predicateStatusCode = builder.equal(root.get("documentStatusCode"),"1");
			if (Boolean.TRUE.equals(isPersonHasPermission)) {
				criteria.where(builder.and(predicateClaim, predicateStatusCode));
			} else {
				Predicate predicateIsPrivate = builder.notEqual(root.get("attachmentType").get("isPrivate"), true);
				criteria.where(builder.and(predicateClaim, predicateStatusCode, predicateIsPrivate));
			}
			criteria.orderBy(builder.desc(root.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<ClaimAttachment> loadClaimAttachmentVersions(Integer claimId, Integer documentId, Boolean isPersonHasPermission) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimAttachment> criteria = builder.createQuery(ClaimAttachment.class);
			Root<ClaimAttachment> root = criteria.from(ClaimAttachment.class);				
			Predicate predicateClaim = builder.equal(root.get("claimId"),claimId);
			Predicate predicateStatusCode = builder.equal(root.get("documentStatusCode"),"2");
			Predicate predicateDocument = builder.equal(root.get("documentId"),documentId);
			if (Boolean.TRUE.equals(isPersonHasPermission)) {
				criteria.where(builder.and(predicateClaim, predicateStatusCode, predicateDocument));
			} else {
				Predicate predicateIsPrivate = builder.notEqual(root.get("attachmentType").get("isPrivate"), true);
				criteria.where(builder.and(predicateClaim, predicateStatusCode, predicateDocument, predicateIsPrivate));
			}
			criteria.orderBy(builder.desc(root.get("updateTimeStamp")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<ClaimAttachmentType> loadClaimAttachmentTypes() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimAttachmentType> criteria = builder.createQuery(ClaimAttachmentType.class);
			Root<ClaimAttachmentType> root = criteria.from(ClaimAttachmentType.class);				
			Predicate predicateActive = builder.equal(root.get("isActive"),true);
			criteria.where(builder.and(predicateActive));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void updateClaimSummaryExcludeFlag(ClaimSummaryDetails claimSummaryDetail) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ClaimSummaryDetails> criteriaUpdate = cb.createCriteriaUpdate(ClaimSummaryDetails.class);
		Root<ClaimSummaryDetails> root = criteriaUpdate.from(ClaimSummaryDetails.class);
		criteriaUpdate.set("isExcludedFlag", claimSummaryDetail.getIsExcludedFlag());
		criteriaUpdate.set("updateUser", claimSummaryDetail.getUpdateUser());
		criteriaUpdate.set("updateTimeStamp", commonDao.getCurrentTimestamp());
		Predicate predicateClaim = cb.equal(root.get("claimDetailsId"), claimSummaryDetail.getClaimDetailsId());
		criteriaUpdate.where(cb.and(predicateClaim)); 		
		session.createQuery(criteriaUpdate).executeUpdate();				
	}

	@Override
	public ClaimAttachment getClaimForeCastAttachmentDetail(Integer claimId) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<ClaimAttachment> criteria = builder.createQuery(ClaimAttachment.class);
				Root<ClaimAttachment> root = criteria.from(ClaimAttachment.class);				
				Predicate predicateClaim = builder.equal(root.get("claimId"),claimId);
				Predicate predicateStatus = builder.equal(root.get("documentStatusCode"),"1");
				Predicate predicateType = builder.equal(root.get("typeCode"),"1");
				criteria.where(builder.and(predicateClaim, predicateStatus, predicateType));
				return session.createQuery(criteria).getSingleResult();
			});
		} catch (Exception e) {
			return null;
		}		
	}

	@Override
	public ClaimAttachment getClaimAttachmentById(Integer attachmentId) {		
		return hibernateTemplate.get(ClaimAttachment.class, attachmentId);
	}

	@Override
	public ClaimSummaryDetails getClaimSummaryDetailById(Integer claimDetailsId) {
		return hibernateTemplate.get(ClaimSummaryDetails.class, claimDetailsId);
	}

	@Override
	public void saveOrUpdateClaimBreakDown(ClaimSummaryDetails claimSummaryDetails) {
		hibernateTemplate.saveOrUpdate(claimSummaryDetails);		
	}

	@Override
	public ClaimSummaryDetails getClaimPrevSummaryDetailById(Integer claimDetailsId) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<ClaimSummaryDetails> criteria = builder.createQuery(ClaimSummaryDetails.class);
				Root<ClaimSummaryDetails> protocolRoot = criteria.from(ClaimSummaryDetails.class);				
				Predicate predicateprevAdjusted = builder.equal(protocolRoot.get("prevAdjustedSummaryDetId"),claimDetailsId);
				criteria.where(builder.and(predicateprevAdjusted));
				return session.createQuery(criteria).getSingleResult();
			});
		} catch (Exception e) {
			return null;
		}		
	}

	@Override
	public void deleteSummaryDetailByPrevSummayId(Integer claimDetailsId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaDelete<ClaimSummaryDetails> criteriaDelete = cb.createCriteriaDelete(ClaimSummaryDetails.class);
		Root<ClaimSummaryDetails> root = criteriaDelete.from(ClaimSummaryDetails.class);
		criteriaDelete.where(cb.equal(root.get("prevAdjustedSummaryDetId"), claimDetailsId));
		session.createQuery(criteriaDelete).executeUpdate();	
	}
	
	@Override
	public BigDecimal getCumExpenseUptoPrevClaimPeriod(Integer claimId, String awardNumber, String budgetCategoryCode){
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select sum(t1.amountInFmacurrency) from AwardExpenseTransaction t1 where t1.awardNumber =:awardNumber and t1.actualOrCommittedFlag = 'A' and substr(t1.internalOrderCode,16,3) =:budgetCategoryCode and t1.fmPostingDate < ");
		hqlQuery.append("(select t2.endDate from Claim t2 where t2.awardNumber = t1.awardNumber and t2.claimId in (select max(t3.claimId) from Claim t3 where t3.awardNumber = t1.awardNumber and t3.claimId <> :claimId))");
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("claimId",claimId);
		queryClaim.setParameter("awardNumber",awardNumber);
		queryClaim.setParameter("budgetCategoryCode",budgetCategoryCode);
		BigDecimal amount =  (BigDecimal) queryClaim.getSingleResult();
		return amount == null ? BigDecimal.ZERO : amount;
	}

	@Override
	public BigDecimal getExpenseIncuredToThisPeriod(String awardNumber, Date startDate, Date endDate, String budgetCategoryCode) {
		String hqlQuery = "select sum(t1.amountInFmacurrency) from AwardExpenseTransaction t1 where	t1.awardNumber =:awardNumber and t1.actualOrCommittedFlag = 'A' and t1.fmPostingDate between :startDate and :endDate and substr(t1.internalOrderCode,16,3) =:budgetCategoryCode ";
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession()
				.createQuery(hqlQuery);
		queryClaim.setParameter("awardNumber",awardNumber);
		queryClaim.setParameter("startDate",startDate);
		queryClaim.setParameter("endDate",endDate);		
		queryClaim.setParameter("budgetCategoryCode",budgetCategoryCode);
		BigDecimal amount =  (BigDecimal) queryClaim.getSingleResult();
		return amount == null ? BigDecimal.ZERO : amount;
	}

	@Override
	public BigDecimal getCommitmentsAmtUptoPeriod(String awardNumber, Date endDate, String budgetCategoryCode, Date startDate) {
		String hqlQuery = new StringBuilder("select sum(amountInFmacurrency) from AwardExpenseTransaction WHERE awardNumber =:awardNumber  ").
				append(" and actualOrCommittedFlag = 'C' and fmPostingDate <= :endDate and substr(internalOrderCode,16,3) =:budgetCategoryCode and awardExpenseTransactionId ").
				append(" in (select awardExpenseTransactionId from AwardExpenseTransaction where (documentNumber,itemNumber,referenceDocumentCategory,referenceOrgUnit,acctAssignmentNumber,scheduleLineNumber,conditionCounter,").
				append(" referenceProcedure,documentNumberFMLineItem,transactionNumber) in (select documentNumber,itemNumber,referenceDocumentCategory,referenceOrgUnit,acctAssignmentNumber,").
				append(" scheduleLineNumber,conditionCounter,referenceProcedure,documentNumberFMLineItem,transactionNumber from ExpenseZeroExcludeCommittedV)) ").toString();
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("endDate", endDate);
		query.setParameter("budgetCategoryCode",budgetCategoryCode);
		BigDecimal amount =  (BigDecimal) query.getSingleResult();
		return amount == null ? BigDecimal.ZERO : amount;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardManpowerResource> loadAwardManpowerResource(String awardNumber, Integer sequenceNumber) {
		String hqlQuery = "select t1 from AwardManpowerResource t1 inner join AwardManpower t2 on t2.awardManpowerId = t1.awardManpowerId where t2.awardNumber =:awardNumber and t2.sequenceNumber =:sequenceNumber and t1.personId is not null and t1.personId not in ('999999999100') and t2.manpowerTypeCode not in (3) ";
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);
		queryClaim.setParameter("awardNumber",awardNumber);
		queryClaim.setParameter("sequenceNumber",sequenceNumber);
		return queryClaim.getResultList();
	}

	@Override
	public Integer getClaimSummaryIdByParams(Integer claimId, String budgetCategoryCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<ClaimSummary> root = query.from(ClaimSummary.class);
		Predicate predicateClaimId = builder.equal(root.get("claimId"),claimId);
		Predicate predicateCategory = builder.equal(root.get("budgetCategoryCode"),budgetCategoryCode);
		query.select(root.get("claimSummaryId"));
		query.where(builder.and(predicateClaimId, predicateCategory));
		query.orderBy(builder.desc(root.get("claimSummaryId")));
		return session.createQuery(query).getResultList().get(0);
	}

	@Override
	public List<Object> getLetterTemplateTypeCodeToExportClaim(Integer awardId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select t4.letterTemplateTypeCode from Award t1 left outer join GrantCall t2 on t2.grantCallId = t1.grantHeaderId left outer join SponsorFundingScheme t3 ");
			hqlQuery.append("on t3.fundingSchemeId = t2.fundingSchemeId left outer join ClaimFundingScheme t4 on t4.fundingSchemeCode = t3.fundingSchemeCode where t1.awardId =:awardId");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("awardId",awardId);
			return queryClaim.getResultList();
		} catch (Exception e) {
			return Collections.EMPTY_LIST;
		}		
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getManpowerdetails(Set<String> internalOrderCode) {
		if(internalOrderCode.isEmpty())
			return new ArrayList<Object[]>();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select distinct(t2.fullName), t1.budgetReferenceNumber, t6.quantity, t1.awardManpowerId, t2.personId, t3.glAccountCode  from AwardManpower t1 ");
		hqlQuery.append("inner join AwardManpowerResource t2 on t2.awardManpowerId = t1.awardManpowerId left join AwardManpowerPayroll t3 on t3.employeeNumber = t2.personId ");
		hqlQuery.append("inner join AwardBudgetHeader t4 on t4.awardId = t1.awardId and t4.versionNumber = ");
		hqlQuery.append("(select max(t5.versionNumber) from AwardBudgetHeader t5 where t5.awardId = t1.awardId) ");
		hqlQuery.append("inner join AwardBudgetDetail t6 on t6.budgetId = t4.budgetId and t6.internalOrderCode = t1.budgetReferenceNumber ");
		hqlQuery.append(" where t1.budgetReferenceNumber in :internalOrderCode and t1.awardId in (select t4.awardId from Award t4 where t4.awardNumber = t1.awardNumber and t4.awardSequenceStatus = 'ACTIVE') ");
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("internalOrderCode", internalOrderCode);
		return queryClaim.getResultList();
	}

	@Override
	public List<Claim> loadSubmittedClaimsForAward(String awardNumber) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Claim> criteria = builder.createQuery(Claim.class);
			Root<Claim> claimRoot = criteria.from(Claim.class);	
			criteria.multiselect(claimRoot.get("claimId"),claimRoot.get("claimNumber"),claimRoot.get("awardId")
					,claimRoot.get("awardNumber"),claimRoot.get("startDate")
					,claimRoot.get("endDate"),claimRoot.get("claimSubmissionDate")
					,claimRoot.get("claimStatus"),claimRoot.get("totalAmount"));
			criteria.where(builder.equal(claimRoot.get("awardNumber"),awardNumber));
			criteria.orderBy(builder.desc(claimRoot.get("claimSubmissionDate")));
			return session.createQuery(criteria).getResultList();
		});	
	}

	@Override
	public List<AwardManpower> getAllAwardManpower(Set<Integer> awardmanpowerIds) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardManpower> query = builder.createQuery(AwardManpower.class);
			Root<AwardManpower> rootAwardManpower = query.from(AwardManpower.class);
			query.where(rootAwardManpower.get("awardManpowerId").in(awardmanpowerIds));
			return session.createQuery(query).getResultList();
		});			
	}

	@Override
	public List<Manpower> getAllManpower(Set<String> personIds) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Manpower> query = builder.createQuery(Manpower.class);
			Root<Manpower> rootAwardManpower = query.from(Manpower.class);
			query.where(rootAwardManpower.get("manpowerPersonId").in(personIds));
			return session.createQuery(query).getResultList();
		});			
	}

	@Override
	public ClaimManpower saveOrUpdateClaimManpower(ClaimManpower claimManpower) {
		hibernateTemplate.saveOrUpdate(claimManpower);
		return claimManpower;
	}

	@Override
	public List<ClaimManpower> getAllClaimManpower(Set<Integer> manpowerResourceIds) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimManpower> query = builder.createQuery(ClaimManpower.class);
			Root<ClaimManpower> rootAwardManpower = query.from(ClaimManpower.class);
			query.where(rootAwardManpower.get("awardManpowerResourceId").in(manpowerResourceIds));
			return session.createQuery(query).getResultList();
		});			
	}

	@Override
	public Date getLastClaimEndDate(String awardNumber, Integer claimId) {
		try {
			String hqlQuery = "select t1.endDate from Claim t1 where t1.awardNumber =:awardNumber and t1.claimId in (select max(t2.claimId) from Claim t2 where claimId <>:claimId and claimId <:claimId and t2.awardNumber =:awardNumber) ";
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);
			queryClaim.setParameter("awardNumber",awardNumber);
			queryClaim.setParameter("claimId",claimId);
			return (Date) queryClaim.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getManpowerdetailsByPayrollId(Set<Integer> payrollId) {
		if(payrollId.isEmpty())
			return new ArrayList<Object[]>();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select distinct(t2.fullName), t1.budgetReferenceNumber, t6.quantity, t1.awardManpowerId, t2.personId, t3.glAccountCode, t3.payrollId, t3.payElement, ");
		hqlQuery.append("t2.chargeStartDate, t2.chargeEndDate, t2.planStartDate, t2.planEndDate,t2.manpowerPlanJobProfileType.defaultJobTitle from AwardManpower t1  inner join AwardManpowerResource t2 on t2.awardManpowerId = t1.awardManpowerId inner join AwardManpowerPayroll t3 on ");
		hqlQuery.append("t3.employeeNumber = t2.personId and t3.internalOrderCode = t1.budgetReferenceNumber inner join AwardBudgetHeader t4 on t4.awardId = t1.awardId and t4.versionNumber = ");
		hqlQuery.append("(select max(t5.versionNumber) from AwardBudgetHeader t5 where t5.awardId = t1.awardId) ");
		hqlQuery.append("inner join AwardBudgetDetail t6 on t6.budgetId = t4.budgetId and t6.internalOrderCode = t1.budgetReferenceNumber ");
		hqlQuery.append("where t3.payrollId in :payrollId  and t1.awardId = (select awardId from Award where awardNumber = t1.awardNumber and awardSequenceStatus = 'ACTIVE') ");
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("payrollId", payrollId);
		return queryClaim.getResultList();
	}

	@Override
	public BigDecimal getClaimSummaryAmountReq(Integer claimId, String budgetCategoryCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ClaimSummaryDetails> query = builder.createQuery(ClaimSummaryDetails.class);
		Root<ClaimSummaryDetails> root = query.from(ClaimSummaryDetails.class);
		Predicate predicateClaimId = builder.equal(root.get("claimId"),claimId);
		Predicate predicateCategory = builder.equal(root.get("budgetCategoryCode"),budgetCategoryCode);
		query.where(builder.and(predicateClaimId, predicateCategory));
		List<ClaimSummaryDetails> encryptedAmount =  session.createQuery(query).getResultList();
		List<BigDecimal> amounts = new ArrayList<>();
		encryptedAmount.forEach(amount -> {
			if (amount.getEncryptedAmount() != null) {
				try {
					amounts.add(new BigDecimal(excelityService.decryptAESData(amount.getEncryptedAmount())));
				} catch (Exception e) {
					e.printStackTrace();
					logger.error("error occured while getting amount for EOM/RSS", e);
				}
			} else {
				amounts.add(amount.getAdjustedTotal());
			}
			
		});
		if (amounts == null || amounts.isEmpty())
			return null;
		return amounts.stream().filter(amount -> amount != null).reduce(BigDecimal.ZERO, BigDecimal::add);
	}

	@Override
	public BigDecimal getClaimSummaryAmountReqForAll(Integer claimId, String budgetCategoryCode) {
		if (!budgetCategoryCode.equals("EOM") && !budgetCategoryCode.equals("RSS")) {
			try {
				String hqlQuery = "select sum(adjustedTotal) from ClaimSummaryDetails where claimId =:claimId and  budgetCategoryCode =:budgetCategoryCode and isExcludedFlag = 'N' ";			
				Query queryClaim =  hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
				queryClaim.setParameter("budgetCategoryCode",budgetCategoryCode);
				queryClaim.setParameter("claimId",claimId);
				BigDecimal result = (BigDecimal) queryClaim.getSingleResult();
				return result == null ? BigDecimal.ZERO : result;
			} catch (Exception e2) {
				logger.error("error occured while updating amount for other budgetcategory", e2);
			}
		} else {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimSummaryDetails> query = builder.createQuery(ClaimSummaryDetails.class);
			Root<ClaimSummaryDetails> root = query.from(ClaimSummaryDetails.class);
			Predicate predicateClaimId = builder.equal(root.get("claimId"),claimId);
			Predicate predicateCategory = builder.equal(root.get("budgetCategoryCode"),budgetCategoryCode);
			query.where(builder.and(predicateClaimId, predicateCategory));
			List<ClaimSummaryDetails> summaryDetail =  session.createQuery(query).getResultList();
			List<BigDecimal> amount = new ArrayList<>();
			summaryDetail.stream().filter(summary -> !(summary.getIsExcludedFlag() != null && summary.getIsExcludedFlag().equals(Boolean.TRUE))).forEach(summary -> {
				if (summary.getAdjustedTotal() != null) {
					amount.add(summary.getAdjustedTotal());
				} else {
					try {
						amount.add(new BigDecimal(excelityService.decryptAESData(summary.getEncryptedAmount())));
					} catch (Exception e) {
						logger.error("error occured while getting amount for EOM/RSS", e);
					}
				}
			});
			BigDecimal result = amount.stream().reduce(BigDecimal.ZERO, BigDecimal::add);
			return result;
			
		}
		return null;		
	}

	@Override
	public void getBudgetDetails(ClaimSummaryDetails summary) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select t1.lineItemDescription, t1.lineItemCost, t2.updateTimeStamp from AwardBudgetHeader t2 ");
		hqlQuery.append("inner join AwardBudgetDetail t1 on t1.budgetId = t2.budgetId ");
		hqlQuery.append("where t1.internalOrderCode =:internalOrderCode and t1.versionNumber in (select max(t3.versionNumber) from AwardBudgetDetail t3 where t3.internalOrderCode =:internalOrderCode) ");
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("internalOrderCode", summary.getAwardExpenseTransaction().getInternalOrderCode());
		@SuppressWarnings("unchecked")
		List<Object[]> result =  queryClaim.getResultList();
		result.forEach(budget ->{
			Object lineItemDescription = budget[0];
			Object lineItemCost = budget[1];
			Object dateOfBudgetApproval = budget[2];
			summary.setOrginalLineItemDescription(lineItemDescription == null ? null : lineItemDescription.toString());
			summary.setApprovedBudget(lineItemCost == null ? BigDecimal.ZERO : new BigDecimal(lineItemCost.toString()));
			try {
				summary.setBudgetApprovalDate(dateOfBudgetApproval == null ? null : new SimpleDateFormat("yyyy/MM/dd").parse(dateOfBudgetApproval.toString().substring(0, 10).replace("-", "/")));
			} catch (ParseException e) {
				logger.error("Error in getBudgetDetails date parsing {}", e.getMessage());
			}
			summary.setDescriptionOfExpenditure(summary.getDescriptionOfExpenditure() == null ? summary.getAwardExpenseTransaction().getRemarks() : summary.getDescriptionOfExpenditure());
		});
	}

	@Override
	public void deleteSummaryDetailForPrevClaimExcluded(Integer prevClaimDetailsId, Integer currentClaimDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaDelete<ClaimSummaryDetails> criteriaDelete = cb.createCriteriaDelete(ClaimSummaryDetails.class);
		Root<ClaimSummaryDetails> root = criteriaDelete.from(ClaimSummaryDetails.class);
		Predicate predicatePrevClaimId = cb.equal(root.get("prevExcludedSummaryDetId"), prevClaimDetailsId);
		Predicate predicateCurrentClaimId = cb.equal(root.get("claimDetailsId"), currentClaimDetailId);
		criteriaDelete.where(cb.and(predicatePrevClaimId, predicateCurrentClaimId));
		session.createQuery(criteriaDelete).executeUpdate();		
	}

	@Override
	public void updateClaimSummaryTransactions(Claim claim) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String procedureName = "INSERT_CLAIM_SUMMARY_DETAILS";
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call INSERT_CLAIM_SUMMARY_DETAILS(?,?,?,?,?,?,?,?)}");
				statement.setString(1, claim.getAwardId().toString());
				statement.setInt(2, claim.getClaimId());
				statement.setString(3, claim.getClaimNumber());
				statement.setString(4, claim.getAward().getAccountNumber());
				statement.setString(5, claim.getAwardNumber());
				statement.setString(6, claim.getUpdateUser());
				statement.setDate(7, new java.sql.Date(claim.getStartDate().getTime()));
				statement.setDate(8, new java.sql.Date(claim.getEndDate().getTime()));
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, claim.getAwardId().toString());
				statement.setInt(3, claim.getClaimId());
				statement.setString(4, claim.getClaimNumber());
				statement.setString(5, claim.getAward().getAccountNumber());
				statement.setString(6, claim.getAwardNumber());
				statement.setString(7, claim.getUpdateUser());
				statement.setDate(8, new java.sql.Date(claim.getStartDate().getTime()));
				statement.setDate(9, new java.sql.Date(claim.getEndDate().getTime()));
				statement.execute();
				statement.getObject(1);
			}				
		}catch (Exception e) {
			logger.error("Error in updateClaimSummaryTransactions {}", e.getMessage());
		}
	}

	@Override
	public ClaimFundingScheme getClaimFundingScheme(Integer awardId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select t4 from Award t1 left outer join GrantCall t2 on t2.grantCallId = t1.grantHeaderId left outer join SponsorFundingScheme t3 ");
			hqlQuery.append("on t3.fundingSchemeId = t2.fundingSchemeId left outer join ClaimFundingScheme t4 on t4.fundingSchemeCode = t3.fundingSchemeCode where t1.awardId =:awardId");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("awardId",awardId);
			List<Object> fundingSchemeObjs = queryClaim.getResultList();
			if (fundingSchemeObjs != null && !fundingSchemeObjs.isEmpty()) {
				return (ClaimFundingScheme) fundingSchemeObjs.get(0);
			}
		} catch (Exception e) {
			logger.error("Exception in getClaimFundingScheme {}", e.getMessage());
		}
		return null;
	}

	@Override
	public BigDecimal getAdjustedIndirectCost(Integer claimId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<BigDecimal> criteria = builder.createQuery(BigDecimal.class);
			Root<Claim> root = criteria.from(Claim.class);
			Predicate predicateClaimId = builder.equal(root.get("claimId"),claimId);
			criteria.select(root.get("adjustedIndirectCost"));
			criteria.where(builder.and(predicateClaimId));
			BigDecimal adjustedCost = session.createQuery(criteria).getSingleResult();
			return adjustedCost == null ? BigDecimal.ZERO : adjustedCost;
		} catch (Exception e) {
			return BigDecimal.ZERO;
		}
	}

	@Override
	public void updateAdjustedIndirectCost(BigDecimal adjustedIndirectCost, Integer claimId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaUpdate<Claim> criteriaUpdate = cb.createCriteriaUpdate(Claim.class);
			Root<Claim> root = criteriaUpdate.from(Claim.class);
			criteriaUpdate.set("adjustedIndirectCost", adjustedIndirectCost);
			criteriaUpdate.where(cb.equal(root.get("claimId"), claimId));
			session.createQuery(criteriaUpdate).executeUpdate();
		} catch (Exception e) {
			logger.error("Error in updateAdjustedIndirectCost {}", e.getMessage());
		}		
	}

	@Override
	public BigDecimal getTotalAmountRequestedForClaim(Integer claimId) {
		String hqlQuery = "select sum(amountRequested) from ClaimSummary where claimId =:claimId ";
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);			
		queryClaim.setParameter("claimId",claimId);
		BigDecimal totalAmountReq = (BigDecimal) queryClaim.getSingleResult();
		return totalAmountReq == null ? BigDecimal.ZERO : totalAmountReq;		
	}

	@Override
	public List<Integer> getClaimSummaryDetailExpenseTransactionIds(Integer claimId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> criteria = builder.createQuery(Integer.class);
			Root<ClaimSummaryDetails> root = criteria.from(ClaimSummaryDetails.class);				
			criteria.select(root.get("awardExpenseTransId"));
			criteria.where(builder.equal(root.get("claimId"),claimId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public ClaimSummary getPrevClaimSummaryOfBudgetCategory(Integer claimId, String awardNumber, String budgetCategoryCode) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("from ClaimSummary t1 where t1.claimSummaryId in (  ");
			hqlQuery.append(" select max(t2.claimSummaryId) from ClaimSummary t2 where t2.claim.awardNumber =:awardNumber and t2.claimId <>:claimId and t2.claimId <:claimId and t2.budgetCategoryCode =:budgetCategoryCode)");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("awardNumber",awardNumber);
			queryClaim.setParameter("claimId",claimId);
			queryClaim.setParameter("budgetCategoryCode",budgetCategoryCode);
			queryClaim.setHint(QueryHints.READ_ONLY, true);
			return (ClaimSummary) queryClaim.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public ClaimSummary getClaimSummaryByParams(Integer claimId, String budgetCategoryCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ClaimSummary> criteria = builder.createQuery(ClaimSummary.class);
		Root<ClaimSummary> root = criteria.from(ClaimSummary.class);				
		Predicate predicateClaimId = builder.equal(root.get("claimId"),claimId);
		Predicate predicateCategory = builder.equal(root.get("budgetCategoryCode"), budgetCategoryCode);
		criteria.where(builder.and(predicateClaimId, predicateCategory));
		return session.createQuery(criteria).getSingleResult();		
	}

	@Override
	public void saveOrUpdateClaimSummary(ClaimSummary summary) {
		hibernateTemplate.saveOrUpdate(summary);		
	}
	
	@Override
	public List<Claim> getClaimsWithoutDuration() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Claim> criteria = builder.createQuery(Claim.class);
			Root<Claim> root = criteria.from(Claim.class);				
			criteria.where(builder.isNull(root.get("duration")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public BigDecimal getAdjustedIndirectCostInPrevClaims(Integer claimId, String awardNumber) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select sum(ifnull(t1.adjustedIndirectCost,0.00) + ifnull(t1.migratedReimPrevIdcAmt,0.00)) from Claim t1 where t1.claimId <>:claimId and t1.awardNumber =:awardNumber and t1.claimId <:claimId ");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("awardNumber",awardNumber);
			queryClaim.setParameter("claimId",claimId);
			return new BigDecimal(queryClaim.getSingleResult().toString());
		} catch (Exception e) {
			return BigDecimal.ZERO;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> loadClaimsForAward(String awardNumber) {	
		try {
			return hibernateTemplate.getSessionFactory().getCurrentSession().createStoredProcedureCall("GET_AWARD_CLAIM_DETAILS").
					registerStoredProcedureParameter("AV_AWARD_NUMBER", String.class, ParameterMode.IN).setParameter("AV_AWARD_NUMBER", awardNumber)
					.getResultList();
		} catch (Exception e) {
			logger.error("error occured while loading award claims", e);
			return new ArrayList<>();
		}		
	}

	@Override
	public Integer getPrevClaimId(Integer claimId, String awardNumber) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select t1.claimId from Claim t1 where t1.claimId in (  ");
			hqlQuery.append(" select max(t2.claimId) from Claim t2 where t2.awardNumber =:awardNumber and t2.claimId <>:claimId and t2.claimId <:claimId)");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("awardNumber",awardNumber);
			queryClaim.setParameter("claimId",claimId);
			return (Integer) queryClaim.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public BigDecimal totalAmountReqForNextPeriod(Integer prevClaimId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select ifnull((t2.IDC_AMOUNT_FORCASTED+t2.IDC_COMMITMENT_UPTO_PREV_CLAIM),0.00)-(ifnull(t2.IDC_CUM_CLAIM_AMT_UPTO_CLAIM,0.00)-ifnull((sum(t1.CUM_EXPENSE_UPTO_PREV_CLAIM+t1.AMOUNT_REQUESTED)*t2.OVERHEAD_PERCENTAGE/100),0.00)) ");
					hqlQuery.append(" from claim_summary t1 inner join claim t2 on t2.claim_id = t1.claim_id where t1.claim_id =:claimId");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery(hqlQuery.toString());
			queryClaim.setParameter("claimId",prevClaimId);
			Object result = queryClaim.getSingleResult();
			return result != null ? new BigDecimal(result.toString()) : BigDecimal.ZERO;
		} catch (Exception e) {
			logger.error("Error in totalAmountReqForNextPeriod {}", e);
			return BigDecimal.ZERO;
		}
	}

	@Override
	public BigDecimal getIdcCumClaimAmtUptoClaim(Integer claimId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select t1.idcCumClaimAmtUptoClaim from Claim t1 where t1.claimId =:claimId");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("claimId",claimId);
			return (BigDecimal) queryClaim.getSingleResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public void saveOrUpdateClaimInvoiceDetail(ClaimInvoiceDetails claimInvoiceDetail) {
		hibernateTemplate.saveOrUpdate(claimInvoiceDetail);
	}

	@Override
	public ClaimInvoice loadClaimInvoice(Integer claimId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("from ClaimInvoice where claimId =:claimId and sequenceNumber in (select max(sequenceNumber) from ClaimInvoice where claimId =:claimId )");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("claimId",claimId);
			return (ClaimInvoice) queryClaim.getSingleResult();
		} catch (Exception e) {
			return null;
		}		
	}

	@Override
	public List<ClaimGlAccount> loadGlAccountCodes() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimGlAccount> criteria = builder.createQuery(ClaimGlAccount.class);
			Root<ClaimGlAccount> root = criteria.from(ClaimGlAccount.class);				
			criteria.where(builder.equal(root.get("isActive"),true));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void saveOrUpdateClaimInvoice(ClaimInvoice claimInvoice) {
		hibernateTemplate.saveOrUpdate(claimInvoice);		
	}

	@Override
	public List<ClaimInvoiceMetadata> getClaimInvoiceMetadata() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimInvoiceMetadata> criteria = builder.createQuery(ClaimInvoiceMetadata.class);
			Root<ClaimInvoiceMetadata> root = criteria.from(ClaimInvoiceMetadata.class);				
			criteria.where(builder.equal(root.get("isActive"),true));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public CustomDataView getAwardCustomData(Integer awardId) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<CustomDataView> criteria = builder.createQuery(CustomDataView.class);
				Root<CustomDataView> root = criteria.from(CustomDataView.class);				
				criteria.where(builder.equal(root.get("moduleItemKey"),awardId));
				return session.createQuery(criteria).getSingleResult();
			});
		} catch (Exception e) {
			return null;
		}			
	}

	@Override
	public String getCampusForUnit(String leadUnitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = null;
		try {
			String functionName = "FN_CAMPUS_FOR_UNIT";
			String functionCall = "{ ? = call " + functionName + "(?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.VARCHAR);
			statement.setString(2, leadUnitNumber);
			statement.execute();
			result = statement.getString(1);
		} catch (SQLException e) {
			logger.error("Exception in getCampusForUnit : {}", e.getMessage());
		}
		return result;
	}

	@Override
	public String getBACodeForUnit(String leadUnitNumber) {
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Query query = session.createSQLQuery("SELECT BA_CODE FROM SAP_FEED_UNIT_MAPPING WHERE UNIT_NUMBER = :unitNumber");
		query.setParameter("unitNumber", leadUnitNumber);
		return query.getSingleResult().toString();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public ClaimInvoiceMetadata getClaimInvoiceMetadataByParams(String baCode, String documentTypeCode, Boolean isReversalType) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<ClaimInvoiceMetadata> criteria = builder.createQuery(ClaimInvoiceMetadata.class);
				Root<ClaimInvoiceMetadata> root = criteria.from(ClaimInvoiceMetadata.class);				
				Predicate predicateBaCode = builder.equal(root.get("baCode"),baCode);
				Predicate predicateDocType = null;
				if(isReversalType) {
					 predicateDocType = builder.equal(root.get("reversalDocumentTypeCode"), documentTypeCode);
				} else {
					predicateDocType = builder.equal(root.get("documentTypeCode"), documentTypeCode);
				}
				criteria.where(builder.and(predicateBaCode, predicateDocType));
				return session.createQuery(criteria).getSingleResult();
			});
		} catch (Exception e) {
			return null;
		}	
	}

	@Override
	public BigDecimal getClaimAmount(String awardNumber, Integer claimId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String procedureName = "GET_AWARD_CLAIM_AMOUNT";
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_AWARD_CLAIM_AMOUNT(?,?)}");
				statement.setString(1, awardNumber);
				statement.setInt(2, claimId);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, awardNumber);
				statement.setInt(3, claimId);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				return  resultSet.getBigDecimal("TOTAL_AMOUNT");
			}	
		}catch (Exception e) {
			logger.error("Error in getClaimAmountForClaim {}", e.getMessage());
		}
		return null;
	}

	@Override
	public ClaimOutputGstTaxCode getClaimOutputGstTaxCode(String outputGstCategory) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<ClaimOutputGstTaxCode> criteria = builder.createQuery(ClaimOutputGstTaxCode.class);
				Root<ClaimOutputGstTaxCode> root = criteria.from(ClaimOutputGstTaxCode.class);				
				criteria.where(builder.equal(root.get("outputGstCategory"), outputGstCategory));
				return session.createQuery(criteria).getSingleResult();
			});
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<ClaimOutputGstTaxCode> getClaimOutputGstTaxCodeInTaxCode(Set<String> taxCodes) {
		try {
			return hibernateTemplate.execute(session -> {
				CriteriaBuilder builder = session.getCriteriaBuilder();
				CriteriaQuery<ClaimOutputGstTaxCode> criteria = builder.createQuery(ClaimOutputGstTaxCode.class);
				Root<ClaimOutputGstTaxCode> root = criteria.from(ClaimOutputGstTaxCode.class);	
				criteria.where(root.get("taxCode").in(taxCodes));
				return session.createQuery(criteria).getResultList();
			});
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public void deleteClaimInvoiceDetail(Integer invoiceDetailId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<ClaimInvoiceDetails> criteria = builder.createCriteriaDelete(ClaimInvoiceDetails.class);
		Root<ClaimInvoiceDetails> root = criteria.from(ClaimInvoiceDetails.class);				
		criteria.where(builder.equal(root.get("invoiceDetailId"),invoiceDetailId));
		session.createQuery(criteria).executeUpdate();	
	}

	@Override
	public List<ClaimOutputGstTaxCode> loadClaimTaxCodes() {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimOutputGstTaxCode> criteria = builder.createQuery(ClaimOutputGstTaxCode.class);
			Root<ClaimOutputGstTaxCode> root = criteria.from(ClaimOutputGstTaxCode.class);				
			criteria.where(builder.equal(root.get("isActive"),true));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<ClaimInvoiceLog> loadAllClaimInvoice(Integer claimId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<ClaimInvoiceLog> criteria = builder.createQuery(ClaimInvoiceLog.class);
			Root<ClaimInvoiceLog> root = criteria.from(ClaimInvoiceLog.class);				
			criteria.where(builder.equal(root.get("claimId"),claimId));
			criteria.orderBy(builder.asc(root.get("invoiceId")));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public void saveOrUpdateSapClaimFeed(SapClaimFeed sapClaimFeed) {
		hibernateTemplate.saveOrUpdate(sapClaimFeed);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SapClaimFeedResponseMessage> loadClaimInvoiceSapResponse(Integer claimId, Integer sequenceNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select t1 from SapClaimFeedResponseMessage t1  ");
		hqlQuery.append("inner join ClaimInvoiceLog t2 on t1.claimInvoiceLogId = t2.claimInvoiceLogId where t2.claimId =:claimId and t2.sequenceNumber =:sequenceNumber order by t1.claimInvoiceLogId asc");
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("claimId",claimId);
		queryClaim.setParameter("sequenceNumber",sequenceNumber);
		return queryClaim.getResultList();
	}

	@Override
	public Boolean canPerformRevisionClaim(Integer claimId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append(" from SapClaimFeed t1 where t1.claimId =:claimId and t1.feedId = (select max(feedId) from SapClaimFeed where claimId = t1.claimId) ");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("claimId",claimId);
			SapClaimFeed sapClaimFeed = (SapClaimFeed) queryClaim.getSingleResult();
			return (sapClaimFeed.getFeedStatus().equals("R")) || (sapClaimFeed.getUserActionCode() != null && sapClaimFeed.getFeedStatus().equals("E") && sapClaimFeed.getUserActionCode().equals("11"))  ? true : false;
		} catch (Exception e) {
			logger.error("Error in createPerformRevisionClaim {}", e.getMessage());
			return false;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ClaimInvoiceDetails> getCliamInvoiceDetailByClaimId(Integer claimId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("select t2 from ClaimInvoice t1 inner join ClaimInvoiceDetails t2 ON t2.invoiceId = t1.invoiceId ");
		hqlQuery.append(" where t1.claimId =:claimId and t1.sequenceNumber in(select max(sequenceNumber) from ClaimInvoice where claimId = :claimId)");
		Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryClaim.setParameter("claimId", claimId);
		return queryClaim.getResultList();
	}

	@Override
	public List<Integer> getAllClaimsBasedOnParams(List<Integer> awardIds) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
			Root<Claim> rootClaim = query.from(Claim.class);
			Predicate predicateAwardId = rootClaim.get("awardId").in(awardIds);
			Predicate predicateStatus = builder.notEqual(rootClaim.get("claimStatusCode"), Constants.CLAIM_STATUS_CODE_COMPLETED);
			query.select(rootClaim.get("awardId"));
			query.where(builder.and(predicateAwardId, predicateStatus));
			return session.createQuery(query).getResultList();
		});			
	}

	@Override
	public void deleteClaimDetails(Integer claimId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			ProcedureCall procedure = session.createStoredProcedureCall("DELETE_CLAIM_RECORD");
			procedure.registerParameter(1, Integer.class, ParameterMode.IN).bindValue(claimId);
			procedure.execute();
		} catch (Exception e) {
			logger.error("error occured while deleteClaimDetails", e);
		}
	}

	@Override
	public Boolean checkIfPreviouslyExcluded(Integer claimSummaryDetailId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select count(*) from ClaimSummaryDetails t1 where t1.prevExcludedSummaryDetId =:claimSummaryDetailId ");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("claimSummaryDetailId",claimSummaryDetailId);
			Integer count = Integer.valueOf(queryClaim.getSingleResult().toString());
			return count > 0 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}
	
	@Override
	public Boolean checkIfPrevClaimIsInEditMode(Integer claimId, String awardNumber, List<String> claimStatus) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select count(*) from Claim t1 where t1.claimId in (  ");
			hqlQuery.append(" select max(t2.claimId) from Claim t2 where t2.awardNumber =:awardNumber and t2.claimId <>:claimId and t2.claimId <:claimId and claimStatusCode in (:claimStatus))");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("awardNumber",awardNumber);
			queryClaim.setParameter("claimId",claimId);
			queryClaim.setParameter("claimStatus", claimStatus);
			return Integer.parseInt(queryClaim.getSingleResult().toString()) != 0 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

	@Override
	public Boolean checkIfLastClaimByParams(Integer claimId, String awardNumber) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select count(*) from Claim t1 where t1.claimId =: claimId and t1.claimId < ( ");
			hqlQuery.append(" select max(t2.claimId) from Claim t2 where t2.awardNumber =:awardNumber)");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			queryClaim.setParameter("claimId",claimId);
			queryClaim.setParameter("awardNumber",awardNumber);
			return Integer.parseInt(queryClaim.getSingleResult().toString()) == 0 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

	@Override
	public BigDecimal getPreviousClaimsTotalAmountById(Integer claimId, String awardNumber, String budgetCategoryCode) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("SELECT (SUM(IFNULL(C2.AMOUNT_REQUESTED,0.00)+ ifnull(if(C1.create_user = 'quickstart',C2.PREV_CLAIM_TOTAL_AMOUNT,0.00),0.00))) AS PREV_CLAIM_TOTAL_AMOUNT ");
			hqlQuery.append(" FROM CLAIM C1 INNER JOIN CLAIM_SUMMARY C2 ON C1.CLAIM_ID = C2.CLAIM_ID"); 
			hqlQuery.append(" WHERE C1.AWARD_NUMBER = :awardNumber AND C1.CLAIM_ID <> :claimId AND C1.CLAIM_ID <> :claimId AND C1.CLAIM_ID < :claimId AND C2.BUDGET_CATEGORY_CODE = :budgetCategoryCode");
			Query queryClaim = hibernateTemplate.getSessionFactory().getCurrentSession().createNativeQuery(hqlQuery.toString());
			queryClaim.setParameter("awardNumber",awardNumber);
			queryClaim.setParameter("claimId",claimId);
			queryClaim.setParameter("budgetCategoryCode",budgetCategoryCode);
			return (BigDecimal) queryClaim.getSingleResult();
		} catch (Exception e) {
			logger.error("Exception in getPreviousClaimsTotalAmountById");
			return BigDecimal.ZERO;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getActualCountOfManpowerResourceBasedOnJobProfile(Set<Integer> awardManpowerResourceIds) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT T5.DEFAULT_JOB_TITLE AS DEFAULT_JOB_TITLE, COUNT(DISTINCT(T2.POSITION_ID)) as POSITION_ID FROM AWARD_MANPOWER T1");
		hqlQuery.append(" INNER JOIN AWARD_MANPOWER_RESOURCE T2 ON T2.AWARD_MANPOWER_ID = T1.AWARD_MANPOWER_ID AND T2.AWARD_MANPOWER_RESOURCE_ID IN(:manpowerResourceId)");
		hqlQuery.append(" AND T2.PERSON_ID IS NOT NULL  AND T2.PERSON_ID <> '999999999100' ");
		hqlQuery.append(" INNER JOIN MANPOWER_JOB_PROFILE_TYPE T5 ON T5.JOB_PROFILE_TYPE_CODE = T2.PLAN_JOB_PROFILE_TYPE_CODE GROUP BY T5.DEFAULT_JOB_TITLE");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createNativeQuery(hqlQuery.toString());
		query.setParameter("manpowerResourceId", awardManpowerResourceIds);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Object[]> getApprovedCountOfManpowerResourceBasedOnJobProfile(Set<Integer> awardManpowerIds) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT T6.DEFAULT_JOB_TITLE, SUM(DISTINCT(T5.QUANTITY)) FROM AWARD_MANPOWER T1 INNER JOIN AWARD_MANPOWER_RESOURCE T2 ON T2.AWARD_MANPOWER_ID = T1.AWARD_MANPOWER_ID AND T2.AWARD_MANPOWER_ID in(:awardManpowerId)");
		hqlQuery.append(" INNER JOIN AWARD_BUDGET_HEADER T3 ON T3.AWARD_ID = T1.AWARD_ID AND T3.VERSION_NUMBER =(SELECT MAX(T4.VERSION_NUMBER) FROM ");
		hqlQuery.append(" AWARD_BUDGET_HEADER T4 WHERE T4.AWARD_ID =T1.AWARD_ID) INNER JOIN AWARD_BUDGET_DETAIL T5 ON T5.BUDGET_HEADER_ID = T3.BUDGET_HEADER_ID AND T5.INTERNAL_ORDER_CODE = T1.BUDGET_REFERENCE_NUMBER");
		hqlQuery.append(" INNER JOIN MANPOWER_JOB_PROFILE_TYPE T6 ON T6.JOB_PROFILE_TYPE_CODE = T2.PLAN_JOB_PROFILE_TYPE_CODE GROUP BY T6.DEFAULT_JOB_TITLE");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createNativeQuery(hqlQuery.toString());
		query.setParameter("awardManpowerId", awardManpowerIds);
		return query.getResultList();
	}

	@Override
	public String getSponsorFundingSchemeByGrantId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT T1.schemeName FROM FundingScheme T1 WHERE T1.fundingSchemeCode =(SELECT T2.fundingSchemeCode FROM SponsorFundingScheme T2 WHERE T2.fundingSchemeId =(SELECT T3.fundingSchemeId FROM GrantCall T3 WHERE T3.grantCallId=:grantCallId))";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("grantCallId", grantCallId);
		return query.getSingleResult().toString();
	}
}
