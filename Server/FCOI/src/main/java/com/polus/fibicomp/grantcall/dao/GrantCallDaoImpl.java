package com.polus.fibicomp.grantcall.dao;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.grantcall.pojo.FundingSchemeAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallActionLog;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachType;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCallCriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibility;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibilityType;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIQuestionnaire;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKeyword;
import com.polus.fibicomp.grantcall.pojo.GrantCallRelevant;
import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallStatus;
import com.polus.fibicomp.grantcall.pojo.GrantCallType;
import com.polus.fibicomp.grantcall.pojo.GrantEligibiltyTargetType;
import com.polus.fibicomp.pojo.RelevantField;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorFundingScheme;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;

@Transactional
@Service(value = "grantCallDao")
public class GrantCallDaoImpl implements GrantCallDao {

	protected static Logger logger = LogManager.getLogger(GrantCallDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private DBEngine dbEngine;

	public static final String GRANT_CALL_ID = "grantCallId";

	@Override
	public List<GrantCallType> fetchAllGrantCallTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallType> query = builder.createQuery(GrantCallType.class);
		Root<GrantCallType> rootGrantCallType = query.from(GrantCallType.class);
		query.orderBy(builder.asc(rootGrantCallType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<GrantCallStatus> fetchAllGrantCallStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallStatus> query = builder.createQuery(GrantCallStatus.class);
		Root<GrantCallStatus> rootGrantCallStatus = query.from(GrantCallStatus.class);
		query.where(builder.notEqual(rootGrantCallStatus.get("grantStatusCode"), Constants.GRANT_CALL_STATUS_CODE_ARCHIVED));
		query.orderBy(builder.asc(rootGrantCallStatus.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ScienceKeyword> fetchAllScienceKeywords() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ScienceKeyword> query = builder.createQuery(ScienceKeyword.class);
		Root<ScienceKeyword> rootScienceKeyword = query.from(ScienceKeyword.class);
		query.orderBy(builder.asc(rootScienceKeyword.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SponsorType> fetchAllSponsorTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorType> query = builder.createQuery(SponsorType.class);
		Root<SponsorType> rootSponsorType = query.from(SponsorType.class);
		query.orderBy(builder.asc(rootSponsorType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<GrantCallCriteria> fetchAllGrantCallCriteria() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallCriteria> query = builder.createQuery(GrantCallCriteria.class);
		Root<GrantCallCriteria> rootGrantCallCriteria = query.from(GrantCallCriteria.class);
		query.select(builder.construct(GrantCallCriteria.class, rootGrantCallCriteria.get("grantCriteriaCode"),
				rootGrantCallCriteria.get(Constants.DESCRIPTION)));
		query.orderBy(builder.asc(rootGrantCallCriteria.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<GrantCallEligibilityType> fetchAllEligibilityTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallEligibilityType> query = builder.createQuery(GrantCallEligibilityType.class);
		Root<GrantCallEligibilityType> rootGrantCallEligibilityType = query.from(GrantCallEligibilityType.class);
		query.orderBy(builder.asc(rootGrantCallEligibilityType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<GrantCallAttachType> fetchAllGrantCallAttachTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallAttachType> query = builder.createQuery(GrantCallAttachType.class);
		Root<GrantCallAttachType> rootGrantCallAttachType = query.from(GrantCallAttachType.class);
		query.orderBy(builder.asc(rootGrantCallAttachType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCall fetchGrantCallById(Integer grantCallId) {
		return hibernateTemplate.get(GrantCall.class, grantCallId);
	}

	@Override
	public GrantCall saveOrUpdateGrantCall(GrantCall grantCall) {
		hibernateTemplate.saveOrUpdate(grantCall);
		return grantCall;
	}

	@Override
	public GrantCallStatus fetchStatusByStatusCode(Integer grantStatusCode) {
		return hibernateTemplate.get(GrantCallStatus.class, grantStatusCode);
	}

	@Override
	public GrantCallAttachment fetchAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(GrantCallAttachment.class, attachmentId);
	}

	@Override
	public GrantCallType fetchGrantCallTypeByGrantTypeCode(Integer grantTypeCode) {
		return hibernateTemplate.get(GrantCallType.class, grantTypeCode);
	}

	@Override
	public String deleteGrantCall(Integer grantCallId) {
		GrantCall grantCall = hibernateTemplate.get(GrantCall.class, grantCallId);
		hibernateTemplate.delete(grantCall);
		return "GrantCall deleted successfully";
	}

	@Override
	public List<Sponsor> fetchSponsorsBySponsorType(String searchString, String sponsorTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Sponsor> query = builder.createQuery(Sponsor.class);
		Root<Sponsor> rootSponsor = query.from(Sponsor.class);
		Predicate predicate1 = null;
		Predicate predicate2 = builder.like(builder.lower(rootSponsor.get("sponsorName")), "%" + searchString.toLowerCase() + "%");
		Predicate predicate3 = builder.like(builder.lower(rootSponsor.get("sponsorCode")), "%" + searchString.toLowerCase() + "%");
		Predicate predicate4 = builder.like(builder.lower(rootSponsor.get("acronym")), "%" + searchString.toLowerCase() + "%");
		Predicate predicate5 = builder.or(predicate3, predicate4, predicate2);
		if (sponsorTypeCode != null && !sponsorTypeCode.isEmpty()) {
			predicate1 = builder.equal(rootSponsor.get("sponsorTypeCode"), sponsorTypeCode);
			query.where(builder.and(predicate1, predicate5));
		} else {
			query.where(builder.and(predicate5));
		}
		query.orderBy(builder.asc(rootSponsor.get("sponsorName")));
		return session.createQuery(query).setMaxResults(25).getResultList();
	}

	@Override
	public List<Proposal> fetchProposalsByGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> query = builder.createQuery(Proposal.class);
		Root<Proposal> rootProposal = query.from(Proposal.class);
		query.where(builder.equal(rootProposal.get(GRANT_CALL_ID), grantCallId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SponsorFundingScheme> fetchFundingSchemeBySponsor(String sponsorCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorFundingScheme> query = builder.createQuery(SponsorFundingScheme.class);
		Root<SponsorFundingScheme> rootSponsor = query.from(SponsorFundingScheme.class);
		query.where(builder.equal(rootSponsor.get("sponsorCode"), sponsorCode));
		query.orderBy(builder.asc(rootSponsor.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<FundingSchemeAttachment> fetchFundingSchemeAttachmentBasedOnScheme(Integer fundingSchemeId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<FundingSchemeAttachment> query = builder.createQuery(FundingSchemeAttachment.class);
		Root<FundingSchemeAttachment> rootFundingSchemeAttachment = query.from(FundingSchemeAttachment.class);
		query.where(builder.equal(rootFundingSchemeAttachment.get("fundingSchemeId"), fundingSchemeId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public FundingSchemeAttachment fetchFundingSchemeAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(FundingSchemeAttachment.class, attachmentId);
		}

	@SuppressWarnings("rawtypes")
	@Override
	public String getMaxGrantCallKeyword() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String queryForMax = "SELECT MAX(CAST(SCIENCE_KEYWORD_CODE AS UNSIGNED INTEGER))+1 as VALUE  FROM SCIENCE_KEYWORD";
		Query query = session.createSQLQuery(queryForMax);
		BigInteger maxValue = (BigInteger) query.getResultList().get(0);
		return maxValue + "";
	}

	@Override
	public GrantCallKeyword saveOrUpdateGrantCallKeyword(GrantCallKeyword grantCallKeyword) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallKeyword);
		} catch (Exception e) {
			logger.error("Exception while saving GrantCallKeyword {}" , e.getMessage());
		}
		return grantCallKeyword;
	}

	@Override
	public List<RelevantField> fetchAllRelevantFields() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<RelevantField> query = builder.createQuery(RelevantField.class);
		Root<RelevantField> rootRelevantField = query.from(RelevantField.class);
		query.where(builder.and(builder.equal(rootRelevantField.get("isActive"), Boolean.TRUE)));
		return session.createQuery(query).getResultList();		
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<GrantCallRelevant> getGrantCallRelevantFieldsByGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM GrantCallRelevant WHERE grantCall.grantCallId=:grantCallId";
		Query<GrantCallRelevant> query = session.createQuery(hqlQuery);
		query.setParameter(GRANT_CALL_ID, grantCallId);
		return query.getResultList();
	}

	@Override
	public GrantCall fetchGrantCallDetails(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCall> query = builder.createQuery(GrantCall.class);
		Root<GrantCall> rootGrantCall = query.from(GrantCall.class);
		Predicate predicate1 = builder.equal(rootGrantCall.get(GRANT_CALL_ID), grantCallId);
		query.where(builder.and(predicate1));
		query.select(builder.construct(GrantCall.class, rootGrantCall.get("grantCallName"), rootGrantCall.get("closingDate")));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<GrantEligibiltyTargetType> fetchAllGrantEligibiltyTargetTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantEligibiltyTargetType> query = builder.createQuery(GrantEligibiltyTargetType.class);
		Root<GrantEligibiltyTargetType> rootGrantEligibiltyTargetType = query.from(GrantEligibiltyTargetType.class);
		query.orderBy(builder.asc(rootGrantEligibiltyTargetType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallIOIQuestionnaire saveOrUpdateGrantCallIOIQuestionnaire(GrantCallIOIQuestionnaire grantQuestionnaire) {
		hibernateTemplate.saveOrUpdate(grantQuestionnaire);
		return grantQuestionnaire;
	}

	@SuppressWarnings("unchecked")
	@Override
	public GrantCallIOIQuestionnaire fetchGrantCallIOIQuestionnaireByGrantId(Integer grantCallHeaderId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM GrantCallIOIQuestionnaire WHERE grantCallId=:grantCallHeaderId";
		Query<GrantCallIOIQuestionnaire> query = session.createQuery(hqlQuery);
		query.setParameter("grantCallHeaderId", grantCallHeaderId);
		if (query.getResultList() != null && !(query.getResultList()).isEmpty()) {
			return query.getSingleResult();
		}
		return null;
	}

	@Override
	public void deleteGrantIOIQuestionnaire(Integer grantIOIQuestionnaireId) {
		GrantCallIOIQuestionnaire grantCallIOIQuestionnaire = hibernateTemplate.get(GrantCallIOIQuestionnaire.class, grantIOIQuestionnaireId);
		hibernateTemplate.delete(grantCallIOIQuestionnaire);
	}

	@Override
	public List<HashMap<String, Object>> fetchEligibilityTargetPersons(Integer moduleItemKey) {
			ArrayList<HashMap<String, Object>> output = new ArrayList<>();
			try {
			String query = "SELECT T2.TARGET_VALUE FROM GRANT_CALL_HEADER T3\r\n" + 
					"LEFT JOIN GRANT_CALL_ELIGIBILITY T1 ON T1.GRANT_HEADER_ID = T3.GRANT_HEADER_ID\r\n" + 
					"INNER JOIN GRANT_ELIGIBILITY_TARGET T2 ON T1.GRANT_ELIGIBILITY_ID = T2.GRANT_ELIGIBILITY_ID\r\n" + 
					"WHERE T3.GRANT_HEADER_ID ="+moduleItemKey+"\r\n" + 
					"AND T2.ELIGIBILITY_TARGET_TYPE_CODE = 3\r\n" + 
					"UNION\r\n" + 
					"\r\n" + 
					"SELECT T4.PERSON_ID FROM GRANT_CALL_HEADER T3\r\n" + 
					"LEFT JOIN GRANT_CALL_ELIGIBILITY T1 ON T1.GRANT_HEADER_ID = T3.GRANT_HEADER_ID\r\n" + 
					"INNER JOIN GRANT_ELIGIBILITY_TARGET T2 ON T1.GRANT_ELIGIBILITY_ID = T2.GRANT_ELIGIBILITY_ID\r\n" + 
					"LEFT JOIN PERSON T4 ON T4.HOME_UNIT = T2.TARGET_VALUE\r\n" + 
					"WHERE T3.GRANT_HEADER_ID = "+moduleItemKey+"\r\n" + 
					"AND T2.ELIGIBILITY_TARGET_TYPE_CODE = 2";
//					"UNION\r\n" + 
//					"\r\n" + 
//					"SELECT T4.PERSON_ID FROM GRANT_CALL_HEADER T3\r\n" + 
//					"LEFT JOIN GRANT_CALL_ELIGIBILITY T1 ON T1.GRANT_HEADER_ID = T3.GRANT_HEADER_ID\r\n" + 
//					"INNER JOIN GRANT_ELIGIBILITY_TARGET T2 ON T1.GRANT_ELIGIBILITY_ID = T2.GRANT_ELIGIBILITY_ID\r\n" + 
//					"LEFT JOIN PERSON T4 ON T4.PERSON_ID = T4.PERSON_ID\r\n" + 
//					"WHERE T3.GRANT_HEADER_ID = \"+moduleItemKey+\"\r\n" + 
//					"AND T2.ELIGIBILITY_TARGET_TYPE_CODE in(1)\r\n" + 
//					"AND T4.IS_FACULTY = 'Y'\r\n" + 
//					"AND T4.STATUS = 'A'";
			output = dbEngine.executeQuerySQL(new ArrayList<Parameter>(),query);
			} catch (Exception e) {
				logger.error("Exception in fetchEligibilityTargetPersons {}" , e.getMessage());
			}
			return output;
		}	

	public GrantCallKPI deleteGrantCallKPI(GrantCallKPI grantCallKPI) {
		try {
			hibernateTemplate.delete(grantCallKPI);
		} catch (Exception e) {
			logger.error("Exception while deleting GrantCallKPI {}" , e.getMessage());
		}
		return grantCallKPI;
	}

	@Override
	public GrantCallScoringCriteria deleteGrantCallScoringCriteria(GrantCallScoringCriteria grantCallScoringCriteria) {
		try {
			hibernateTemplate.delete(grantCallScoringCriteria);
		} catch (Exception e) {
			logger.error("Exception while deleting GrantCallScoringCriteria {}", e.getMessage());
		}
		return grantCallScoringCriteria;
	}

	@Override
	public ProposalEvaluationScore saveOrUpdateProposalEvalautionScore(ProposalEvaluationScore proposalEvaluationScore) {
	try {
			hibernateTemplate.saveOrUpdate(proposalEvaluationScore);
		} catch (Exception e) {
			logger.error("Exception while saving ProposalEvalautionScore {}", e.getMessage());
		}
		return proposalEvaluationScore;
	}

	@Override
	public List<ProposalEvaluationScore> fetchProposalEvaluationById(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalEvaluationScore> query = builder.createQuery(ProposalEvaluationScore.class);
		Root<ProposalEvaluationScore> rootproposalEvaluationScore = query.from(ProposalEvaluationScore.class);
		query.where(builder.equal(rootproposalEvaluationScore.get("grantHeaderId"), grantCallId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallEvaluationPanel deleteGrantCallEvaluationPanel(GrantCallEvaluationPanel grantCallEvaluationPanel) {
		try {
			hibernateTemplate.delete(grantCallEvaluationPanel);
		} catch (Exception e) {
			logger.error("Exception while deleting GrantCallEvaluationPanel {}", e.getMessage());
		}
		return grantCallEvaluationPanel;
	}

	@Override
	public Integer fetchGrantCategoryCodeByGrantTypeCode(Integer grantTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT categoryCode FROM GrantCallType WHERE grantTypeCode=:grantTypeCode";
		@SuppressWarnings("unchecked")
		Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("grantTypeCode", grantTypeCode);
		return query.uniqueResult();
	}

	@Override
	public GrantCallEligibility saveOrUpdateGrantCallEligibility(GrantCallEligibility grantCallEligibility) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallEligibility);
		} catch (Exception e) {
			logger.error("Exception while saving GrantCallEligibility {}", e.getMessage());
		}
		return grantCallEligibility;
	}

	@Override
	public String getGrantCallNameByGrantId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT grantCallName FROM GrantCall WHERE grantCallId=:grantCallId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("grantCallId", grantCallId);
		return query.getSingleResult().toString();
	}

	@Override
	public String getLeadUnitNumberByGrantId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT homeUnitNumber FROM GrantCall WHERE grantCallId=:grantCallId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("grantCallId", grantCallId);
		return (String) query.getSingleResult();
	}

	@Override
	public List<SponsorFundingScheme> fetchAllSponsorFundingSchemes() {
		return hibernateTemplate.loadAll(SponsorFundingScheme.class);
	}

	@Override
	public GrantCallActionLog saveOrUpdateGrantCallActionLog(GrantCallActionLog grantCallActionLog) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallActionLog);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallActionLog :{}", e.getMessage());
		}
		return grantCallActionLog;
	}

	@Override
	public List<GrantCallActionLog> fetchGrantCallActionLog(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallActionLog> query = builder.createQuery(GrantCallActionLog.class);
		Root<GrantCallActionLog> rootGrantCallActionLog = query.from(GrantCallActionLog.class);
		query.where(builder.equal(rootGrantCallActionLog.get("grantHeaderId"), grantCallId));
		query.orderBy(builder.desc(rootGrantCallActionLog.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String getGrantCallTitleByGrantId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT grantCallName FROM GrantCall WHERE grantCallId=:grantCallId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("grantCallId", grantCallId);
		return (String) query.getSingleResult();
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Boolean checkGrantCallLinked(Integer grantCallId) {
		try {
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createNativeQuery("SELECT SUM(GRANTCALLLINKCOUNT) FROM (SELECT COUNT(*) GRANTCALLLINKCOUNT FROM EPS_PROPOSAL WHERE GRANT_HEADER_ID = :grantCallId UNION SELECT COUNT(*) GRANTCALLLINKCOUNT FROM AWARD WHERE GRANT_HEADER_ID = :grantCallId) GRANTCALLLINKCOUNT");
			query.setParameter(GRANT_CALL_ID, grantCallId);
			return Integer.parseInt(query.getSingleResult().toString()) > 0 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

	@Override
	public Boolean checkGrantEligibilityExternalExist(Integer grantCallId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT count(*) FROM GrantCall T1 left join GrantCallEligibility T2 ON T1.grantCallId=T2.grantCallId "
					+ "inner join GrantEligibilityTarget T3 on T3.grantCallEligibility.grantEligibilityId=T2.grantEligibilityId WHERE T1.grantCallId=:grantCallId and T3.eligibilityTargetTypeCode=1";
			javax.persistence.Query query = session.createQuery(hqlQuery);
			query.setParameter("grantCallId", grantCallId);
			Long count = (Long) query.getSingleResult();
			return count > 0 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

}
