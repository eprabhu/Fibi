package com.polus.fibicomp.coi.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.coi.dto.DisclosureDetailDto;
import com.polus.fibicomp.coi.pojo.COIDisclosureCategoryType;
import com.polus.fibicomp.coi.pojo.COIDisclosureStatus;
import com.polus.fibicomp.coi.pojo.COIDispositionStatus;
import com.polus.fibicomp.coi.pojo.COIEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntity;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityDetails;
import com.polus.fibicomp.coi.pojo.COIFinancialEntityRelType;
import com.polus.fibicomp.coi.pojo.COIReviewStatus;
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiDisclosureDetails;
import com.polus.fibicomp.coi.pojo.EntityStatus;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.security.AuthenticatedUser;

import oracle.jdbc.OracleTypes;

import com.polus.fibicomp.coi.pojo.CoiDisclosureDetailsStatus;
import com.polus.fibicomp.coi.pojo.CoiFileData;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewActivity;
import com.polus.fibicomp.coi.pojo.CoiReviewAssigneeHistory;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentTag;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;

@Service(value = "conflictOfInterestDao")
@Transactional
public class ConflictOfInterestDaoImpl implements ConflictOfInterestDao {

	protected static Logger logger = LogManager.getLogger(ConflictOfInterestDaoImpl.class.getName());
	
	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private CommonService commonService;

	@Override
	public CoiDisclosure saveOrUpdateCoiDisclosure(CoiDisclosure coiDisclosure) {
		hibernateTemplate.saveOrUpdate(coiDisclosure);
		return coiDisclosure;
	}

	@Override
	public CoiDisclosure loadDisclosure (Integer coiDisclosure) {
		return hibernateTemplate.get(CoiDisclosure.class, coiDisclosure);
	}

	@Override
	public Integer numberOfSFI (String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<COIFinancialEntity> rootCOIFinancialEntity = query.from(COIFinancialEntity.class);
		query.where(builder.equal(rootCOIFinancialEntity.get("personId"), personId));
		query.select(rootCOIFinancialEntity.get("coiFinancialEntityId"));
		return (session.createQuery(query).getResultList().size());
	}

	@Override
	public Integer generateMaxDisclosureId () {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
        CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
        Root<CoiDisclosure> rootAcAttachmentProtocol = query.from(CoiDisclosure.class);
        query.select(builder.max(rootAcAttachmentProtocol.get("disclosureId")));
        if(session.createQuery(query).getSingleResult() != null) {
            return session.createQuery(query).getSingleResult() + 1;
        } else {
            return 1;
        }
	}

	@Override
	public Integer getNumberOfDisclosure(String disclosureCategoryType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiDisclosure> query = builder.createQuery(CoiDisclosure.class);
		Root<CoiDisclosure> rootCOIFinancialEntity = query.from(CoiDisclosure.class);
		Predicate predicate1 = builder.equal(rootCOIFinancialEntity.get("disclosureCategoryTypeCode"), disclosureCategoryType);
		Predicate predicate2 = builder.equal(rootCOIFinancialEntity.get("personId"), AuthenticatedUser.getLoginPersonId());
		query.where(builder.and(predicate1,predicate2));
		return session.createQuery(query).getResultList().size();
	}

	@Override
	public List<COIFinancialEntity> fetchCOIFinancialEntityByPersonId(String personId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<COIFinancialEntity> criteria = builder.createQuery(COIFinancialEntity.class);
			Root<COIFinancialEntity> root = criteria.from(COIFinancialEntity.class);				
			Predicate predicatePersonId = builder.equal(root.get("personId"),personId);
			criteria.where(builder.and(predicatePersonId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<COIFinancialEntity> getSFIOfDisclosure(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<COIFinancialEntity> query = builder.createQuery(COIFinancialEntity.class);
			Root<COIFinancialEntity> rootCOIFinancialEntity = query.from(COIFinancialEntity.class);
			query.where(builder.equal(rootCOIFinancialEntity.get("personId"), personId));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}
	
	@Override
	public List<COIEntity> searchEnitiy(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<COIEntity> query = builder.createQuery(COIEntity.class);
		Root<COIEntity> rootEntityName = query.from(COIEntity.class);
		query.where(builder.like(builder.lower(rootEntityName.get("coiEntityName")), "%" + searchString.toLowerCase() + "%"));
		query.orderBy(builder.asc(rootEntityName.get("coiEntityName")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<EntityStatus> fetchEntityStatus() {
		return hibernateTemplate.loadAll(EntityStatus.class);
	}

	@Override
	public List<EntityType> fetchEntityType() {
		return hibernateTemplate.loadAll(EntityType.class);
	}

	@Override
	public List<COIFinancialEntityRelType> fetchCOIFinancialEntityRelType() {
		return hibernateTemplate.loadAll(COIFinancialEntityRelType.class);
	}

	@Override
	public List<CoiDisclosureDetailsStatus> getCoiDisclosureDetailStatuses() {
		return hibernateTemplate.loadAll(CoiDisclosureDetailsStatus.class);
	}

	@Override
	public COIFinancialEntity getSFIDetails(Integer coiFinancialEntityId) {
		return hibernateTemplate.get(COIFinancialEntity.class, coiFinancialEntityId);
	}

	@Override
	public COIFinancialEntity saveOrUpdateCoiSFI(COIFinancialEntity coiFinancialEntity) {
		hibernateTemplate.saveOrUpdate(coiFinancialEntity);
		return coiFinancialEntity;
	}

	@Override
	public COIEntity saveOrUpdateCOIEntity(COIEntity coiEntity) {
		hibernateTemplate.saveOrUpdate(coiEntity);
		return coiEntity;
	}

	@Override
	public List<COIFinancialEntityDetails> getCoiFinancialEntityDetails(Integer coiFinancialEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<COIFinancialEntityDetails> query = builder.createQuery(COIFinancialEntityDetails.class);
		Root<COIFinancialEntityDetails> rootCOIFinancialEntity = query.from(COIFinancialEntityDetails.class);
		query.where(builder.equal(rootCOIFinancialEntity.get("coiFinancialEntityId"), coiFinancialEntityId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public COIFinancialEntityDetails saveOrUpdateCoiFinancialEntityDetails(COIFinancialEntityDetails coiFinancialEntityDetails) {
		hibernateTemplate.saveOrUpdate(coiFinancialEntityDetails);
		return coiFinancialEntityDetails;
	}

	@Override
	public void certifyDisclosure(CoiDisclosure coiDisclosure) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclosure> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosure.class);
		Root<CoiDisclosure> root = criteriaUpdate.from(CoiDisclosure.class);
		criteriaUpdate.set("certificationText", coiDisclosure.getCertificationText());
		criteriaUpdate.set("certifiedTimestamp", coiDisclosure.getCertifiedTimestamp());
		criteriaUpdate.set("certifiedBy", coiDisclosure.getCertifiedBy());
		criteriaUpdate.set("disclosureStatusCode", coiDisclosure.getDisclosureStatusCode());
		criteriaUpdate.set("dispositionStatusTypeCode", coiDisclosure.getDispositionStatusTypeCode());
		criteriaUpdate.set("reviewStatusTypeCode", coiDisclosure.getReviewStatusTypeCode());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(cb.equal(root.get("disclosureId"), coiDisclosure.getDisclosureId()));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<CoiDisclosureDetails> getProjectRelationshipByParam(Integer moduleCode, Integer moduleItemId, String loginPersonId, Integer disclosureId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<CoiDisclosureDetails> criteria = builder.createQuery(CoiDisclosureDetails.class);
			Root<CoiDisclosureDetails> root = criteria.from(CoiDisclosureDetails.class);				
			Predicate predicatePersonId = builder.equal(root.get("coiDisclosure").get("personId"), loginPersonId);
			Predicate predicateDisclosureId = builder.equal(root.get("disclosureId"), disclosureId);
			if (moduleCode != null && moduleItemId != null) {
				Predicate predicateModuleCode = builder.equal(root.get("moduleCode"), moduleCode);
				Predicate predicateModuleItemId = builder.equal(root.get("moduleItemKey"), moduleItemId);
				criteria.where(builder.and(predicatePersonId, predicateModuleCode, predicateModuleItemId, predicateDisclosureId));
			} else {
				criteria.where(builder.and(predicatePersonId, predicateDisclosureId));
			}
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public CoiDisclosureDetails saveOrUpdateCoiDisclosureDetail(CoiDisclosureDetails entityProjectRelation) {
		hibernateTemplate.saveOrUpdate(entityProjectRelation);
		return entityProjectRelation;
	}

	@Override
	public Boolean checkIsSFICompletedForProject(Integer moduleCode, Integer moduleItemId, Integer disclosureId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT (SELECT COUNT(*) FROM COI_FINANCIAL_ENTITY WHERE PERSON_ID = :personId) = ");
		hqlQuery.append("(SELECT COUNT(*) FROM COI_DISCLOSURE C1 INNER JOIN COI_DISCLOSURE_DETAILS C2 ON C2.DISCLOSURE_ID=C1.DISCLOSURE_ID ");
		hqlQuery.append("INNER JOIN COI_FINANCIAL_ENTITY C3 ON C3.COI_FINANCIAL_ENTITY_ID=C2.COI_FINANCIAL_ENTITY_ID ");
		hqlQuery.append("INNER JOIN COI_DISC_DETAILS_COMMENTS C4 ON C4.DISCLOSURE_DETAILS_ID=C2.DISCLOSURE_DETAILS_ID ");
		hqlQuery.append("WHERE C2.DISC_DET_STATUS_CODE IS NOT NULL AND C4.COMMENT IS NOT NULL AND C1.PERSON_ID = :personId and C2.DISCLOSURE_ID = :disclosureId and C2.MODULE_CODE= :moduleCode and C2.MODULE_ITEM_KEY= :moduleItemId) as completedFlag");
		Query query = session.createNativeQuery(hqlQuery.toString());
		query.setParameter("personId", personId);
		query.setParameter("disclosureId", disclosureId);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("moduleItemId", moduleItemId);
		return Integer.parseInt(query.getSingleResult().toString()) > 0 ? Boolean.TRUE : Boolean.FALSE;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Integer> getProjectIdsBasedOnParams(Integer moduleCode, String personId, List<Integer> statuses, Integer disclosureId) {
		List<Integer> ids = new ArrayList<>();
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (disclosureId != null && Constants.DEV_PROPOSAL_MODULE_CODE.equals(moduleCode)) {
			hqlQuery.append("SELECT DISTINCT T1.MODULE_ITEM_KEY FROM COI_DISCLOSURE_DETAILS T1 INNER JOIN EPS_PROPOSAL T2 ON T2.PROPOSAL_ID=T1.MODULE_ITEM_KEY WHERE T1.MODULE_CODE = 3 AND T2.DOCUMENT_STATUS_CODE <> 3 AND T2.STATUS_CODE not IN (:statuses) AND T1.DISCLOSURE_ID= :disclosureId");
			Query  query = session.createNativeQuery(hqlQuery.toString());
			query.setParameter("statuses", statuses);
			query.setParameter("disclosureId", disclosureId);
			ids = query.getResultList();
		} else if (disclosureId != null && Constants.AWARD_MODULE_CODE.equals(moduleCode)) {
			hqlQuery.append("SELECT DISTINCT T1.MODULE_ITEM_KEY FROM COI_DISCLOSURE_DETAILS T1 INNER JOIN AWARD T2 ON T2.AWARD_ID=T1.MODULE_ITEM_KEY WHERE T1.MODULE_CODE = 1 AND (T2.AWARD_SEQUENCE_STATUS ='ACTIVE' OR (T2.AWARD_SEQUENCE_STATUS='PENDING' AND AWARD_DOCUMENT_TYPE_CODE=1)) AND T1.DISCLOSURE_ID= :disclosureId");
			Query  query = session.createNativeQuery(hqlQuery.toString());
			query.setParameter("disclosureId", disclosureId);
			ids = query.getResultList();
		} else if (Constants.DEV_PROPOSAL_MODULE_CODE.equals(moduleCode)) {
			hqlQuery.append("SELECT T2.PROPOSAL_ID FROM EPS_PROPOSAL T1 INNER JOIN EPS_PROPOSAL_PERSONS T2 ON T2.PROPOSAL_ID=T1.PROPOSAL_ID WHERE T1.DOCUMENT_STATUS_CODE <> 3 AND T1.STATUS_CODE not IN (:statuses) and T2.PERSON_ID = :personId");
			Query  query = session.createNativeQuery(hqlQuery.toString());
			query.setParameter("statuses", statuses);
			query.setParameter("personId", personId);
			ids = query.getResultList();
		} else if (Constants.AWARD_MODULE_CODE.equals(moduleCode)) {
			hqlQuery.append("SELECT T2.AWARD_ID FROM AWARD T1 INNER JOIN AWARD_PERSONS T2 ON T2.AWARD_ID=T1.AWARD_ID WHERE (T1.AWARD_SEQUENCE_STATUS ='ACTIVE' OR (T1.AWARD_SEQUENCE_STATUS='PENDING' AND AWARD_DOCUMENT_TYPE_CODE=1)) and T2.PERSON_ID = :personId");
			Query awardQuery = session.createNativeQuery(hqlQuery.toString());
			awardQuery.setParameter("personId", personId);
			ids = awardQuery.getResultList();
		}
		return ids;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<COIFinancialEntity> getSFIBasedOnDisclosureId(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT DISTINCT C3 FROM CoiDisclosure C1 INNER JOIN CoiDisclosureDetails C2 ON C2.disclosureId=C1.disclosureId ");
		hqlQuery.append("INNER JOIN COIFinancialEntity C3 ON C3.coiFinancialEntityId=C2.coiFinancialEntityId ");
		hqlQuery.append("WHERE  C2.disclosureId = :disclosureId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("disclosureId", disclosureId);
		return query.getResultList();
	}

	@Override
	public boolean evaluateDisclosureQuestionnaire(Integer moduleCode,Integer submoduleCode,Integer moduleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			String functionName = "FN_EVAL_DISCLOSURE_QUESTIONNAIRE";
			String functionCall = "{ ? = call " + functionName + "(?,?,?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setInt(2, moduleCode);
			statement.setInt(3, submoduleCode);
			statement.setInt(4, moduleItemKey);
			statement.execute();
			int result = statement.getInt(1);
			if (result == 1) {
				return true;
			}
		} catch (SQLException e) {
			throw new ApplicationException("error in evaluateDisclosureQuestionnaire", e, Constants.DB_FN_ERROR);
		}
		return false;
	}

	@Override
	public void setDisclosureQuestionnaire(Boolean isDisclosureQuestionnaire,Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclosure> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosure.class);
		Root<CoiDisclosure> root = criteriaUpdate.from(CoiDisclosure.class);
		criteriaUpdate.set("isDisclosureQuestionnaire",isDisclosureQuestionnaire);
		criteriaUpdate.where(cb.equal(root.get("disclosureId"),disclosureId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Integer> getDisclosureIdsByCOIFinancialEntityId(Integer coiFinancialEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT distinct(disclosureId) FROM CoiDisclosureDetails WHERE coiFinancialEntityId=:coiFinancialEntityId ";
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("coiFinancialEntityId", coiFinancialEntityId);
		return query.getResultList();
	}

	@Override
	public List<CoiDisclosure> getActiveAndPendingCoiDisclosureDetailsByDisclosureIdsAndSequenceStatus(List<Integer> disclosureIds, List<String> statusCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiDisclosure> queryCoiDisclosure = builder.createQuery(CoiDisclosure.class);
		Root<CoiDisclosure> rootCoiDisclosure = queryCoiDisclosure.from(CoiDisclosure.class);
		Predicate predicate1 = rootCoiDisclosure.get("disclosureId").in(disclosureIds);
		Predicate predicate2 = rootCoiDisclosure.get("disclosureSequenceStatusCode").in(statusCodes);
		queryCoiDisclosure.where(builder.and(predicate1,predicate2));
		return session.createQuery(queryCoiDisclosure).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getModuleItemKeysByCOIFinancialEntityIdAndModuleCode(Integer coiFinancialEntityId, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT distinct(moduleItemKey) FROM CoiDisclosureDetails WHERE coiFinancialEntityId=:coiFinancialEntityId and moduleCode=:moduleCode ";
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("coiFinancialEntityId", coiFinancialEntityId);
		query.setParameter("moduleCode", moduleCode);
		return query.getResultList();
	}

	@Override
	public List<Proposal> getProposalsBasedOnProposalIds(List<Integer> proposalIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> queryProposal = builder.createQuery(Proposal.class);
		Root<Proposal> rootProposal = queryProposal.from(Proposal.class);
		queryProposal.where(rootProposal.get("proposalId").in(proposalIds));
		return session.createQuery(queryProposal).getResultList();
	}

	@Override
	public List<Award> getAwardsBasedOnAwardIds(List<Integer> awardIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> queryAward = builder.createQuery(Award.class);
		Root<Award> rootAward = queryAward.from(Award.class);
		queryAward.where(rootAward.get("awardId").in(awardIds));
		return session.createQuery(queryAward).getResultList();
	}

	@Override
	public List<CoiSectionsType> fetchCoiSections() {
		return hibernateTemplate.loadAll(CoiSectionsType.class);
	}

	@Override
	public CoiReview saveOrUpdateCoiReview(CoiReview coiReview) {
		hibernateTemplate.saveOrUpdate(coiReview);
		return coiReview;
	}

	@Override
	public List<CoiReview> getCoiReview(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiReview> query = builder.createQuery(CoiReview.class);
		Root<CoiReview> rootCoiReview = query.from(CoiReview.class);
		query.where(builder.equal(rootCoiReview.get("disclosureId"), disclosureId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void startReview(String reviewStatusTypeCode ,Integer coiReviewId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiReview> criteriaUpdate = cb.createCriteriaUpdate(CoiReview.class);
		Root<CoiReview> root = criteriaUpdate.from(CoiReview.class);
		criteriaUpdate.set("reviewStatusTypeCode",reviewStatusTypeCode);
		criteriaUpdate.where(cb.equal(root.get("coiReviewId"),coiReviewId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public CoiReview loadCoiReview (Integer coiReviewId) {
		return hibernateTemplate.get(CoiReview.class, coiReviewId);
	}

	@Override
	public List<CoiReviewActivity> fetchCoiReviewActivity() {
		return hibernateTemplate.loadAll(CoiReviewActivity.class);
	}

	@Override
	public CoiReviewComments saveOrUpdateCoiReviewComments(CoiReviewComments coiReviewComments) {
		hibernateTemplate.saveOrUpdate(coiReviewComments);
		return coiReviewComments;
	}

	@Override
	public CoiReviewCommentAttachment saveOrUpdateAttachment(CoiReviewCommentAttachment coiReviewCommentAttachment) {
		hibernateTemplate.saveOrUpdate(coiReviewCommentAttachment);
		return coiReviewCommentAttachment;
	}

	@Override
	public CoiFileData saveFileData(CoiFileData fileData) {
		hibernateTemplate.save(fileData);
		return fileData;
	}

	@Override
	public COIReviewStatus getReviewStatus(String reviewStatusTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<COIReviewStatus> query = builder.createQuery(COIReviewStatus.class);
		Root<COIReviewStatus> rootCoiReview = query.from(COIReviewStatus.class);
		query.where(builder.equal(rootCoiReview.get("reviewStatusTypeCode"), reviewStatusTypeCode));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public CoiReviewAssigneeHistory saveOrUpdateCoiReviewAssigneeHistory(CoiReviewAssigneeHistory coiReviewAssigneeHistory) {
		hibernateTemplate.saveOrUpdate(coiReviewAssigneeHistory);
		return coiReviewAssigneeHistory;
	}

	@Override
	public COIDisclosureStatus getDisclosureStatusByCode(String disclosureStatusCode) {
		return hibernateTemplate.get(COIDisclosureStatus.class, disclosureStatusCode);
	}

	@Override
	public COIDispositionStatus getDispositionStatusByCode(String dispositionStatusTypeCode) {
		return hibernateTemplate.get(COIDispositionStatus.class, dispositionStatusTypeCode);
	}

	@Override
	public Integer getSFICountBasedOnParams(String disclosureStatusCode, String personId, Integer disclosureId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode)) {
			hqlQuery.append("select count(*) from COIFinancialEntity where personId=:personId");
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("personId", personId);
			return Integer.parseInt(query.getSingleResult().toString());
		} else {
			hqlQuery.append("SELECT COUNT(DISTINCT C3) FROM CoiDisclosure C1 INNER JOIN CoiDisclosureDetails C2 ON C2.disclosureId=C1.disclosureId ");
			hqlQuery.append("INNER JOIN COIFinancialEntity C3 ON C3.coiFinancialEntityId=C2.coiFinancialEntityId ");
			hqlQuery.append("WHERE  C2.disclosureId = :disclosureId");
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("disclosureId", disclosureId);
			return Integer.parseInt(query.getSingleResult().toString());
		}
	}

	@Override
	public List<CoiReviewCommentAttachment> fetchReviewCommentAttachment(Integer coiReviewCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiReviewCommentAttachment> queryAward = builder.createQuery(CoiReviewCommentAttachment.class);
		Root<CoiReviewCommentAttachment> rootAward = queryAward.from(CoiReviewCommentAttachment.class);
		queryAward.where(rootAward.get("coiReviewCommentId").in(coiReviewCommentId));
		return session.createQuery(queryAward).getResultList();
	}
	
	@Override
	public void deleteFileData(CoiFileData fileData) {
		hibernateTemplate.delete(fileData);
	}
	
	@Override
	public void deleteReviewCommentAttachment(Integer coiReviewId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReviewCommentAttachment> query = builder.createCriteriaDelete(CoiReviewCommentAttachment.class);
		Root<CoiReviewCommentAttachment> root = query.from(CoiReviewCommentAttachment.class);
		query.where(builder.equal(root.get("coiReviewId"), coiReviewId));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteReviewComment(Integer coiReviewId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReviewComments> query = builder.createCriteriaDelete(CoiReviewComments.class);
		Root<CoiReviewComments> root = query.from(CoiReviewComments.class);
		query.where(builder.equal(root.get("coiReviewId"), coiReviewId));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteReview(Integer coiReviewId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReview> query = builder.createCriteriaDelete(CoiReview.class);
		Root<CoiReview> root = query.from(CoiReview.class);
		query.where(builder.equal(root.get("coiReviewId"), coiReviewId));
		session.createQuery(query).executeUpdate();
	}
	
	@Override
	public void deleteReviewAttachmentByCommentId(Integer coiReviewCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReviewCommentAttachment> query = builder.createCriteriaDelete(CoiReviewCommentAttachment.class);
		Root<CoiReviewCommentAttachment> root = query.from(CoiReviewCommentAttachment.class);
		query.where(builder.equal(root.get("coiReviewCommentId"), coiReviewCommentId));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteReviewCommentByCommentId(Integer coiReviewCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReviewComments> query = builder.createCriteriaDelete(CoiReviewComments.class);
		Root<CoiReviewComments> root = query.from(CoiReviewComments.class);
		query.where(builder.equal(root.get("coiReviewCommentId"), coiReviewCommentId));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteReviewAssigneeHistory(Integer coiReviewId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReviewAssigneeHistory> query = builder.createCriteriaDelete(CoiReviewAssigneeHistory.class);
		Root<CoiReviewAssigneeHistory> root = query.from(CoiReviewAssigneeHistory.class);
		query.where(builder.equal(root.get("coiReviewId"), coiReviewId));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public CoiReviewCommentAttachment fetchAttachmentById(Integer coiReviewCommentAttId) {
		return hibernateTemplate.get(CoiReviewCommentAttachment.class, coiReviewCommentAttId);
	}

	@Override
	public CoiFileData getFileDataById(String fileDataId) {
		return hibernateTemplate.get(CoiFileData.class, fileDataId);
	}

	@Override
	public List<CoiReviewCommentAttachment> fetchReviewAttachmentByReviewId(Integer coiReviewId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiReviewCommentAttachment> query = builder.createQuery(CoiReviewCommentAttachment.class);
		Root<CoiReviewCommentAttachment> root = query.from(CoiReviewCommentAttachment.class);
		query.where(root.get("coiReviewId").in(coiReviewId));
		return session.createQuery(query).getResultList();
	}
	
	@Override
	public List<CoiReviewCommentAttachment> fetchReviewAttachmentByCommentId(Integer coiReviewCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiReviewCommentAttachment> query = builder.createQuery(CoiReviewCommentAttachment.class);
		Root<CoiReviewCommentAttachment> root = query.from(CoiReviewCommentAttachment.class);
		query.where(root.get("coiReviewCommentId").in(coiReviewCommentId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public CoiReviewCommentAttachment deleteAttachment(Integer coiReviewCommentAttId) {
		CoiReviewCommentAttachment coiReviewCommentAttachment = hibernateTemplate.get(CoiReviewCommentAttachment.class, coiReviewCommentAttId);
		CoiFileData filedata = hibernateTemplate.get(CoiFileData.class, coiReviewCommentAttachment.getFileDataId());
		if (filedata != null) {
			hibernateTemplate.delete(coiReviewCommentAttachment);
			hibernateTemplate.delete(filedata);
		}
		return coiReviewCommentAttachment;
	}

	@Override
	public ConflictOfInterestVO loadCoiReviewComments(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiReviewComments> query = builder.createQuery(CoiReviewComments.class);
		Root<CoiReviewComments> root = query.from(CoiReviewComments.class);
		Subquery<Integer> subQuery = query.subquery(Integer.class);
		Root<CoiReviewCommentTag> rootTag = subQuery.from(CoiReviewCommentTag.class);
		List<Order> orderList = new ArrayList<>();
		List<Predicate> inRestrictions = new ArrayList<>();
		List<Predicate> inRestrictions2 = new ArrayList<>();
		Predicate predicateOne =  builder.in(root.get("disclosureId")).value(vo.getDisclosureId());
		Predicate predicateTwo =  builder.in(root.get("coiSubSectionsId")).value(vo.getCoiSubSectionsId());
		Predicate predicateThree = builder.like(root.get("coiSectionsTypeCode"), "%" + vo.getCoiSectionsTypeCode() + "%");	
		if(vo.getPersonId() != null) {
			Predicate predicate1 = builder.like(rootTag.get("tagPersonId"), "%" + vo.getPersonId() + "%");
			Predicate predicate2 = builder.in(rootTag.get("tagGroupId")).value(vo.getTagGroupId());
			subQuery.select(rootTag.get("coiReviewCommentId"));
			if(!vo.getTagGroupId().isEmpty()) {
				inRestrictions2.add(predicate2);
			}
			if(vo.getPersonId() != null) {
				inRestrictions2.add(predicate1);
			}
			subQuery.where(builder.or(inRestrictions2.toArray(new Predicate[inRestrictions2.size()])));
		}
		Predicate predicate4 = builder.in(root.get("coiReviewCommentId")).value(subQuery);
		if(vo.getDisclosureId() != null) {
			inRestrictions.add(predicateOne);
		}
		if(vo.getCoiSubSectionsId() != null) {
			inRestrictions.add(predicateTwo);
		}
		if(vo.getCoiSectionsTypeCode() != null) {
			inRestrictions.add(predicateThree);
		}
		if(vo.getPersonId() != null) {
			inRestrictions.add(predicate4);
		}
		if(vo.getSort().equals("desc")) {
		 orderList.add(builder.desc(root.get("updateTimestamp")));
		} else {
		 orderList.add(builder.asc(root.get("updateTimestamp")));
		}
		query.orderBy(orderList);
		query.where(builder.and(inRestrictions.toArray(new Predicate[inRestrictions.size()])));
		vo.setCoiReviewComments(session.createQuery(query).getResultList());
		vo.setCommentCount((session.createQuery(query).getResultList()).size());
		return vo;
	}
	
	@Override
	public CoiReviewCommentTag saveOrUpdateCoiReviewCommentTag(CoiReviewCommentTag coiReviewCommentTag) {
		hibernateTemplate.saveOrUpdate(coiReviewCommentTag);
		return coiReviewCommentTag;
	}

	@Override
	public CoiDisclosureDetails getProjectRelationship(Integer disclosureDetailsId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiDisclosureDetails> query = builder.createQuery(CoiDisclosureDetails.class);
		Root<CoiDisclosureDetails> root = query.from(CoiDisclosureDetails.class);
		query.where(root.get("disclosureDetailsId").in(disclosureDetailsId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public List<CoiReviewCommentTag> fetchCoiReviewCommentTag(Integer coiReviewCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiReviewCommentTag> query = builder.createQuery(CoiReviewCommentTag.class);
		Root<CoiReviewCommentTag> root = query.from(CoiReviewCommentTag.class);
		query.where(root.get("coiReviewCommentId").in(coiReviewCommentId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String fetchadminGroupName(Integer adminGroupId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<AdminGroup> root = query.from(AdminGroup.class);
		query.select(root.get("adminGroupName"));
		query.where(root.get("adminGroupId").in(adminGroupId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public void deleteReviewTagByCommentId(Integer coiReviewCommentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReviewCommentTag> query = builder.createCriteriaDelete(CoiReviewCommentTag.class);
		Root<CoiReviewCommentTag> root = query.from(CoiReviewCommentTag.class);
		query.where(builder.equal(root.get("coiReviewCommentId"), coiReviewCommentId));
		session.createQuery(query).executeUpdate();
	}
	
	@Override
	public Integer numberOfInCompleteReview(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<CoiReview> root = query.from(CoiReview.class);
		Predicate predicate1 = builder.equal(root.get("disclosureId"), disclosureId);
		Predicate predicate2 = builder.notEqual(root.get("reviewStatusTypeCode"), "3");
		query.select(root.get("coiReviewId"));
		query.where(builder.and(predicate1,predicate2));
		return (session.createQuery(query).getResultList().size());
	}

	@Override
	public void completeDisclosureReview(CoiDisclosure coiDisclosure) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclosure> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosure.class);
		Root<CoiDisclosure> root = criteriaUpdate.from(CoiDisclosure.class);
		criteriaUpdate.set("disclosureStatusCode", coiDisclosure.getDisclosureStatusCode());
		criteriaUpdate.set("dispositionStatusTypeCode", coiDisclosure.getDispositionStatusTypeCode());
		criteriaUpdate.set("reviewStatusTypeCode", coiDisclosure.getReviewStatusTypeCode());
		criteriaUpdate.set("disclosureSequenceStatusCode", coiDisclosure.getDisclosureSequenceStatusCode());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(cb.equal(root.get("disclosureId"), coiDisclosure.getDisclosureId()));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public void deleteReviewTagByReviewId(Integer coiReviewId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<CoiReviewCommentTag> query = builder.createCriteriaDelete(CoiReviewCommentTag.class);
		Root<CoiReviewCommentTag> root = query.from(CoiReviewCommentTag.class);
		query.where(builder.equal(root.get("coiReviewId"), coiReviewId));
		session.createQuery(query).executeUpdate();
	}
	
	@Override
	public void addReviewerStatus(CoiDisclosureDetails coiDisclosureDetails) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclosureDetails> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosureDetails.class);
		Root<CoiDisclosureDetails> root = criteriaUpdate.from(CoiDisclosureDetails.class);
		criteriaUpdate.set("coiReviewerStatusCode", coiDisclosureDetails.getCoiReviewerStatusCode());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("disclosureDetailsId"), coiDisclosureDetails.getDisclosureDetailsId()));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public CoiConflictHistory saveOrUpdateCoiConflictHistory(CoiConflictHistory coiConflictHistory) {
		hibernateTemplate.saveOrUpdate(coiConflictHistory);
		return coiConflictHistory;
	}

	@Override
	public List<CoiConflictHistory> getCoiConflictHistory(Integer disclosureDetailsId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiConflictHistory> query = builder.createQuery(CoiConflictHistory.class);
		Root<CoiConflictHistory> root = query.from(CoiConflictHistory.class);
		query.where(root.get("disclosureDetailsId").in(disclosureDetailsId));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<DisclosureDetailDto> getProposalsBasedOnParams(List<Integer> proposalIds, List<Integer> proposalStatuses, String searchString) {
		StringBuilder hqlQuery = new StringBuilder();
		String likeCriteria = "%" + searchString.toUpperCase() + "%";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("select t1.proposalId,t1.title,t3.sponsorCode,t3.sponsorName,t3.acronym,t2.fullName,t1.startDate,t1.endDate,t4.unitNumber,t4.unitName from Proposal t1 left join ProposalPerson t2 on t2.proposalId = t1.proposalId and t2.isPi='Y' ");
		hqlQuery.append(" left join Sponsor t3 on t3.sponsorCode = t1.sponsorCode left join Unit t4 on t4.unitNumber= t1.homeUnitNumber");
		hqlQuery.append(" where t1.documentStatusCode <> 3 and t1.proposalId in (:proposalIds)");
		hqlQuery.append(" and (UPPER(cast(t1.proposalId as string)) like :likeCriteria or UPPER(t1.sponsorCode) like :likeCriteria or UPPER(t3.sponsorName) like :likeCriteria");
		hqlQuery.append(" or UPPER(t1.homeUnitNumber) like :likeCriteria OR UPPER(t4.unitName) like :likeCriteria  OR UPPER(t2.fullName) like :likeCriteria");
		hqlQuery.append(" or UPPER(t1.title) like :likeCriteria )");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("proposalIds", proposalIds);
		query.setParameter("likeCriteria", likeCriteria);
		List<Object[]> entities = query.getResultList();
		List<DisclosureDetailDto> proposals = new ArrayList<>();
		for (Object[] entity : entities) {
			DisclosureDetailDto proposal = new DisclosureDetailDto();
			proposal.setModuleItemId(entity[0] != null ? Integer.parseInt(entity[0].toString()): null);
			proposal.setTitle(entity[1].toString());
			String sponsorCode = null;
			String sponsorName = null;
			String sponsorAcronym = null;
			String unitNumber = null;
			String unitName = null;
			if (entity[2] != null) {
				sponsorCode = entity[2].toString();
			}
			if (entity[3] != null) {
				sponsorName = entity[3].toString();
			}
			if (entity[4] != null) {
				sponsorAcronym = entity[4].toString();
			}
			proposal.setSponsor(commonService.getSponsorFormatBySponsorDetail(sponsorCode, sponsorName, sponsorAcronym));
			proposal.setPrincipalInvestigator(entity[5] !=null ? entity[5].toString() : null);
			proposal.setStartDate(entity[6] != null ? Timestamp.valueOf(entity[6].toString()) : null);
			proposal.setEndDate(entity[7] != null ? Timestamp.valueOf(entity[7].toString()) : null);
			if (entity[8] != null) {
				unitNumber = entity[8].toString();
			}
			if (entity[9] != null) {
				unitName = entity[9].toString();
			}
			proposal.setUnitName(commonService.getUnitFormatByUnitDetail(unitNumber, unitName));
			proposals.add(proposal);
		}
		return proposals;
	}

	@Override
	public String getProposalIdLinkedInDisclosure(Integer disclosureId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("select distinct moduleItemKey from CoiDisclosureDetails where disclosureId =:disclosureId and moduleCode = 3");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("disclosureId", disclosureId);
		return query.getSingleResult().toString();
	}

	@Override
	public COIDisclosureCategoryType getDisclosureCategoryTypeByCode(String disclosureCategoryTypeCode) {
		return hibernateTemplate.get(COIDisclosureCategoryType.class, disclosureCategoryTypeCode);
	}

	@Override
	public List<CoiDisclosure> getCoiDisclosuresByDisclosureNumber(String disclosureNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiDisclosure> query = builder.createQuery(CoiDisclosure.class);
		Root<CoiDisclosure> rootCoiReview = query.from(CoiDisclosure.class);
		query.where(builder.equal(rootCoiReview.get("disclosureNumber"), disclosureNumber));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer getReviewCommentsCount() {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("select count(*) from CoiReviewComments where coiParentCommentId is null and coiReviewCommentId in");
		hqlQuery.append(" (Select coiReviewCommentId from CoiReviewCommentTag where tagPersonId =: personId or tagGroupId in");
		hqlQuery.append(" (Select t1.adminGroupId from AdminGroup t1 where");
		hqlQuery.append(" t1.roleId IN (Select t2.roleId from PersonRoles t2 where t2.personId = :personId)))");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("personId", AuthenticatedUser.getLoginPersonId());
		return ((Long) query.getSingleResult()).intValue();
	}

	@Override
	public void updateFinacialEntityInDisclosureRelation(Integer disclosureId, Integer coiFinancialEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclosureDetails> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosureDetails.class);
		Root<CoiDisclosureDetails> root = criteriaUpdate.from(CoiDisclosureDetails.class);
		criteriaUpdate.set("coiFinancialEntityId", coiFinancialEntityId);
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("disclosureId"), disclosureId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

}
