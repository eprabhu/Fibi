package com.polus.fibicomp.coi.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Collections;
import java.util.HashMap;

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
import com.polus.fibicomp.coi.dto.COIFinancialEntityDto;
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
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.view.DisclosureView;

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
		hqlQuery.append("SELECT (SELECT COUNT(*) FROM COI_FINANCIAL_ENTITY WHERE PERSON_ID = :personId) as entityCount, ");
		hqlQuery.append("(SELECT COUNT(*) FROM COI_DISCLOSURE C1 INNER JOIN COI_DISCLOSURE_DETAILS C2 ON C2.DISCLOSURE_ID=C1.DISCLOSURE_ID ");
		hqlQuery.append("INNER JOIN COI_FINANCIAL_ENTITY C3 ON C3.COI_FINANCIAL_ENTITY_ID=C2.COI_FINANCIAL_ENTITY_ID ");
		hqlQuery.append("INNER JOIN COI_DISC_DETAILS_COMMENTS C4 ON C4.DISCLOSURE_DETAILS_ID=C2.DISCLOSURE_DETAILS_ID ");
		hqlQuery.append("WHERE C2.DISC_DET_STATUS_CODE IS NOT NULL AND C1.PERSON_ID = :personId ");
		hqlQuery.append("and C2.DISCLOSURE_ID = :disclosureId and C2.MODULE_CODE= :moduleCode and C2.MODULE_ITEM_KEY= :moduleItemId) as disDetCount");
		Query query = session.createNativeQuery(hqlQuery.toString());
		query.setParameter("personId", personId);
		query.setParameter("disclosureId", disclosureId);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("moduleItemId", moduleItemId);
		List<Object[]> countData = query.getResultList();
		if (countData != null && !countData.isEmpty()) {
			Integer entityCount = Integer.parseInt(countData.get(0)[0].toString());
			Integer disDetCount = Integer.parseInt(countData.get(0)[1].toString());
			return  entityCount == 0 || disDetCount == 0 ? null : entityCount > disDetCount ? Boolean.FALSE : Boolean.TRUE;
		}
		return null;
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


	@Override
	public List<Map<Object, Object>> disclosureStatusCount(Integer moduleCode, Integer moduleItemId, Integer disclosureId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT  C2.DISC_DET_STATUS_CODE, COUNT(*) FROM COI_DISCLOSURE C1 INNER JOIN COI_DISCLOSURE_DETAILS C2 ON C2.DISCLOSURE_ID=C1.DISCLOSURE_ID ");
		hqlQuery.append("INNER JOIN COI_FINANCIAL_ENTITY C3 ON C3.COI_FINANCIAL_ENTITY_ID=C2.COI_FINANCIAL_ENTITY_ID ");
		hqlQuery.append("INNER JOIN COI_DISC_DETAILS_COMMENTS C4 ON C4.DISCLOSURE_DETAILS_ID=C2.DISCLOSURE_DETAILS_ID ");
		hqlQuery.append("WHERE C2.DISC_DET_STATUS_CODE IS NOT NULL AND C1.PERSON_ID = :personId ");
		hqlQuery.append("and C2.DISCLOSURE_ID = :disclosureId and C2.MODULE_CODE= :moduleCode and C2.MODULE_ITEM_KEY= :moduleItemId GROUP BY C2.DISC_DET_STATUS_CODE ");
		hqlQuery.append("ORDER BY C2.DISC_DET_STATUS_CODE ASC");
		Query query = session.createNativeQuery(hqlQuery.toString());
		query.setParameter("personId", personId);
		query.setParameter("disclosureId", disclosureId);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("moduleItemId", moduleItemId);
		List<Object[]> countData = query.getResultList();
		if (countData != null && !countData.isEmpty()) {
			List<Map<Object, Object>> countList = new ArrayList<>();
			for (Object[] obj : countData) {
				Map<Object, Object> countObj = new HashMap<>();
				countObj.put(obj[0], obj[1]);
				countList.add(countObj);
			}
			return countList;
		}
		return Collections.emptyList();
	}
	
	@Override
	public COIEntity getCoiEntityDetailsById(Integer coiEntityId) {
		return hibernateTemplate.get(COIEntity.class, coiEntityId);
	}

	@Override
	public List<CoiDisclosure> getActiveDisclosure(String personId) {
		List<CoiDisclosure> coiDisclosures = new ArrayList<>();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<CoiDisclosure> query = builder.createQuery(CoiDisclosure.class);
			Root<CoiDisclosure> rootCoiDisclosure = query.from(CoiDisclosure.class);
			query.where(builder.and(builder.equal(rootCoiDisclosure.get("personId"), personId),
					builder.equal(rootCoiDisclosure.get("disclosureCategoryTypeCode"), 1),
					builder.equal(rootCoiDisclosure.get("disclosureSequenceStatusCode"), 2)));
			CoiDisclosure coiDisclosure = session.createQuery(query).getSingleResult();
			coiDisclosure.setNumberOfSFI(getNumberOfSFIBasedOnDisclosureId(coiDisclosure.getDisclosureId()));
			coiDisclosures.add(coiDisclosure);
		} catch (Exception ex) {
			coiDisclosures = null;
		}
		return coiDisclosures;
	}

	@Override
	public Integer getNumberOfSFIBasedOnDisclosureId(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Long> query = builder.createQuery(Long.class);
		Root<CoiDisclosureDetails> rootCoiDisclosureDetails = query.from(CoiDisclosureDetails.class);
		query.where(builder.equal(rootCoiDisclosureDetails.get("disclosureDetailsId"), disclosureId));
		query.multiselect(builder.countDistinct(rootCoiDisclosureDetails.get("coiFinancialEntityId")));
		Long numberOfSFI = session.createQuery(query).getSingleResult();
		return numberOfSFI.intValue();
	}

	@Override
	public DashBoardProfile getCOIDashboard(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<DisclosureView> disclosureViews = new ArrayList<>();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String personId = vo.getPersonId();
		String filterType = vo.getFilterType();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_DASHBOARD(?,?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, setCOISortOrder(sort));
				statement.setInt(3, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(4, (pageNumber == null ? 0 : pageNumber));
				statement.setString(5, tabName);
				statement.setBoolean(6, isDownload);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, filterType);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, setCOISortOrder(sort));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, tabName);
				statement.setBoolean(7, isDownload);
				statement.setString(8, isAdvancedSearch);
				statement.setString(9, filterType);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				DisclosureView disclosureView =  new DisclosureView();
				disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
				disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
				disclosureView.setDisclosureStatusCode(Integer.parseInt(resultSet.getString("DISCLOSURE_STATUS_CODE")));
				disclosureView.setDisclosureStatus(resultSet.getString("DISCLOSURE_STATUS"));
				disclosureView.setDisclosureDispositionCode(Integer.parseInt(resultSet.getString("DISPOSITION_STATUS_TYPE_CODE")));
				disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
				disclosureView.setSubmittedDate(resultSet.getTimestamp("CERTIFICATION_TIMESTAMP"));
				disclosureView.setReviewStatusTypeCode(resultSet.getString("REVIEW_STATUS_TYPE_CODE"));
				disclosureView.setDisclosureCategoryTypeCode(resultSet.getString("DISCLOSURE_CATEGORY_TYPE_CODE"));
				disclosureView.setDisclosureCategoryType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
				disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
				disclosureView.setLastApprovedVersion(resultSet.getInt("DISCLOSURE_VERSION_NUMBER"));
				disclosureView.setDisclosureSequenceStatusCode(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS_CODE"));
				disclosureView.setDisclosureSequenceStatus(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS"));
				disclosureView.setExpirationDate(resultSet.getTimestamp("EXPIRATION_DATE"));
				disclosureView.setNoOfSfiInActive(resultSet.getInt("NO_OF_SFI"));
				disclosureView.setNoOfProposalInActive(resultSet.getInt("NO_OF_PROPOSAL"));
				disclosureView.setNoOfAwardInActive(resultSet.getInt("NO_OF_AWARD"));
				disclosureView.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
				disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER"));
				disclosureView.setPersonId(resultSet.getString("PERSON_ID"));
				disclosureViews.add(disclosureView);
			}
			dashBoardProfile.setDisclosureViews(disclosureViews);
			dashBoardProfile.setTotalServiceRequest(getCOIDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getCOIDashboard {}", e.getMessage());
		}
		return dashBoardProfile;
	}

	@Override
	public Integer getCOIDashboardCount(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String personId = vo.getPersonId();
		String tabName = vo.getTabName();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Map<String, String> sort = vo.getSort();
		String filterType = vo.getFilterType();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, setCOISortOrder(sort));
				statement.setInt(3, 0);
				statement.setInt(4, 0);
				statement.setString(5, tabName);
				statement.setBoolean(6, true);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, filterType);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_COI_DISCLOSURE_DASHBOARD_COUNT (?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, setCOISortOrder(sort));
				statement.setInt(4, 0);
				statement.setInt(5, 0);
				statement.setString(6, tabName);
				statement.setBoolean(7, true);
				statement.setString(8, isAdvancedSearch);
				statement.setString(9, filterType);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getCOIDashboardCount {}", e.getMessage());
		}
		return count;
	}
	private String setCOISortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("createTimestamp")) {
					sortOrder = (sortOrder == null ? "T.CREATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.CREATE_TIMESTAMP " + mapElement.getValue());
				}
			}
		}
		return sortOrder;
	}

	@Override
	public DashBoardProfile getCOIAdminDashboard(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<DisclosureView> disclosureViews = new ArrayList<>();
		String disclosureId = vo.getProperty1();
		String disclosurePersonId = vo.getProperty2();
		String homeUnit = vo.getProperty3();
		List<String> disclosureStatusCodes = vo.getProperty4();
		List<String> disclosureCategoryTypeCodes = vo.getProperty5();
		String startDate = vo.getProperty6();
		String endDate = vo.getProperty7();
		String entityName = vo.getProperty8();
		String entityCountry = vo.getProperty9();
		String proposalId = vo.getProperty10();
		String title = vo.getProperty11();
		String awardId = vo.getProperty12();
		String projectTypeCode = vo.getProperty14();
		String hasSFIFlag = vo.getProperty15() != null ? (vo.getProperty15().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String personId = AuthenticatedUser.getLoginPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_ADMIN_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(5, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(6, startDate);
				statement.setString(7, endDate);
				statement.setString(8, entityName);
				statement.setString(9, entityCountry);
				statement.setString(10, proposalId);
				statement.setString(11, title);
				statement.setString(12, awardId);
				statement.setString(13, personId);
				statement.setString(14, setCOISortOrder(sort));
				statement.setInt(15, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(16, (pageNumber == null ? 0 : pageNumber));
				statement.setString(17, tabName);
				statement.setBoolean(18, isDownload);
				statement.setString(19, isAdvancedSearch);
				statement.setString(20, projectTypeCode);
				statement.setString(21, hasSFIFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_ADMIN_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, disclosureId);
				statement.setString(3, disclosurePersonId);
				statement.setString(4, homeUnit);
				statement.setString(5, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(6, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(7, startDate);
				statement.setString(8, endDate);
				statement.setString(9, entityName);
				statement.setString(10, entityCountry);
				statement.setString(11, proposalId);
				statement.setString(12, title);
				statement.setString(13, awardId);
				statement.setString(14, personId);
				statement.setString(15, setCOISortOrder(sort));
				statement.setInt(16, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(17, (pageNumber == null ? 0 : pageNumber));
				statement.setString(18, tabName);
				statement.setBoolean(19, isDownload);
				statement.setString(20, isAdvancedSearch);
				statement.setString(21, projectTypeCode);
				statement.setString(22, hasSFIFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				DisclosureView disclosureView =  new DisclosureView();
				disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
				disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
				disclosureView.setDisclosurePersonFullName(resultSet.getString("DISCLOSURE_PERSON_NAME"));
				disclosureView.setDisclosureStatusCode(Integer.parseInt(resultSet.getString("DISCLOSURE_STATUS_CODE")));
				disclosureView.setDisclosureStatus(resultSet.getString("DISCLOSURE_STATUS"));
				disclosureView.setDisclosureDispositionCode(Integer.parseInt(resultSet.getString("DISPOSITION_STATUS_TYPE_CODE")));
				disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
				disclosureView.setDisclosureCategoryTypeCode(resultSet.getString("DISCLOSURE_CATEGORY_TYPE_CODE"));
				disclosureView.setDisclosureCategoryType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
				disclosureView.setReviewStatusTypeCode(resultSet.getString("REVIEW_STATUS_TYPE_CODE"));
				disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
				disclosureView.setLastApprovedVersion(resultSet.getInt("LAST_APPROVED_VERSION"));
				disclosureView.setLastApprovedVersionDate(resultSet.getTimestamp("LAST_APPROVED_DATE"));
				disclosureView.setDisclosureSequenceStatusCode(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS_CODE"));
				disclosureView.setDisclosureSequenceStatus(resultSet.getString("DISCLOSURE_SEQUENCE_STATUS"));
				disclosureView.setNoOfProposalInPending(resultSet.getInt("NO_OF_PENDING_PROPOSALS"));
				disclosureView.setNoOfAwardInPending(resultSet.getInt("NO_OF_PENDING_AWARDS"));
				disclosureView.setNoOfProposalInActive(resultSet.getInt("NO_OF_ACTIVE_PROPOSAL"));
				disclosureView.setNoOfAwardInActive(resultSet.getInt("NO_OF_ACTIVE_AWARD"));
				disclosureView.setNoOfSfiInActive(resultSet.getInt("NO_OF_SFI_IN_ACTIVE"));
				disclosureView.setNoOfSfiInPending(resultSet.getInt("NO_OF_SFI_IN_PENDING"));
				disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER_FULL_NAME"));
				disclosureView.setReviseComment(resultSet.getString("REVISE_COMMENT"));
				disclosureView.setPersonId(resultSet.getString("PERSON_ID"));
				if (tabName.equals("PENDING_DISCLOSURES")) {
					disclosureView.setReviewId(resultSet.getInt("COI_REVIEW_ID"));
					disclosureView.setReviewDescription(resultSet.getString("REVIEW_DESCRIPTION"));
					disclosureView.setReviewerStatusCode(resultSet.getString("REVIEWER_STATUS_CODE"));
					disclosureView.setReviewerStatus(resultSet.getString("REVIEWER_STATUS"));
					disclosureView.setReviewerFullName(resultSet.getString("REVIEWER_NAME"));
				}
				disclosureViews.add(disclosureView);
			}
			dashBoardProfile.setDisclosureViews(disclosureViews);
			dashBoardProfile.setDisclosureCount(getCOIAdminDashboardCount(vo));
		} catch (Exception e) {
			logger.error("Error in getCOIAdminDashboard {}", e.getMessage());
		}
		return dashBoardProfile;
	}

	private Integer getCOIAdminDashboardCount(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		String disclosureId = vo.getProperty1();
		String disclosurePersonId = vo.getProperty2();
		String homeUnit = vo.getProperty3();
		List<String> disclosureStatusCodes = vo.getProperty4();
		List<String> disclosureCategoryTypeCodes = vo.getProperty5();
		String startDate = vo.getProperty6();
		String endDate = vo.getProperty7();
		String entityName = vo.getProperty8();
		String entityCountry = vo.getProperty9();
		String proposalId = vo.getProperty10();
		String title = vo.getProperty11();
		String awardId = vo.getProperty12();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String projectTypeCode = vo.getProperty14();
		String hasSFIFlag = vo.getProperty15() != null ? (vo.getProperty15().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		Map<String, String> sort = vo.getSort();
		String personId = AuthenticatedUser.getLoginPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(5, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(6, startDate);
				statement.setString(7, endDate);
				statement.setString(8, entityName);
				statement.setString(9, entityCountry);
				statement.setString(10, proposalId);
				statement.setString(11, title);
				statement.setString(12, awardId);
				statement.setString(13, personId);
				statement.setString(14, setCOISortOrder(sort));
				statement.setInt(15, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(16, (pageNumber == null ? 0 : pageNumber));
				statement.setString(17, tabName);
				statement.setBoolean(18, true);
				statement.setString(19, isAdvancedSearch);
				statement.setString(20, projectTypeCode);
				statement.setString(21, hasSFIFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, disclosureId);
				statement.setString(3, disclosurePersonId);
				statement.setString(4, homeUnit);
				statement.setString(5, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(6, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(7, startDate);
				statement.setString(8, endDate);
				statement.setString(9, entityName);
				statement.setString(10, entityCountry);
				statement.setString(11, proposalId);
				statement.setString(12, title);
				statement.setString(13, awardId);
				statement.setString(14, personId);
				statement.setString(15, setCOISortOrder(sort));
				statement.setInt(16, 0);
				statement.setInt(17, 0);
				statement.setString(18, tabName);
				statement.setBoolean(19, true);
				statement.setString(20, isAdvancedSearch);
				statement.setString(21, projectTypeCode);
				statement.setString(22, hasSFIFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			logger.error("Error in getCOIAdminDashboardCount {}", e.getMessage());
		}
		return count;
	}

	@Override
	public DashBoardProfile getSFIDashboard(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		List<COIFinancialEntityDto> coiFinancialEntityDtos = new ArrayList<>();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		String entityName = vo.getProperty8();
		String entityId = vo.getProperty16();
		String hasDisclosureFlag = vo.getProperty17() != null ? (vo.getProperty17().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasProposalFlag = vo.getProperty18() != null ? (vo.getProperty18().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasAwardFlag = vo.getProperty19() != null ? (vo.getProperty19().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		Map<String, String> sort = vo.getSort();
		String personId = vo.getPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SFI_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, null);
				statement.setInt(3, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(4, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(5, isDownload);
				statement.setString(6, isAdvancedSearch);
				statement.setString(7, entityName);
				statement.setString(8, entityId);
				statement.setString(9, hasDisclosureFlag);
				statement.setString(10, hasProposalFlag);
				statement.setString(11, hasAwardFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SFI_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, null);
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(6, isDownload);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, entityName);
				statement.setString(9, entityId);
				statement.setString(10, hasDisclosureFlag);
				statement.setString(11, hasProposalFlag);
				statement.setString(12, hasAwardFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				COIFinancialEntityDto coiFinancialEntityDto = new COIFinancialEntityDto();
				coiFinancialEntityDto.setCoiFinancialEntityId(resultSet.getInt("COI_FINANCIAL_ENTITY_ID"));
				coiFinancialEntityDto.setEntityVersionNumber(resultSet.getInt("ENTITY_VERSION_NUMBER"));
				coiFinancialEntityDto.setCoiEntityName(resultSet.getString("COI_ENTITY_NAME"));
				coiFinancialEntityDto.setInvolvementStartDate(resultSet.getDate("INVOLVEMENT_START_DATE"));
				coiFinancialEntityDto.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
				coiFinancialEntityDto.setLastUpdatedOn(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				coiFinancialEntityDto.setNoOfDisclosures(resultSet.getInt("NO_OF_DISCLOSURES"));
				coiFinancialEntityDto.setIsActive(resultSet.getString("IS_ACTIVE"));
				coiFinancialEntityDto.setNoOfProposals(resultSet.getInt("NO_OF_PROPOSALS"));
				coiFinancialEntityDto.setNoOfAwards(resultSet.getInt("NO_OF_AWARDS"));
				coiFinancialEntityDto.setInvolvementEndDate(resultSet.getDate("INVOLVEMENT_END_DATE"));
				coiFinancialEntityDto.setCoiEntityType(resultSet.getString("ENTITY_TYPE"));
				coiFinancialEntityDto.setCoiEntityCountry(resultSet.getString("COUNTRY_NAME"));
				coiFinancialEntityDto.setCoiEntityEmail(resultSet.getString("EMAIL_ADDRESS"));
				coiFinancialEntityDtos.add(coiFinancialEntityDto);
			}
			dashBoardProfile.setCoiFinancialEntityList(coiFinancialEntityDtos);
			dashBoardProfile.setCoiFinancialEntityListCount(getSFIDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getSFIDashboard {}", e.getMessage());
		}
		return dashBoardProfile;
	}

	private Integer getSFIDashboardCount(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer count = 0;
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String entityName = vo.getProperty8();
		String entityId = vo.getProperty16();
		String hasDisclosureFlag = vo.getProperty17() != null ? (vo.getProperty17().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasProposalFlag = vo.getProperty18() != null ? (vo.getProperty18().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasAwardFlag = vo.getProperty19() != null ? (vo.getProperty19().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String personId = vo.getPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SFI_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, null);
				statement.setInt(3, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(4, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(5, isDownload);
				statement.setString(6, isAdvancedSearch);
				statement.setString(7, entityName);
				statement.setString(8, entityId);
				statement.setString(9, hasDisclosureFlag);
				statement.setString(10, hasProposalFlag);
				statement.setString(11, hasAwardFlag);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SFI_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, null);
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setBoolean(6, Boolean.TRUE);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, entityName);
				statement.setString(9, entityId);
				statement.setString(10, hasDisclosureFlag);
				statement.setString(11, hasProposalFlag);
				statement.setString(12, hasAwardFlag);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in getSFIDashboardCount {}", e.getMessage());
		}
		return count;
	}

	@Override
	public List<COIEntity> getAllEntityList(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<COIEntity> outerQuery = builder.createQuery(COIEntity.class);
		Root<COIEntity> coiEntity = outerQuery.from(COIEntity.class);
		Subquery<COIFinancialEntity> subQuery = outerQuery.subquery(COIFinancialEntity.class);
		Root<COIFinancialEntity> coiFinancialEntity = subQuery.from(COIFinancialEntity.class);
		Predicate predicate1 = builder.equal(coiFinancialEntity.get("personId"), vo.getPersonId());
		subQuery.select(coiFinancialEntity.get("coiEntityId")).where(builder.and(predicate1));
		if (vo.getFilterType() == null || vo.getFilterType().isEmpty()) {
			outerQuery.select(coiEntity).where(builder.in(coiEntity.get("coiEntityId")).value(subQuery));
		} else if (vo.getFilterType().equals("active")) {
			outerQuery.select(coiEntity).where(builder.and(builder.in(coiEntity.get("coiEntityId")).value(subQuery),
					builder.equal(coiEntity.get("entityStatusCode"), "1")));
		} else if (vo.getFilterType().equals("inactive")) {
			outerQuery.select(coiEntity).where(builder.and(builder.in(coiEntity.get("coiEntityId")).value(subQuery),
					builder.equal(coiEntity.get("entityStatusCode"), "2")));
		}
		outerQuery.orderBy(builder.asc(coiEntity.get("coiEntityName")));
		return session.createQuery(outerQuery).getResultList();
	}

	@Override
	public void setEntityStatus(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<COIEntity> criteriaUpdate = cb.createCriteriaUpdate(COIEntity.class);
		Root<COIEntity> root = criteriaUpdate.from(COIEntity.class);
		criteriaUpdate.set("isActive", vo.getIsActive());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("coiEntityId"), vo.getCoiEntityId()));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<DisclosureDetailDto> getProjectsBasedOnParams(Integer moduleCode, String personId, Integer disclosureId, String disclosureStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<DisclosureDetailDto> awardDetails = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_DISCLOSURE_RELATIONS(?,?,?)}");
				statement.setInt(1, moduleCode);
				statement.setString(2, personId);
				if (disclosureId == null) {
					statement.setNull(3, Types.INTEGER);
				} else {
					statement.setInt(3, disclosureId);
				}
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_DISCLOSURE_RELATIONS";
				String functionCall = "{call " + procedureName + "(?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, moduleCode);
				statement.setString(3, personId);
				if (disclosureId == null) {
					statement.setNull(4, Types.INTEGER);
				} else {
					statement.setInt(4, disclosureId);
				}
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset != null && rset.next()) {
				DisclosureDetailDto detail = new DisclosureDetailDto();
				if (moduleCode == Constants.AWARD_MODULE_CODE) {
					detail.setModuleCode(Constants.AWARD_MODULE_CODE);
					detail.setModuleItemId(rset.getInt("AWARD_ID"));
					detail.setModuleItemKey(rset.getString("AWARD_NUMBER"));
					detail.setTitle(rset.getString("TITLE"));
					detail.setStartDate(rset.getTimestamp("BEGIN_DATE"));
					detail.setEndDate(rset.getTimestamp("FINAL_EXPIRATION_DATE"));
					detail.setUnitNumber(rset.getString("LEAD_UNIT_NUMBER"));
					detail.setUnitName(rset.getString("UNIT_NAME"));
					detail.setSponsor(rset.getString("SPONSOR_NAME"));
					detail.setPrincipalInvestigator(rset.getString("PI"));
					detail.setModuleStatus(rset.getString("STATUS"));
				}
				else if (moduleCode == Constants.DEV_PROPOSAL_MODULE_CODE) {
					detail.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
					detail.setModuleItemId(rset.getInt("PROPOSAL_ID"));
					detail.setTitle(rset.getString("TITLE"));
					detail.setStartDate(rset.getTimestamp("START_DATE"));
					detail.setEndDate(rset.getTimestamp("END_DATE"));
					detail.setUnitNumber(rset.getString("HOME_UNIT_NUMBER"));
					detail.setUnitName(rset.getString("HOME_UNIT_NAME"));
					detail.setSponsor(rset.getString("SPONSOR_NAME"));
					detail.setPrimeSponsor(rset.getString("PRIME_SPONSOR_NAME"));
					detail.setPrincipalInvestigator(rset.getString("PI"));
					detail.setModuleStatus(rset.getString("STATUS"));
				}
				awardDetails.add(detail);

			}

		} catch (SQLException e) {
			e.printStackTrace();
			logger.error("Exception in getProjectsBasedOnParams: {} ", e.getMessage());
		}
		return awardDetails;
	}

	@Override
	public List<COIEntity> getAllSystemEntityList(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<COIEntity> query = builder.createQuery(COIEntity.class);
		Root<COIEntity> rootEntityName = query.from(COIEntity.class);
		query.orderBy(builder.asc(rootEntityName.get("coiEntityName")));
		return session.createQuery(query).getResultList();
	}

}
