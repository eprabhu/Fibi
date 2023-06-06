package com.polus.fibicomp.coi.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import com.polus.fibicomp.coi.dto.CoiConflictStatusTypeDto;
import com.polus.fibicomp.pojo.Country;
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
import com.polus.fibicomp.coi.pojo.CoiConflictHistory;
import com.polus.fibicomp.coi.pojo.CoiConflictStatusType;
import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiDisclosureFcoiType;
import com.polus.fibicomp.coi.pojo.CoiDispositionStatusType;
import com.polus.fibicomp.coi.pojo.CoiEntity;
import com.polus.fibicomp.coi.pojo.CoiFileData;
import com.polus.fibicomp.coi.pojo.CoiProjConflictStatusType;
import com.polus.fibicomp.coi.pojo.CoiProjectAward;
import com.polus.fibicomp.coi.pojo.CoiProjectProposal;
import com.polus.fibicomp.coi.pojo.CoiProjectType;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewActivity;
import com.polus.fibicomp.coi.pojo.CoiReviewAssigneeHistory;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentTag;
import com.polus.fibicomp.coi.pojo.CoiReviewComments;
import com.polus.fibicomp.coi.pojo.CoiReviewStatusType;
import com.polus.fibicomp.coi.pojo.CoiRiskCategory;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosure;
import com.polus.fibicomp.coi.pojo.CoiTravelDisclosureTraveler;
import com.polus.fibicomp.coi.pojo.CoiTravelerStatusType;
import com.polus.fibicomp.coi.pojo.CoiTravelerType;
import com.polus.fibicomp.coi.pojo.DisclComment;
import com.polus.fibicomp.coi.pojo.EntityRiskCategory;
import com.polus.fibicomp.coi.pojo.EntityStatus;
import com.polus.fibicomp.coi.pojo.EntityType;
import com.polus.fibicomp.coi.pojo.PersonEntity;
import com.polus.fibicomp.coi.pojo.PersonEntityRelType;
import com.polus.fibicomp.coi.pojo.PersonEntityRelationship;
import com.polus.fibicomp.coi.pojo.ValidPersonEntityRelType;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.view.DisclosureView;

import oracle.jdbc.OracleTypes;

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
	public CoiDisclosure loadDisclosure (Integer disclosureId) {
		return hibernateTemplate.get(CoiDisclosure.class, disclosureId);
	}

	@Override
	public Integer numberOfSFI (String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<PersonEntity> rootCOIFinancialEntity = query.from(PersonEntity.class);
		query.where(builder.equal(rootCOIFinancialEntity.get("personId"), personId));
		query.select(rootCOIFinancialEntity.get("personEntityId"));
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
	public List<PersonEntity> fetchCOIFinancialEntityByPersonId(String personId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<PersonEntity> criteria = builder.createQuery(PersonEntity.class);
			Root<PersonEntity> root = criteria.from(PersonEntity.class);
			Predicate predicatePersonId = builder.equal(root.get("personId"),personId);
			criteria.where(builder.and(predicatePersonId));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<PersonEntity> getSFIOfDisclosure(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<PersonEntity> query = builder.createQuery(PersonEntity.class);
			Root<PersonEntity> rootCOIFinancialEntity = query.from(PersonEntity.class);
			query.where(builder.equal(rootCOIFinancialEntity.get("personId"), personId));
			query.orderBy(builder.desc(rootCOIFinancialEntity.get("updateTimestamp")));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}
	
	@Override
	public List<CoiEntity> searchEnitiy(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiEntity> query = builder.createQuery(CoiEntity.class);
		Root<CoiEntity> rootEntityName = query.from(CoiEntity.class);
		query.where(builder.like(builder.lower(rootEntityName.get("entityName")), "%" + searchString.toLowerCase() + "%"));
		query.orderBy(builder.asc(rootEntityName.get("entityName")));
		return session.createQuery(query).setMaxResults(50).list();
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
	public List<PersonEntityRelType> fetchPersonEntityRelType() {
		return hibernateTemplate.loadAll(PersonEntityRelType.class);
	}

	@Override
	public List<CoiConflictStatusType> getCoiConflictStatusTypes() {
		return hibernateTemplate.loadAll(CoiConflictStatusType.class);
	}

	@Override
	public PersonEntity getSFIDetails(Integer personEntityId) {
		return hibernateTemplate.get(PersonEntity.class, personEntityId);
	}

	@Override
	public PersonEntity saveOrUpdateCoiSFI(PersonEntity personEntity) {
		hibernateTemplate.saveOrUpdate(personEntity);
		return personEntity;
	}

	@Override
	public CoiEntity saveOrUpdateCoiEntity(CoiEntity CoiEntity) {
		hibernateTemplate.saveOrUpdate(CoiEntity);
		return CoiEntity;
	}

	@Override
	public List<PersonEntityRelationship> getCoiFinancialEntityDetails(Integer coiFinancialEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonEntityRelationship> query = builder.createQuery(PersonEntityRelationship.class);
		Root<PersonEntityRelationship> rootCOIFinancialEntity = query.from(PersonEntityRelationship.class);
		query.where(builder.equal(rootCOIFinancialEntity.get("personEntityId"), coiFinancialEntityId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public PersonEntityRelationship saveOrUpdatePersonEntityRelationship(PersonEntityRelationship personEntityRelationship) {
		hibernateTemplate.saveOrUpdate(personEntityRelationship);
		return personEntityRelationship;
	}

	@Override
	public void certifyDisclosure(CoiDisclosure coiDisclosure) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclosure> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosure.class);
		Root<CoiDisclosure> root = criteriaUpdate.from(CoiDisclosure.class);
		criteriaUpdate.set("certificationText", coiDisclosure.getCertificationText());
		criteriaUpdate.set("certifiedAt", coiDisclosure.getCertifiedAt());
		criteriaUpdate.set("certifiedBy", coiDisclosure.getCertifiedBy());
		criteriaUpdate.set("conflictStatusCode", coiDisclosure.getConflictStatusCode());
		criteriaUpdate.set("dispositionStatusCode", coiDisclosure.getDispositionStatusCode());
		criteriaUpdate.set("reviewStatusCode", coiDisclosure.getReviewStatusCode());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("expirationDate",coiDisclosure.getExpirationDate());
		criteriaUpdate.where(cb.equal(root.get("disclosureId"), coiDisclosure.getDisclosureId()));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<CoiDisclEntProjDetails> getProjectRelationshipByParam(Integer moduleCode, Integer moduleItemId, String loginPersonId, Integer disclosureId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<CoiDisclEntProjDetails> criteria = builder.createQuery(CoiDisclEntProjDetails.class);
			Root<CoiDisclEntProjDetails> root = criteria.from(CoiDisclEntProjDetails.class);
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
	public CoiDisclEntProjDetails saveOrUpdateCoiDisclEntProjDetails(CoiDisclEntProjDetails entityProjectRelation) {
		hibernateTemplate.saveOrUpdate(entityProjectRelation);
		return entityProjectRelation;
	}

	@Override
	public Boolean checkIsSFICompletedForProject(Integer moduleCode, Integer moduleItemId, Integer disclosureId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT (SELECT COUNT(*) FROM COI_DISCLOSURE C1 INNER JOIN COI_DISCL_ENT_PROJ_DETAILS C2 ON C2.DISCLOSURE_ID=C1.DISCLOSURE_ID ");
		hqlQuery.append("WHERE C1.PERSON_ID = :personId and C2.DISCLOSURE_ID = :disclosureId and C2.MODULE_CODE= :moduleCode and C2.MODULE_ITEM_KEY= :moduleItemId ) as entityCount, ");
		hqlQuery.append("(SELECT COUNT(*) FROM COI_DISCLOSURE C1 INNER JOIN COI_DISCL_ENT_PROJ_DETAILS C2 ON C2.DISCLOSURE_ID=C1.DISCLOSURE_ID ");
		hqlQuery.append("INNER JOIN PERSON_ENTITY C3 ON C3.PERSON_ENTITY_ID=C2.PERSON_ENTITY_ID ");
//		hqlQuery.append("INNER JOIN COI_DISC_DETAILS_COMMENTS C4 ON C4.DISCLOSURE_DETAILS_ID=C2.DISCLOSURE_DETAILS_ID ");
		hqlQuery.append("WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NOT NULL AND C1.PERSON_ID = :personId ");
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
	public List<PersonEntity> getSFIBasedOnDisclosureId(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT DISTINCT C3 FROM CoiDisclEntProjDetails C2  ");
		hqlQuery.append("INNER JOIN PersonEntity C3 ON C3.personEntityId=C2.personEntityId ");
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
			logger.error("Exception on evaluateDisclosureQuestionnaire {}", e.getMessage());
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
	public List<Integer> getDisclosureIdsByCOIFinancialEntityId(Integer personEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT distinct(disclosureId) FROM CoiDisclEntProjDetails WHERE personEntityId=:personEntityId ";
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("personEntityId", personEntityId);
		return query.getResultList();
	}

	@Override
	public List<CoiDisclosure> getActiveAndPendingCoiDisclosureDetailsByDisclosureIdsAndSequenceStatus(List<Integer> disclosureIds, List<String> statusCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiDisclosure> queryCoiDisclosureOld = builder.createQuery(CoiDisclosure.class);
		Root<CoiDisclosure> rootCoiDisclosureOld = queryCoiDisclosureOld.from(CoiDisclosure.class);
		Predicate predicate1 = rootCoiDisclosureOld.get("disclosureId").in(disclosureIds);
		Predicate predicate2 = rootCoiDisclosureOld.get("versionStatus").in(statusCodes);
		queryCoiDisclosureOld.where(builder.and(predicate1,predicate2));
		return session.createQuery(queryCoiDisclosureOld).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getModuleItemKeysByCOIFinancialEntityIdAndModuleCode(Integer coiFinancialEntityId, Integer moduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT distinct(moduleItemKey) FROM CoiDisclEntProjDetails WHERE personEntityId=:personEntityId and moduleCode=:moduleCode ";
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("personEntityId", coiFinancialEntityId);
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
	public CoiReviewStatusType getReviewStatus(String reviewStatusTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiReviewStatusType> query = builder.createQuery(CoiReviewStatusType.class);
		Root<CoiReviewStatusType> rootCoiReview = query.from(CoiReviewStatusType.class);
		query.where(builder.equal(rootCoiReview.get("reviewStatusCode"), reviewStatusTypeCode));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public CoiReviewAssigneeHistory saveOrUpdateCoiReviewAssigneeHistory(CoiReviewAssigneeHistory coiReviewAssigneeHistory) {
		hibernateTemplate.saveOrUpdate(coiReviewAssigneeHistory);
		return coiReviewAssigneeHistory;
	}

	@Override
	public CoiConflictStatusType getDisclosureStatusByCode(String disclosureStatusCode) {
		return hibernateTemplate.get(CoiConflictStatusType.class, disclosureStatusCode);
	}

	@Override
	public CoiDispositionStatusType getDispositionStatusByCode(String dispositionStatusTypeCode) {
		return hibernateTemplate.get(CoiDispositionStatusType.class, dispositionStatusTypeCode);
	}

	@Override
	public Integer getSFICountBasedOnParams(String disclosureStatusCode, String personId, Integer disclosureId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		if (Constants.DISCLOSURE_STATUS_PENDING.equals(disclosureStatusCode)) {
			hqlQuery.append("select count(*) from PersonEntity where personId=:personId");
			Query query = session.createQuery(hqlQuery.toString());
			query.setParameter("personId", personId);
			return Integer.parseInt(query.getSingleResult().toString());
		} else {
			hqlQuery.append("SELECT COUNT(DISTINCT C3) FROM CoiDisclosure C1 INNER JOIN CoiDisclEntProjDetails C2 ON C2.disclosureId=C1.disclosureId ");
			hqlQuery.append("INNER JOIN PersonEntity C3 ON C3.personEntityId=C2.personEntityId ");
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
	public CoiDisclEntProjDetails getProjectRelationship(Integer disclosureDetailsId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiDisclEntProjDetails> query = builder.createQuery(CoiDisclEntProjDetails.class);
		Root<CoiDisclEntProjDetails> root = query.from(CoiDisclEntProjDetails.class);
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
		Predicate predicate2 = builder.notEqual(root.get("reviewStatusTypeCode"), "4");
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
		criteriaUpdate.set("dispositionStatusCode", coiDisclosure.getDispositionStatusCode());
		criteriaUpdate.set("reviewStatusCode", coiDisclosure.getReviewStatusCode());
		criteriaUpdate.set("versionStatus", coiDisclosure.getVersionStatus());
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
	public void addReviewerStatus(CoiDisclEntProjDetails disclEntProjDetails) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclEntProjDetails> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclEntProjDetails.class);
		Root<CoiDisclEntProjDetails> root = criteriaUpdate.from(CoiDisclEntProjDetails.class);
//		criteriaUpdate.set("coiReviewerStatusCode", disclEntProjDetails.getCoiReviewerStatusCode());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("disclosureDetailsId"), disclEntProjDetails.getDisclosureDetailsId()));
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
		hqlQuery.append("select distinct moduleItemKey from CoiDisclosureOldDetails where disclosureId =:disclosureId and moduleCode = 3");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("disclosureId", disclosureId);
		return query.getSingleResult().toString();
	}

//	@Override
//	public CoiDisclosureCategoryType getDisclosureCategoryTypeByCode(String disclosureCategoryTypeCode) {
//		return hibernateTemplate.get(CoiDisclosureOldCategoryType.class, disclosureCategoryTypeCode);
//	}

	@Override
	public List<CoiDisclosure> getCoiDisclosuresByDisclosureNumber(Integer disclosureNumber) {
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
	public void updateFinacialEntityInDisclosureRelation(Integer disclosureId, Integer personEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclEntProjDetails> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclEntProjDetails.class);
		Root<CoiDisclEntProjDetails> root = criteriaUpdate.from(CoiDisclEntProjDetails.class);
		criteriaUpdate.set("personEntityId", personEntityId);
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("disclosureId"), disclosureId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}


	@Override
	public List<Map<Object, Object>> disclosureStatusCount(Integer moduleCode, Integer moduleItemId, Integer disclosureId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT  C2.PROJECT_CONFLICT_STATUS_CODE, COUNT(*) FROM COI_DISCLOSURE C1 INNER JOIN COI_DISCL_ENT_PROJ_DETAILS C2 ON C2.DISCLOSURE_ID=C1.DISCLOSURE_ID ");
		hqlQuery.append("INNER JOIN PERSON_ENTITY C3 ON C3.PERSON_ENTITY_ID=C2.PERSON_ENTITY_ID ");
//		hqlQuery.append("INNER JOIN COI_DISC_DETAILS_COMMENTS C4 ON C4.DISCLOSURE_DETAILS_ID=C2.DISCLOSURE_DETAILS_ID ");
		hqlQuery.append("WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NOT NULL AND C1.PERSON_ID = :personId ");
		hqlQuery.append("and C2.DISCLOSURE_ID = :disclosureId and C2.MODULE_CODE= :moduleCode and C2.MODULE_ITEM_KEY= :moduleItemId GROUP BY C2.PROJECT_CONFLICT_STATUS_CODE ");
		hqlQuery.append("ORDER BY C2.PROJECT_CONFLICT_STATUS_CODE ASC");
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
	public CoiEntity getCoiEntityDetailsById(Integer coiEntityId) {
		return hibernateTemplate.get(CoiEntity.class, coiEntityId);
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
					builder.equal(rootCoiDisclosure.get("fcoiTypeCode"), "1"),
					builder.equal(rootCoiDisclosure.get("versionStatus"), "Active")));
			List<CoiDisclosure> disclData = session.createQuery(query).getResultList();
			if (disclData != null && !disclData.isEmpty()) {
				CoiDisclosure coiDisclosure = disclData.get(0);
				coiDisclosure.setUpdateUserFullName(coiDisclosure.getPerson().getFullName());
				coiDisclosure.setNumberOfSFI(getNumberOfSFIBasedOnDisclosureId(coiDisclosure.getDisclosureId()));
				coiDisclosure.setNumberOfProposals(getNumberOfProposalsBasedOnDisclosureId(coiDisclosure.getDisclosureId()));
				coiDisclosure.setNumberOfAwards(getNumberOfAwardsBasedOnDisclosureId(coiDisclosure.getDisclosureId()));
				coiDisclosures.add(coiDisclosure);
			}
			CoiDisclosure coiDisclosure = getPendingFCOIDisclosure(personId);
			if (coiDisclosure != null) {
				coiDisclosures.add(coiDisclosure);
			}
		} catch (Exception ex) {
			throw new ApplicationException("Unable to fetch Active Disclosure", ex, Constants.JAVA_ERROR);
		}
		return coiDisclosures;
	}

	private CoiDisclosure getPendingFCOIDisclosure(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<CoiDisclosure> query = builder.createQuery(CoiDisclosure.class);
			Root<CoiDisclosure> rootCoiDisclosure = query.from(CoiDisclosure.class);
			query.where(builder.and(builder.equal(rootCoiDisclosure.get("personId"), personId),
					builder.equal(rootCoiDisclosure.get("fcoiTypeCode"), "1"),
					builder.equal(rootCoiDisclosure.get("versionStatus"), "Pending")));
			return session.createQuery(query).getSingleResult();
		} catch(Exception e) {
			logger.error("Exception in getPendingFCOIDisclosure {}", e.getMessage());
		}
		return null;
	}

	@Override
	public Integer getNumberOfSFIBasedOnDisclosureId(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Long> query = builder.createQuery(Long.class);
		Root<CoiDisclEntProjDetails> rootCoiDisclosureDetails = query.from(CoiDisclEntProjDetails.class);
		query.where(builder.equal(rootCoiDisclosureDetails.get("disclosureId"), disclosureId));
		query.multiselect(builder.countDistinct(rootCoiDisclosureDetails.get("personEntityId")));
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
				if (tabName.equals("TRAVEL_DISCLOSURES")) {
					DisclosureView disclosureView =  new DisclosureView();
					disclosureView.setTravelDisclosureId(resultSet.getInt("TRAVEL_DISCLOSURE_ID"));
					disclosureView.setTravelStartDate(resultSet.getDate("TRAVEL_START_DATE"));
					disclosureView.setTravelEndDate(resultSet.getDate("TRAVEL_END_DATE"));
					disclosureView.setTravelSubmissionDate(resultSet.getDate("SUBMISSION_DATE"));
					disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER"));
					disclosureView.setAcknowledgeBy(resultSet.getString("ACKNOWLEDGE_BY"));
					disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
					disclosureView.setTravelEntityName(resultSet.getString("TRAVEL_ENTITY_NAME"));
					disclosureView.setTravellerName(resultSet.getString("TRAVELLER_NAME"));
					disclosureView.setDisclosurestatus(resultSet.getString("TRAVEL_DISCLOSURE_STATUS"));
					disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
					disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
					disclosureView.setVersionStatus(resultSet.getString("VERSION_STATUS"));
					disclosureViews.add(disclosureView);
				} else {
					DisclosureView disclosureView =  new DisclosureView();
					disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
					disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
					disclosureView.setConflictStatusCode(resultSet.getString("CONFLICT_STATUS_CODE"));
					disclosureView.setConflictStatus(resultSet.getString("DISCLOSURE_STATUS"));
					disclosureView.setDispositionStatusCode(resultSet.getString("DISPOSITION_STATUS_CODE"));
					disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
					disclosureView.setCertifiedAt(resultSet.getTimestamp("CERTIFIED_AT"));
					disclosureView.setReviewStatusCode(resultSet.getString("REVIEW_STATUS_CODE"));
					disclosureView.setFcoiTypeCode(resultSet.getString("FCOI_TYPE_CODE"));
					disclosureView.setFcoiType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
					disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
					disclosureView.setLastApprovedVersion(resultSet.getInt("VERSION_NUMBER")); // TODO procedure change
					disclosureView.setVersionStatus(resultSet.getString("VERSION_STATUS"));
					disclosureView.setExpirationDate(resultSet.getTimestamp("EXPIRATION_DATE"));
//					disclosureView.setNoOfSfiInActive(resultSet.getInt("NO_OF_SFI"));
//					disclosureView.setNoOfProposalInActive(resultSet.getInt("NO_OF_PROPOSAL"));
//					disclosureView.setNoOfAwardInActive(resultSet.getInt("NO_OF_AWARD"));
					disclosureView.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
					disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
					disclosureView.setDisclosurePersonFullName(resultSet.getString("DISCLOSURE_PERSON_FULL_NAME"));
					disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER"));
					disclosureView.setUpdateUserFullName(resultSet.getString("UPDATE_USER_FULL_NAME"));
					disclosureView.setCreateUser(resultSet.getString("CREATE_USER"));
//					disclosureView.setPersonId(resultSet.getString("PERSON_ID"));
					disclosureView.setNoOfSfi(resultSet.getInt("NO_OF_SFI"));
					disclosureView.setNoOfProposal(resultSet.getInt("NO_OF_PROPOSAL"));
					disclosureView.setNoOfAward(resultSet.getInt("NO_OF_AWARD"));
					disclosureView.setProposalTitle(resultSet.getString("PROPOSAL_TITLES"));
					disclosureView.setProposalId(resultSet.getString("PROPOSAL_IDS"));
					disclosureView.setAwardId(resultSet.getString("AWARD_IDS"));
					disclosureView.setAwardTitle(resultSet.getString("AWARD_TITLES"));
					Unit unit = new Unit();
					unit.setUnitNumber(resultSet.getString("UNIT"));
					unit.setUnitName(resultSet.getString("UNIT_NAME"));
					unit.setOrganizationId(resultSet.getString("ORGANIZATION_ID"));
					unit.setParentUnitNumber(resultSet.getString("PARENT_UNIT_NUMBER"));
					unit.setAcronym(resultSet.getString("ACRONYM"));
					unit.setIsFundingUnit(resultSet.getString("IS_FUNDING_UNIT"));
					disclosureView.setUnit(unit);
					disclosureViews.add(disclosureView);
				}
			}
			dashBoardProfile.setDisclosureViews(disclosureViews);
			dashBoardProfile.setTotalServiceRequest(getCOIDashboardCount(vo));
		} catch (SQLException e) {
			e.printStackTrace();
			logger.error("Error in getCOIDashboard {}", e.getMessage());
			throw new ApplicationException("Unable to fetch data", e, Constants.JAVA_ERROR);
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
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_DASHBOARD_COUNT(?,?,?,?,?,?,?,?)}");
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
			throw new ApplicationException("Error in getCOIDashboardCount", e, Constants.JAVA_ERROR);
		}
		return count;
	}
	private String setCOISortOrder(Map<String, String> sort) {
		String sortOrder = null;
		if (!sort.isEmpty()) {
			for (Map.Entry<String, String> mapElement : sort.entrySet()) {
				if (mapElement.getKey().equals("createTimestamp")) {
					sortOrder = (sortOrder == null ? "T.CREATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.CREATE_TIMESTAMP " + mapElement.getValue());
				} else if (mapElement.getKey().equals("coiDisclosureNumber")) {
					sortOrder = (sortOrder == null ? "T.DISCLOSURE_ID " + mapElement.getValue() : sortOrder + ", T.DISCLOSURE_ID " + mapElement.getValue());
				} else if (mapElement.getKey().equals("disclosurePersonFullName")) {
					sortOrder = (sortOrder == null ? "T.DISCLOSURE_PERSON_NAME " + mapElement.getValue() : sortOrder + ", T.DISCLOSURE_PERSON_NAME " + mapElement.getValue());
				} else if (mapElement.getKey().equals("disclosureCategoryType")) {
					sortOrder = (sortOrder == null ? "T.DISCLOSURE_CATEGORY_TYPE " + mapElement.getValue() : sortOrder + ", T.DISCLOSURE_CATEGORY_TYPE " + mapElement.getValue());
				} else if (mapElement.getKey().equals("disclosureStatus")) {
					sortOrder = (sortOrder == null ? "T.DISCLOSURE_STATUS " + mapElement.getValue() : sortOrder + ", T.DISCLOSURE_STATUS " + mapElement.getValue());
				} else if (mapElement.getKey().equals("expirationDate")) {
					sortOrder = (sortOrder == null ? "T.EXPIRATION_DATE " + mapElement.getValue() : sortOrder + ", T.EXPIRATION_DATE " + mapElement.getValue());
				} else if (mapElement.getKey().equals("updateTimeStamp")) {
					sortOrder = (sortOrder == null ? "T.UPDATE_TIMESTAMP " + mapElement.getValue() : sortOrder + ", T.UPDATE_TIMESTAMP " + mapElement.getValue());
				} else if (mapElement.getKey().equals("name")) {
					sortOrder = (sortOrder == null ? "T.ENTITY_NAME " + mapElement.getValue() : sortOrder + ", T.ENTITY_NAME " + mapElement.getValue());
				} else if (mapElement.getKey().equals("country")) {
					sortOrder = (sortOrder == null ? "T.COUNTRY " + mapElement.getValue() : sortOrder + ", T.COUNTRY " + mapElement.getValue());
				} else if (mapElement.getKey().equals("entityType")) {
					sortOrder = (sortOrder == null ? "T.ENTITY_TYPE " + mapElement.getValue() : sortOrder + ",T.ENTITY_TYPE " + mapElement.getValue());
				} else if (mapElement.getKey().equals("riskLevel")) {
					sortOrder = (sortOrder == null ? "T.RISK_LEVEL " + mapElement.getValue() : sortOrder + ", T.RISK_LEVEL " + mapElement.getValue());
				} else if (mapElement.getKey().equals("certificationDate")) {
				sortOrder = (sortOrder == null ? "T.CERTIFIED_AT " + mapElement.getValue() : sortOrder + ", T.CERTIFIED_AT " + mapElement.getValue());
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
		List<String> dispositionStatusCodes = vo.getProperty20();
		List<String> reviewStatusCodes = vo.getProperty21();
		List<String> conflictStatusCodes = vo.getProperty4();
		String certificationDate = vo.getProperty23();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_ADMIN_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, conflictStatusCodes != null && !conflictStatusCodes.isEmpty() ? String.join(",", conflictStatusCodes) : null);
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
				statement.setString(22, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(23, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.setString(24, certificationDate);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_ADMIN_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, disclosureId);
				statement.setString(3, disclosurePersonId);
				statement.setString(4, homeUnit);
				statement.setString(5, conflictStatusCodes != null && !conflictStatusCodes.isEmpty() ? String.join(",", conflictStatusCodes) : null);
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
				statement.setString(23, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(24, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.setString(25, certificationDate);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				DisclosureView disclosureView =  new DisclosureView();
				disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
				disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
				disclosureView.setDisclosurePersonFullName(resultSet.getString("DISCLOSURE_PERSON_NAME"));
				disclosureView.setConflictStatusCode(resultSet.getString("CONFLICT_STATUS_CODE"));
				disclosureView.setConflictStatus(resultSet.getString("DISCLOSURE_STATUS"));
				disclosureView.setDispositionStatusCode(resultSet.getString("DISPOSITION_STATUS_CODE"));
				disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
				disclosureView.setFcoiTypeCode(resultSet.getString("FCOI_TYPE_CODE"));
				disclosureView.setFcoiType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
				disclosureView.setReviewStatusCode(resultSet.getString("REVIEW_STATUS_CODE"));
				disclosureView.setReviewStatus(resultSet.getString("EXPIRATION_DATE"));
				disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
				disclosureView.setLastApprovedVersion(resultSet.getInt("LAST_APPROVED_VERSION"));
				disclosureView.setLastApprovedVersionDate(resultSet.getTimestamp("LAST_APPROVED_DATE"));
				disclosureView.setVersionStatus(resultSet.getString("VERSION_STATUS"));
				disclosureView.setDisclosureVersionNumber(resultSet.getInt("VERSION_NUMBER"));
//				disclosureView.setNoOfProposalInPending(resultSet.getInt("NO_OF_PENDING_PROPOSALS"));
//				disclosureView.setNoOfAwardInPending(resultSet.getInt("NO_OF_PENDING_AWARDS"));
				disclosureView.setNoOfProposalInActive(resultSet.getInt("NO_OF_ACTIVE_PROPOSAL"));
				disclosureView.setNoOfAwardInActive(resultSet.getInt("NO_OF_ACTIVE_AWARD"));
				disclosureView.setNoOfSfiInActive(resultSet.getInt("NO_OF_SFI_IN_ACTIVE"));
				disclosureView.setNoOfSfiInPending(resultSet.getInt("NO_OF_SFI_IN_PENDING"));
				disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER_FULL_NAME"));
				disclosureView.setReviseComment(resultSet.getString("REVISION_COMMENT"));
				disclosureView.setPersonId(resultSet.getString("PERSON_ID"));
				disclosureView.setExpirationDate(resultSet.getTimestamp("EXPIRATION_DATE"));
				disclosureView.setCertifiedAt(resultSet.getTimestamp("CERTIFIED_AT"));
				disclosureView.setProposalTitle(resultSet.getString("PROPOSAL_TITLES"));
				disclosureView.setProposalId(resultSet.getString("PROPOSAL_IDS"));
				disclosureView.setAwardId(resultSet.getString("AWARD_NUMBERS"));
				disclosureView.setAwardTitle(resultSet.getString("AWARD_TITLES"));
				disclosureView.setHomeUnit(resultSet.getString("HOME_UNIT"));
				disclosureView.setHomeUnitName(resultSet.getString("HOME_UNIT_NAME"));
				disclosureView.setAdminGroupName(resultSet.getString("ADMIN_GROUP_NAME"));
				disclosureView.setAdministrator(resultSet.getString("ADMINISTRATOR"));

//				if (tabName.equals("PENDING_DISCLOSURES")) {
//					disclosureView.setReviewId(resultSet.getInt("COI_REVIEW_ID"));
//					disclosureView.setReviewDescription(resultSet.getString("REVIEW_DESCRIPTION"));
//					disclosureView.setReviewerStatusCode(resultSet.getString("REVIEWER_STATUS_CODE"));
//					disclosureView.setReviewerStatus(resultSet.getString("REVIEWER_STATUS"));
//					disclosureView.setReviewerFullName(resultSet.getString("REVIEWER_NAME"));
//				}
				disclosureViews.add(disclosureView);
			}
			dashBoardProfile.setDisclosureViews(disclosureViews);
			dashBoardProfile.setDisclosureCount(getCOIAdminDashboardCount(vo));
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in getCOIAdminDashboard {}", e.getMessage());
			throw new ApplicationException("Error in getCOIAdminDashboard {}", e, Constants.JAVA_ERROR);
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
		List<String> conflictStatusCode = vo.getProperty4();
		List<String> disclosureCategoryTypeCodes = vo.getProperty5();
		String startDate = vo.getProperty6();
		String endDate = vo.getProperty7();
		String entityName = vo.getProperty8();
		String entityCountry = vo.getProperty9();
		String proposalId = vo.getProperty10();
		String title = vo.getProperty11();
		String awardId = vo.getProperty12();
		String tabName = vo.getTabName();
		String isAdvancedSearch = vo.getAdvancedSearch();
		String projectTypeCode = vo.getProperty14();
		String hasSFIFlag = vo.getProperty15() != null ? (vo.getProperty15().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		Map<String, String> sort = vo.getSort();
		List<String> dispositionStatusCodes = vo.getProperty20();
		List<String> reviewStatusCodes = vo.getProperty21();
		String personId = AuthenticatedUser.getLoginPersonId();
		String certificationDate = vo.getProperty23();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, conflictStatusCode != null && !conflictStatusCode.isEmpty() ? String.join(",", conflictStatusCode) : null);
				statement.setString(5, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(6, startDate);
				statement.setString(7, endDate);
				statement.setString(8, entityName);
				statement.setString(9, entityCountry);
				statement.setString(10, proposalId);
				statement.setString(11, title);
				statement.setString(12, awardId);
				statement.setString(13, personId);
				statement.setString(14, null);
				statement.setInt(15, 0);
				statement.setInt(16, 0);
				statement.setString(17, tabName);
				statement.setBoolean(18, true);
				statement.setString(19, isAdvancedSearch);
				statement.setString(20, projectTypeCode);
				statement.setString(21, hasSFIFlag);
				statement.setString(22, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(23, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.setString(24, certificationDate);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, disclosureId);
				statement.setString(3, disclosurePersonId);
				statement.setString(4, homeUnit);
				statement.setString(5, conflictStatusCode != null && !conflictStatusCode.isEmpty() ? String.join(",", conflictStatusCode) : null);
				statement.setString(6, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(7, startDate);
				statement.setString(8, endDate);
				statement.setString(9, entityName);
				statement.setString(10, entityCountry);
				statement.setString(11, proposalId);
				statement.setString(12, title);
				statement.setString(13, awardId);
				statement.setString(14, personId);
				statement.setString(15, null);
				statement.setInt(16, 0);
				statement.setInt(17, 0);
				statement.setString(18, tabName);
				statement.setBoolean(19, true);
				statement.setString(20, isAdvancedSearch);
				statement.setString(21, projectTypeCode);
				statement.setString(22, hasSFIFlag);
				statement.setString(23, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(24, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.setString(25, certificationDate);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			logger.error("Error in getCOIAdminDashboardCount {}", e.getMessage());
			throw new ApplicationException("Unable to fetch data {}", e, Constants.DB_PROC_ERROR);
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
		String filterType = vo.getFilterType();
		String personId = vo.getPersonId();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SFI_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?)}");
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
				statement.setString(12, filterType);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SFI_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?)}";
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
				statement.setString(13, filterType);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				COIFinancialEntityDto coiFinancialEntityDto = new COIFinancialEntityDto();
				coiFinancialEntityDto.setCoiFinancialEntityId(resultSet.getInt("PERSON_ENTITY_ID"));
				coiFinancialEntityDto.setEntityVersionNumber(resultSet.getInt("VERSION_NUMBER"));
				coiFinancialEntityDto.setCoiEntityName(resultSet.getString("ENTITY_NAME"));
				coiFinancialEntityDto.setInvolvementStartDate(resultSet.getDate("INVOLVEMENT_START_DATE"));
				coiFinancialEntityDto.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
				coiFinancialEntityDto.setLastUpdatedOn(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				coiFinancialEntityDto.setNoOfDisclosures(resultSet.getInt("NO_OF_DISCLOSURES"));
				coiFinancialEntityDto.setIsActive(resultSet.getString("IS_RELATIONSHIP_ACTIVE"));
				coiFinancialEntityDto.setNoOfProposals(resultSet.getInt("NO_OF_PROPOSALS"));
				coiFinancialEntityDto.setNoOfAwards(resultSet.getInt("NO_OF_AWARDS"));
				coiFinancialEntityDto.setInvolvementEndDate(resultSet.getDate("INVOLVEMENT_END_DATE"));
				coiFinancialEntityDto.setCoiEntityType(resultSet.getString("ENTITY_TYPE"));
				coiFinancialEntityDto.setCoiEntityCountry(resultSet.getString("COUNTRY_NAME"));
				coiFinancialEntityDto.setCoiEntityEmail(resultSet.getString("EMAIL_ADDRESS"));
				coiFinancialEntityDto.setRelationshipTypes(resultSet.getString("RELATIONSHIP_TYPES"));
				coiFinancialEntityDto.setVersionStatus(resultSet.getString("VERSION_STATUS"));
				coiFinancialEntityDtos.add(coiFinancialEntityDto);
			}
			dashBoardProfile.setCoiFinancialEntityList(coiFinancialEntityDtos);
			dashBoardProfile.setCoiFinancialEntityListCount(getSFIDashboardCount(vo));
		} catch (SQLException e) {
			logger.error("Error in getSFIDashboard {}", e.getMessage());
			throw new ApplicationException("Unable to fetch data", e, Constants.JAVA_ERROR);
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
		String isAdvancedSearch = vo.getAdvancedSearch();
		String entityName = vo.getProperty8();
		String entityId = vo.getProperty16();
		String hasDisclosureFlag = vo.getProperty17() != null ? (vo.getProperty17().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasProposalFlag = vo.getProperty18() != null ? (vo.getProperty18().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String hasAwardFlag = vo.getProperty19() != null ? (vo.getProperty19().equals(Boolean.TRUE) ? "YES" : "NO" ) : null;
		String personId = vo.getPersonId();
		String filterType = vo.getFilterType();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SFI_DASHBOARD_COUNT(?,?,?,?,?,?,?,?)}");
				statement.setString(1, personId);
				statement.setString(2, isAdvancedSearch);
				statement.setString(3, entityName);
				statement.setString(4, entityId);
				statement.setString(5, hasDisclosureFlag);
				statement.setString(6, hasProposalFlag);
				statement.setString(7, hasAwardFlag);
				statement.setString(8, filterType);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SFI_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setString(3, isAdvancedSearch);
				statement.setString(4, entityName);
				statement.setString(5, entityId);
				statement.setString(6, hasDisclosureFlag);
				statement.setString(7, hasProposalFlag);
				statement.setString(8, hasAwardFlag);
				statement.setString(9, filterType);
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
	public List<CoiEntity> getAllEntityList(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiEntity> outerQuery = builder.createQuery(CoiEntity.class);
		Root<CoiEntity> coiEntity = outerQuery.from(CoiEntity.class);
		Subquery<PersonEntity> subQuery = outerQuery.subquery(PersonEntity.class);
		Root<PersonEntity> personEntity = subQuery.from(PersonEntity.class);
		Predicate predicate1 = builder.equal(personEntity.get("personId"), vo.getPersonId());
		subQuery.select(personEntity.get("entityId")).where(builder.and(predicate1));
		if (vo.getFilterType() == null || vo.getFilterType().isEmpty()) {
			outerQuery.select(coiEntity).where(builder.in(coiEntity.get("entityId")).value(subQuery));
		} else if (vo.getFilterType().equals("active")) {
			outerQuery.select(coiEntity).where(builder.and(builder.in(coiEntity.get("entityId")).value(subQuery),
					builder.equal(coiEntity.get("isActive"), true)));
		} else if (vo.getFilterType().equals("inactive")) {
			outerQuery.select(coiEntity).where(builder.and(builder.in(coiEntity.get("entityId")).value(subQuery),
					builder.equal(coiEntity.get("isActive"), false)));
		}
		outerQuery.orderBy(builder.asc(coiEntity.get("entityName")));
		return session.createQuery(outerQuery).getResultList();
	}

	@Override
	public void setEntityStatus(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiEntity> criteriaUpdate = cb.createCriteriaUpdate(CoiEntity.class);
		Root<CoiEntity> root = criteriaUpdate.from(CoiEntity.class);
		criteriaUpdate.set("isActive", vo.getActive());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(root.get("entityId"), vo.getCoiEntityId()));
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
	public List<CoiEntity> getAllSystemEntityList(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<CoiEntity> resultCoiEntityList = new ArrayList<>();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String entityName = vo.getProperty1();
		String country = vo.getProperty2();
		String entityStatus = vo.getProperty20() != null && !vo.getProperty20().isEmpty() ? String.join(",", vo.getProperty20()) : "";
		String entityType = vo.getProperty21() != null && !vo.getProperty21().isEmpty() ? String.join(",", vo.getProperty21()) : "";
		String entityRiskLevel = vo.getProperty22() != null && !vo.getProperty22().isEmpty() ? String.join(",", vo.getProperty22()) : "";
		Boolean hasSFI = vo.getProperty18();
		Boolean hasDisclosure = vo.getProperty19();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_ALL_SYSTEM_ENTITY_LIST(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, tabName);
				statement.setString(2, setCOISortOrder(sort));
				statement.setInt(3, (pageNumber == null ? 0 : pageNumber));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setString(5, country);
				statement.setBoolean(6, isDownload);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, entityStatus);
				statement.setString(9, entityType);
				statement.setString(10, entityRiskLevel);
				statement.setBoolean(11, hasSFI);
				statement.setBoolean(12, hasDisclosure);
				statement.setString(13, entityName);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_ALL_SYSTEM_ENTITY_LIST";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, tabName);
				statement.setString(3, setCOISortOrder(sort));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, country);
				statement.setBoolean(7, isDownload);
				statement.setString(8, isAdvancedSearch);
				statement.setString(9, entityStatus);
				statement.setString(10, entityType);
				statement.setString(11, entityRiskLevel);
				statement.setBoolean(12, hasSFI);
				statement.setBoolean(13, hasDisclosure);
				statement.setString(13, entityName);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				CoiEntity coiEntity =  new CoiEntity();
				coiEntity.setEntityId(resultSet.getInt("ENTITY_ID"));
				coiEntity.setEntityName(resultSet.getString("ENTITY_NAME"));
				coiEntity.setCountryDescription(resultSet.getString("COUNTRY"));
				coiEntity.setEntityTypeDescription(resultSet.getString("ENTITY_TYPE"));
				coiEntity.setRiskLevelDescription(resultSet.getString("RISK_LEVEL"));
				coiEntity.setStatusDescription(resultSet.getString("STATUS"));
				coiEntity.setIsActive(resultSet.getString("IS_ACTIVE").equals("Y")?true:false);
				coiEntity.setEntityStatusCode(resultSet.getString("ENTITY_STATUS_CODE"));
				resultCoiEntityList.add(coiEntity);
			}
		} catch (SQLException e) {
			logger.error("Error in getAllSystemEntityList {}", e.getMessage());
		}
		return resultCoiEntityList;
	}
	
	@Override
	public CoiTravelDisclosure saveOrUpdateCoiTravelDisclosure(CoiTravelDisclosure coiTravelDisclosure) {
		hibernateTemplate.saveOrUpdate(coiTravelDisclosure);
		return coiTravelDisclosure;
	}

	@Override
	public List<CoiTravelDisclosure> getAllCoiTravelDisclosureList(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiTravelDisclosure> query = builder.createQuery(CoiTravelDisclosure.class);
		Root<CoiTravelDisclosure> rootEntityName = query.from(CoiTravelDisclosure.class);
		query.orderBy(builder.asc(rootEntityName.get("travelNumber")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public CoiTravelDisclosure getCoiTravelDisclosureDetailsById(Integer travelDisclosureId) {
		return hibernateTemplate.get(CoiTravelDisclosure.class, travelDisclosureId);
	}
	
	@Override
	public List<CoiProjectType> getCoiProjectTypes() {
		return hibernateTemplate.loadAll(CoiProjectType.class);
	}

	@Override
	public DashBoardProfile getPersonEntityDashboard(CoiDashboardVO vo) {
		List<Object> personEntities = new ArrayList<>();
		List<Object> disclosureViews = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer entityId = vo.getId();
		String tabType = vo.getFilterType();
		String startDate = vo.getProperty1();
		String endDate = vo.getProperty2();
		String personName = vo.getProperty3();
		List<String> status = vo.getProperty4();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		Map<String, String> sort = vo.getSort();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PERSON_ENTITY_DASHBOARD(?,?,?,?,?,?,?,?,?)}");
				statement.setInt(1, entityId);
				statement.setString(2, tabType);
				statement.setString(3, startDate);
				statement.setString(4, endDate);
				statement.setString(5, personName);
				statement.setString(6, status != null && !status.isEmpty() ? String.join(",", status) : null);
				statement.setString(7, setCOISortOrder(sort));
				statement.setInt(8, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(9, (pageNumber == null ? 0 : pageNumber));
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_PERSON_ENTITY_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, entityId);
				statement.setString(3, tabType);
				statement.setString(4, startDate);
				statement.setString(5, endDate);
				statement.setString(6, personName);
				statement.setString(7, status != null && !status.isEmpty() ? String.join(",", status) : null);
				statement.setString(8, setCOISortOrder(sort));
				statement.setInt(9, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(10, (pageNumber == null ? 0 : pageNumber));
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			if (tabType.equals("PERSON")) {
				while (resultSet.next()) {
					PersonEntity personEntity = new PersonEntity();
					personEntity.setPersonEntityId(resultSet.getInt("PERSON_ENTITY_ID"));
					personEntity.setPersonId(resultSet.getString("PERSON_ID"));
					personEntity.setPersonFullName(resultSet.getString("FULL_NAME"));
					personEntity.setEntityId(resultSet.getInt("ENTITY_ID"));
					personEntity.setEntityNumber(resultSet.getInt("ENTITY_NUMBER"));
					personEntity.setIsRelationshipActive(resultSet.getBoolean("IS_RELATIONSHIP_ACTIVE"));
					personEntity.setVersionNumber(resultSet.getInt("VERSION_NUMBER"));
					personEntity.setVersionStatus(resultSet.getString("VERSION_STATUS"));
					personEntity.setInvolvementStartDate(resultSet.getDate("INVOLVEMENT_START_DATE"));
					personEntity.setInvolvementEndDate(resultSet.getDate("INVOLVEMENT_END_DATE"));
					personEntity.setStudentInvolvement(resultSet.getString("STUDENT_INVOLVEMENT"));
					personEntity.setStaffInvolvement(resultSet.getString("STAFF_INVOLVEMENT"));
					personEntity.setInstituteResourceInvolvement(resultSet.getString("INSTITUTE_RESOURCE_INVOLVEMENT"));
					personEntity.setRelationshipTypes(resultSet.getString("RELATIONSHIP_TYPES"));
					personEntity.setDesignation(resultSet.getString("DESIGNATION"));
					Unit unit = new Unit();
					unit.setUnitNumber(resultSet.getString("UNIT"));
					unit.setUnitName(resultSet.getString("UNIT_NAME"));
					unit.setOrganizationId(resultSet.getString("ORGANIZATION_ID"));
					unit.setParentUnitNumber(resultSet.getString("PARENT_UNIT_NUMBER"));
					unit.setAcronym(resultSet.getString("ACRONYM"));
					unit.setIsFundingUnit(resultSet.getString("IS_FUNDING_UNIT"));
					personEntity.setUnit(unit);
					personEntities.add(personEntity);
				}
			} else if (tabType.equals("FINANCIAL_DISCLOSURES")) {
				while (resultSet.next()) {
					DisclosureView disclosureView = new DisclosureView();
					disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
					disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
					disclosureView.setConflictStatusCode(resultSet.getString("CONFLICT_STATUS_CODE"));
					disclosureView.setConflictStatus(resultSet.getString("DISCLOSURE_STATUS"));
					disclosureView.setDispositionStatusCode(resultSet.getString("DISPOSITION_STATUS_CODE"));
					disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
					disclosureView.setCertifiedAt(resultSet.getTimestamp("CERTIFIED_AT"));
					disclosureView.setReviewStatusCode(resultSet.getString("REVIEW_STATUS_CODE"));
					disclosureView.setFcoiTypeCode(resultSet.getString("FCOI_TYPE_CODE"));
					disclosureView.setFcoiType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
					disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
					disclosureView.setLastApprovedVersion(resultSet.getInt("VERSION_NUMBER")); // TODO procedure change
					disclosureView.setVersionStatus(resultSet.getString("VERSION_STATUS"));
					disclosureView.setExpirationDate(resultSet.getTimestamp("EXPIRATION_DATE"));
					disclosureView.setCreateTimestamp(resultSet.getTimestamp("CREATE_TIMESTAMP"));
					disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
					disclosureView.setDisclosurePersonFullName(resultSet.getString("DISCLOSURE_PERSON_FULL_NAME"));
					disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER"));
					disclosureView.setCreateUser(resultSet.getString("CREATE_USER"));
					disclosureView.setNoOfSfi(resultSet.getInt("NO_OF_SFI"));
					disclosureView.setNoOfProposal(resultSet.getInt("NO_OF_PROPOSAL"));
					disclosureView.setNoOfAward(resultSet.getInt("NO_OF_AWARD"));
					disclosureView.setProposalTitle(resultSet.getString("PROPOSAL_TITLES"));
					disclosureView.setProposalId(resultSet.getString("PROPOSAL_IDS"));
					disclosureView.setAwardId(resultSet.getString("AWARD_IDS"));
					disclosureView.setAwardTitle(resultSet.getString("AWARD_TITLES"));
					disclosureView.setDescription(resultSet.getString("DESCRIPTION"));
					Unit unit = new Unit();
					unit.setUnitNumber(resultSet.getString("UNIT"));
					unit.setUnitName(resultSet.getString("UNIT_NAME"));
					unit.setOrganizationId(resultSet.getString("ORGANIZATION_ID"));
					unit.setParentUnitNumber(resultSet.getString("PARENT_UNIT_NUMBER"));
					unit.setAcronym(resultSet.getString("ACRONYM"));
					unit.setIsFundingUnit(resultSet.getString("IS_FUNDING_UNIT"));
					disclosureView.setUnit(unit);
					disclosureViews.add(disclosureView);
				}
			} else if (tabType.equals("TRAVEL_DISCLOSURES")) {
				while (resultSet.next()) {
					DisclosureView disclosureView = new DisclosureView();
					disclosureView.setTravelDisclosureId(resultSet.getInt("TRAVEL_DISCLOSURE_ID"));
					disclosureView.setTravelStartDate(resultSet.getDate("TRAVEL_START_DATE"));
					disclosureView.setTravelEndDate(resultSet.getDate("TRAVEL_END_DATE"));
					disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER"));
					disclosureView.setAcknowledgeBy(resultSet.getString("ACKNOWLEDGE_BY"));
					disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
					disclosureView.setTravelEntityName(resultSet.getString("TRAVEL_ENTITY_NAME"));
					disclosureView.setTravellerName(resultSet.getString("TRAVELLER_NAME"));
					disclosureView.setTravelDisclosurestatus(resultSet.getString("TRAVEL_DISCLOSURE_STATUS"));
					disclosureView.setDestination(resultSet.getString("DESTINATION"));
					disclosureView.setPurpose(resultSet.getString("PURPOSE_OF_THE_TRIP"));
					disclosureView.setCertificationDate(resultSet.getDate("CERTIFICATION_DATE"));
					disclosureView.setAcknowledgeDate(resultSet.getDate("ACKNOWLEDGE_DATE"));
					disclosureView.setTravelDisclosureNumber(resultSet.getString("TRAVEL_NUMBER"));
					Unit unit = new Unit();
					unit.setUnitNumber(resultSet.getString("UNIT"));
					unit.setUnitName(resultSet.getString("UNIT_NAME"));
					unit.setOrganizationId(resultSet.getString("ORGANIZATION_ID"));
					unit.setParentUnitNumber(resultSet.getString("PARENT_UNIT_NUMBER"));
					unit.setAcronym(resultSet.getString("ACRONYM"));
					unit.setIsFundingUnit(resultSet.getString("IS_FUNDING_UNIT"));
					disclosureView.setUnit(unit);
					disclosureViews.add(disclosureView);
				}
			}
			if (tabType.equals("PERSON")) {
				return new DashBoardProfile(getPersonEntityDashboardCount(vo), personEntities);
			} else {
				return new DashBoardProfile(getPersonEntityDashboardCount(vo), disclosureViews);
			}
		} catch (Exception ex) {
			ex.printStackTrace();
			logger.error("Exception in getPersonEntityDashboard {}", ex.getMessage());
			throw new ApplicationException("Unable to fetch data", ex, Constants.JAVA_ERROR);
		}
	}

	@Override
	public Integer generateMaxCoiEntityNumber() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
        CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
        Root<CoiEntity> root = query.from(CoiEntity.class);
        query.select(builder.max(root.get("entityNumber")));
        if(session.createQuery(query).getSingleResult() != null) {
            return session.createQuery(query).getSingleResult() + 1;
        } else {
            return 1;
        }
	}

	@Override
	public PersonEntity saveOrUpdateSFI(PersonEntity personEntity) {
		hibernateTemplate.saveOrUpdate(personEntity);
		return personEntity;
	}

	@Override
	public Integer generateMaxDisclosureNumber() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
        CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
        Root<CoiDisclosure> root = query.from(CoiDisclosure.class);
        query.select(builder.max(root.get("disclosureNumber")));
        if(session.createQuery(query).getSingleResult() != null) {
            return session.createQuery(query).getSingleResult() + 1;
        } else {
            return 1;
        }
	}

	@Override
	public DashBoardProfile getCOIReviewerDashboard(CoiDashboardVO vo) {
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
		List<String> conflictStatusCodes = vo.getProperty4();
		List<String> disclosureCategoryTypeCodes = vo.getProperty5();
		String startDate = vo.getProperty6();
		String endDate = vo.getProperty7();
		String entityName = vo.getProperty8();
		String entityCountry = vo.getProperty9();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String personId = AuthenticatedUser.getLoginPersonId();
		List<String> dispositionStatusCodes = vo.getProperty20();
		List<String> reviewStatusCodes = vo.getProperty21();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_REVIEWER_DASHBOARD(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, conflictStatusCodes != null && !conflictStatusCodes.isEmpty() ? String.join(",", conflictStatusCodes) : null);
				statement.setString(5, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(6, startDate);
				statement.setString(7, endDate);
				statement.setString(8, entityName);
				statement.setString(9, entityCountry);
				statement.setString(10, setCOISortOrder(sort));
				statement.setInt(11, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(12, (pageNumber == null ? 0 : pageNumber));
				statement.setString(13, tabName);
				statement.setBoolean(14, isDownload);
				statement.setString(15, isAdvancedSearch);
				statement.setString(16, personId);
				statement.setString(17, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(18, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_REVIEWER_DASHBOARD";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, disclosureId);
				statement.setString(3, disclosurePersonId);
				statement.setString(4, homeUnit);
				statement.setString(5, conflictStatusCodes != null && !conflictStatusCodes.isEmpty() ? String.join(",", conflictStatusCodes) : null);
				statement.setString(6, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(7, startDate);
				statement.setString(8, endDate);
				statement.setString(9, entityName);
				statement.setString(10, entityCountry);
				statement.setString(11, setCOISortOrder(sort));
				statement.setInt(12, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(13, (pageNumber == null ? 0 : pageNumber));
				statement.setString(14, tabName);
				statement.setBoolean(15, isDownload);
				statement.setString(16, isAdvancedSearch);
				statement.setString(17, personId);
				statement.setString(18, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(19, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				DisclosureView disclosureView =  new DisclosureView();
				disclosureView.setCoiDisclosureId(resultSet.getInt("DISCLOSURE_ID"));
				disclosureView.setCoiDisclosureNumber(resultSet.getString("DISCLOSURE_NUMBER"));
				disclosureView.setDisclosurePersonFullName(resultSet.getString("DISCLOSURE_PERSON_FULL_NAME"));
				disclosureView.setVersionStatus(resultSet.getString("VERSION_STATUS"));
				disclosureView.setDispositionStatusCode(resultSet.getString("DISPOSITION_STATUS_CODE"));
				disclosureView.setDispositionStatus(resultSet.getString("DISPOSITION_STATUS"));
				disclosureView.setFcoiTypeCode(resultSet.getString("FCOI_TYPE_CODE"));
				disclosureView.setFcoiType(resultSet.getString("DISCLOSURE_CATEGORY_TYPE"));
				disclosureView.setReviewStatusCode(resultSet.getString("REVIEW_STATUS_CODE"));
				disclosureView.setReviewStatus(resultSet.getString("REVIEW_STATUS"));
				disclosureView.setLastApprovedVersion(resultSet.getInt("LAST_APPROVED_VERSION"));
				disclosureView.setLastApprovedVersionDate(resultSet.getTimestamp("LAST_APPROVED_DATE"));
				disclosureView.setConflictStatus(resultSet.getString("DISCLOSURE_STATUS"));
				disclosureView.setConflictStatusCode(resultSet.getString("CONFLICT_STATUS_CODE"));
				disclosureView.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				disclosureView.setUpdateUser(resultSet.getString("UPDATE_USER_FULL_NAME"));
				disclosureView.setReviseComment(resultSet.getString("REVISION_COMMENT"));
				disclosureView.setPersonId(resultSet.getString("PERSON_ID"));
				disclosureView.setExpirationDate(resultSet.getTimestamp("EXPIRATION_DATE"));
				disclosureView.setCertifiedAt(resultSet.getTimestamp("CERTIFIED_AT"));
				disclosureView.setReviewId(resultSet.getInt("COI_REVIEW_ID"));
				disclosureView.setReviewDescription(resultSet.getString("REVIEW_DESCRIPTION"));
				disclosureView.setReviewerStatusCode(resultSet.getString("REVIEWER_STATUS_CODE"));
				disclosureView.setReviewerStatus(resultSet.getString("REVIEWER_STATUS"));
				disclosureView.setReviewerFullName(resultSet.getString("REVIEWER_NAME"));
				disclosureView.setNoOfSfi(resultSet.getInt("NO_OF_SFI"));
				disclosureView.setNoOfProposal(resultSet.getInt("NO_OF_PROPOSAL"));
				disclosureView.setNoOfAward(resultSet.getInt("NO_OF_AWARD"));
				disclosureView.setProposalTitle(resultSet.getString("PROPOSAL_TITLES"));
				disclosureView.setProposalId(resultSet.getString("PROPOSAL_IDS"));
				disclosureView.setAwardId(resultSet.getString("AWARD_IDS"));
				disclosureView.setAwardTitle(resultSet.getString("AWARD_TITLES"));
				Unit unit = new Unit();
				unit.setUnitNumber(resultSet.getString("UNIT"));
				unit.setUnitName(resultSet.getString("UNIT_NAME"));
				unit.setOrganizationId(resultSet.getString("ORGANIZATION_ID"));
				unit.setParentUnitNumber(resultSet.getString("PARENT_UNIT_NUMBER"));
				unit.setAcronym(resultSet.getString("ACRONYM"));
				unit.setIsFundingUnit(resultSet.getString("IS_FUNDING_UNIT"));
				disclosureView.setUnit(unit);
				disclosureViews.add(disclosureView);
			}
			dashBoardProfile.setDisclosureViews(disclosureViews);
			dashBoardProfile.setDisclosureCount(getCOIReviewerDashboardCount(vo));
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Error in getCOIReviewerDashboard {}", e.getMessage());
			throw new ApplicationException("Unable to fetch data", e, Constants.JAVA_ERROR);
		}
		return dashBoardProfile;
	}

	public Integer getCOIReviewerDashboardCount(CoiDashboardVO vo) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String disclosureId = vo.getProperty1();
		String disclosurePersonId = vo.getProperty2();
		String homeUnit = vo.getProperty3();
		List<String> disclosureStatusCodes = vo.getProperty4();
		List<String> disclosureCategoryTypeCodes = vo.getProperty5();
		String startDate = vo.getProperty6();
		String endDate = vo.getProperty7();
		String entityName = vo.getProperty8();
		String entityCountry = vo.getProperty9();
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String personId = AuthenticatedUser.getLoginPersonId();
		List<String> dispositionStatusCodes = vo.getProperty20();
		List<String> reviewStatusCodes = vo.getProperty21();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_COI_DISCLOSURE_REVIEWER_DASHBOARD_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, disclosureId);
				statement.setString(2, disclosurePersonId);
				statement.setString(3, homeUnit);
				statement.setString(4, disclosureStatusCodes != null && !disclosureStatusCodes.isEmpty() ? String.join(",", disclosureStatusCodes) : null);
				statement.setString(5, disclosureCategoryTypeCodes != null && !disclosureCategoryTypeCodes.isEmpty() ? String.join(",", disclosureCategoryTypeCodes) : null);
				statement.setString(6, startDate);
				statement.setString(7, endDate);
				statement.setString(8, entityName);
				statement.setString(9, entityCountry);
				statement.setString(10, setCOISortOrder(sort));
				statement.setInt(11, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(12, (pageNumber == null ? 0 : pageNumber));
				statement.setString(13, tabName);
				statement.setBoolean(14, isDownload);
				statement.setString(15, isAdvancedSearch);
				statement.setString(16, personId);
				statement.setString(17, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(18, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_COI_DISCLOSURE_REVIEWER_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
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
				statement.setString(11, setCOISortOrder(sort));
				statement.setInt(12, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(13, (pageNumber == null ? 0 : pageNumber));
				statement.setString(14, tabName);
				statement.setBoolean(15, isDownload);
				statement.setString(16, isAdvancedSearch);
				statement.setString(17, personId);
				statement.setString(18, dispositionStatusCodes != null && !dispositionStatusCodes.isEmpty() ? String.join(",", dispositionStatusCodes) : null);
				statement.setString(19, reviewStatusCodes != null && !reviewStatusCodes.isEmpty() ? String.join(",", reviewStatusCodes) : null);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (Exception e) {
			logger.error("Error in getCOIReviewerDashboardCount {}", e.getMessage());
			throw new ApplicationException("Error in getCOIReviewerDashboardCount", e, Constants.JAVA_ERROR);
		}
		return count;
	}


	@Override
	public ConflictOfInterestVO loadDisclosureQuickCardCounts(String dashboardType, String loginPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_QUICK_CARD_COUNTS(?,?)}");
				statement.setString(1, dashboardType);
				statement.setString(2, loginPersonId);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_QUICK_CARD_COUNTS";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, dashboardType);
				statement.setString(3, loginPersonId);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			ConflictOfInterestVO conflictOfInterestVO = new ConflictOfInterestVO();
			while (resultSet.next()) {
				conflictOfInterestVO.setNewSubmissionsCount(resultSet.getInt("NEW_SUBMISSIONS"));
				conflictOfInterestVO.setConflictIdentifiedCount(resultSet.getInt("CONFLICTS_IDENTIFIED"));
			}
			return conflictOfInterestVO;
		} catch (Exception e) {
			logger.error("Error in loadDisclosureQuickCardCounts {}", e.getMessage());
			throw new ApplicationException("Error in loadDisclosureQuickCardCounts", e, Constants.JAVA_ERROR);
		}
	}
	
	@Override
	public CoiProjectProposal saveOrUpdateCoiProjectProposal(CoiProjectProposal coiProjectProposal) {
		hibernateTemplate.saveOrUpdate(coiProjectProposal);
		return coiProjectProposal;
	}
	
	@Override
	public CoiProjectAward saveOrUpdateCoiProjectAward(CoiProjectAward coiProjectAward) {
		hibernateTemplate.saveOrUpdate(coiProjectAward);
		return coiProjectAward;
	}

	@Override
	public CoiEntity getCoiEntityDetailsByEntityId(Integer personEntityId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("select pe.coiEntity from PersonEntity pe where pe.personEntityId = :personEntityId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("personEntityId", personEntityId);
		return (CoiEntity) query.getSingleResult();
	}

	@Override
	public PersonEntity getPersonEntityDetailsByEntityId(Integer personEntityId) {
		PersonEntity personEntity = new PersonEntity();
		personEntity = hibernateTemplate.get(PersonEntity.class, personEntityId);
		personEntity.setCoiEntity(null);
		return personEntity;
	}

	public Integer getPersonEntityDashboardCount(CoiDashboardVO vo) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer entityId = vo.getId();
		String tabType = vo.getFilterType();
		String startDate = vo.getProperty1();
		String endDate = vo.getProperty2();
		String personName = vo.getProperty3();
		List<String> status = vo.getProperty4();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PERSON_ENTITY_DASHBOARD_COUNT(?,?,?,?,?,?)}");
				statement.setInt(1, entityId);
				statement.setString(2, tabType);
				statement.setString(3, startDate);
				statement.setString(4, endDate);
				statement.setString(5, personName);
				statement.setString(6, status != null && !status.isEmpty() ? String.join(",", status) : null);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_PERSON_ENTITY_DASHBOARD_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, entityId);
				statement.setString(3, tabType);
				statement.setString(4, startDate);
				statement.setString(5, endDate);
				statement.setString(6, personName);
				statement.setString(7, status != null && !status.isEmpty() ? String.join(",", status) : null);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				count = Integer.parseInt(resultSet.getString(1));
			}
			return count;
		} catch (Exception ex) {
			ex.printStackTrace();
			logger.error("getPersonEntityDashboardCount {}", ex.getMessage());
			throw new ApplicationException("Unable to fetch data", ex, Constants.JAVA_ERROR);
		}
	}
	
	@Override
	public List<ValidPersonEntityRelType> getRelatioshipDetails(String disclosureTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ValidPersonEntityRelType> query = builder.createQuery(ValidPersonEntityRelType.class);
		Root<ValidPersonEntityRelType> rootValidPersonEntityRelType = query.from(ValidPersonEntityRelType.class);
		query.where(builder.equal(rootValidPersonEntityRelType.get("disclosureTypeCode"), disclosureTypeCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<PersonEntityRelationship> getRelatioshipDetails(ConflictOfInterestVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonEntityRelationship> query = builder.createQuery(PersonEntityRelationship.class);
		Root<PersonEntityRelationship> rootPersonEntityRelationship = query.from(PersonEntityRelationship.class);
		query.where(builder.and(
			    builder.equal(rootPersonEntityRelationship.get("personEntityId"), vo.getPersonEntityId()),
			    builder.equal(rootPersonEntityRelationship.get("validPersonEntityRelType").get("disclosureTypeCode"), vo.getDisclosureTypeCode())
			));
		return session.createQuery(query).getResultList();
	}

	@Override
	public CoiReviewStatusType getReviewStatusByCode(String reviewStatusPending) {
		return hibernateTemplate.get(CoiReviewStatusType.class, reviewStatusPending);
	}

	@Override
	public CoiRiskCategory getRiskCategoryStatusByCode(String riskCategoryLow) {
		return hibernateTemplate.get(CoiRiskCategory.class, riskCategoryLow);
	}

	@Override
	public PersonEntityRelationship getPersonEntityRelationshipByPersonEntityRelId(Integer personEntityRelId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonEntityRelationship> query = builder.createQuery(PersonEntityRelationship.class);
		Root<PersonEntityRelationship> rootPersonEntityRelationship = query.from(PersonEntityRelationship.class);
		query.where(builder.equal(rootPersonEntityRelationship.get("personEntityRelId"), personEntityRelId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public ValidPersonEntityRelType getValidPersonEntityRelTypeByTypeCode(Integer validPersonEntityRelTypeCode) {
		return hibernateTemplate.get(ValidPersonEntityRelType.class, validPersonEntityRelTypeCode);
	}
	
	@Override
	public CoiTravelDisclosureTraveler saveOrUpdateCoiTravelDisclosureTraveller(CoiTravelDisclosureTraveler coiTravelDisclosureTraveller) {
		hibernateTemplate.saveOrUpdate(coiTravelDisclosureTraveller);
		return coiTravelDisclosureTraveller;
	}
	
	@Override
	public Integer generateMaxTravelNumber() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
        CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
        Root<CoiTravelDisclosure> root = query.from(CoiTravelDisclosure.class);
        query.select(builder.max(root.get("travelNumber")));
        if(session.createQuery(query).getSingleResult() != null) {
             Integer result = session.createQuery(query).getSingleResult();
             return result+1;
        } else {
            return 1;
        }
	}
    
	@Override
	public List<CoiTravelerType> loadTravellerTypesLookup() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiTravelerType> query = builder.createQuery(CoiTravelerType.class);
		Root<CoiTravelerType> root = query.from(CoiTravelerType.class);
		query.where(builder.equal(root.get("isActive"), true));
		query.orderBy(builder.asc(root.get("travelerTypeCode")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<CoiTravelerStatusType> loadTravelStatusTypesLookup() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<CoiTravelerStatusType> query = builder.createQuery(CoiTravelerStatusType.class);
		Root<CoiTravelerStatusType> root = query.from(CoiTravelerStatusType.class);
		query.where(builder.equal(root.get("isActive"), true));
		query.orderBy(builder.asc(root.get("travelStatusCode")));
		return session.createQuery(query).getResultList();
	}
	
	@Override
	public List<ValidPersonEntityRelType> getValidPersonEntityRelTypes(Integer personEntityId) {	
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ValidPersonEntityRelType> query = builder.createQuery(ValidPersonEntityRelType.class);
		Root<PersonEntityRelationship> rootCOIFinancialEntity = query.from(PersonEntityRelationship.class);
		query.select(rootCOIFinancialEntity.get("validPersonEntityRelType")); 
		query.where(builder.equal(rootCOIFinancialEntity.get("personEntityId"), personEntityId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer getAllSystemEntityListCount(CoiDashboardVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		String tabName = vo.getTabName();
		Integer currentPage = vo.getCurrentPage();
		Integer pageNumber = vo.getPageNumber();
		String isAdvancedSearch = vo.getAdvancedSearch();
		Boolean isDownload = vo.getIsDownload();
		Map<String, String> sort = vo.getSort();
		String entityName = vo.getProperty1();
		String country = vo.getProperty2();
		String entityStatus = vo.getProperty20() != null && !vo.getProperty20().isEmpty() ? String.join(",", vo.getProperty20()) : "";
		String entityType = vo.getProperty21() != null && !vo.getProperty21().isEmpty() ? String.join(",", vo.getProperty21()) : "";
		String entityRiskLevel = vo.getProperty22() != null && !vo.getProperty22().isEmpty() ? String.join(",", vo.getProperty22()) : "";
		Boolean hasSFI = vo.getProperty18();
		Boolean hasDisclosure = vo.getProperty19();
		Integer count = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_ALL_SYSTEM_ENTITY_LIST_COUNT(?,?,?,?,?,?,?,?,?,?,?,?,?)}");
				statement.setString(1, tabName);
				statement.setString(2, setCOISortOrder(sort));
				statement.setInt(3, (pageNumber == null ? 0 : pageNumber));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setString(5, country);
				statement.setBoolean(6, isDownload);
				statement.setString(7, isAdvancedSearch);
				statement.setString(8, entityStatus);
				statement.setString(9, entityType);
				statement.setString(10, entityRiskLevel);
				statement.setBoolean(11, hasSFI);
				statement.setBoolean(12, hasDisclosure);
				statement.setString(13, entityName);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_ALL_SYSTEM_ENTITY_LIST_COUNT";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, tabName);
				statement.setString(3, setCOISortOrder(sort));
				statement.setInt(4, (currentPage == null ? 0 : currentPage - 1));
				statement.setInt(5, (pageNumber == null ? 0 : pageNumber));
				statement.setString(6, country);
				statement.setBoolean(7, isDownload);
				statement.setString(8, isAdvancedSearch);
				statement.setString(9, entityStatus);
				statement.setString(10, entityType);
				statement.setString(11, entityRiskLevel);
				statement.setBoolean(12, hasSFI);
				statement.setBoolean(13, hasDisclosure);
				statement.setString(13, entityName);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			while (resultSet.next()) {
				 count = Integer.parseInt(resultSet.getString(1));
			}
		} catch (SQLException e) {
			logger.error("Error in GET_ALL_SYSTEM_ENTITY_LIST_COUNT {}", e.getMessage());
		}
		return count;
	}

	@Override
	public List<CoiProjConflictStatusType> getProjConflictStatusTypes() {
		return hibernateTemplate.loadAll(CoiProjConflictStatusType.class);
	}

	@Override
	public boolean checkEntityAdded(Integer entityId) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("select (CASE WHEN count(pe.personEntityId) > 0 THEN true ELSE false END) from PersonEntity pe where " +
				"pe.entityId = :entityId AND pe.personId = :personId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("entityId", entityId);
		query.setParameter("personId", AuthenticatedUser.getLoginPersonId());
		return (Boolean) query.getSingleResult();
	}

	@Override
	public void syncProjectWithDisclosure(Integer disclosureId, Integer disclosureNumber, Integer personEntityId, Integer moduleCode, String moduleItemKey, String type) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		try {
			CallableStatement statement = connection.prepareCall("{call SYNC_SFIS_AND_DISCLOSURE(?,?,?,?,?,?,?,?)}");
			statement.setInt(1, disclosureId);
			statement.setInt(2, disclosureNumber);
			statement.setString(3, AuthenticatedUser.getLoginPersonId());
			statement.setString(4, AuthenticatedUser.getLoginUserName());
			if (personEntityId == null) {
				statement.setNull(5, Types.INTEGER);
			} else {
				statement.setInt(5, personEntityId);
			}
			if (moduleCode == null) {
				statement.setNull(6, Types.INTEGER);
			} else {
				statement.setInt(6, moduleCode);
			}
			if (moduleItemKey == null) {
				statement.setNull(7, Types.INTEGER);
			} else {
				statement.setString(7, moduleItemKey);
			}
			statement.setString(8, type);
			statement.execute();
		} catch (Exception e) {
			throw new ApplicationException("error in sync projects", e, Constants.DB_FN_ERROR);
		}
	}

	@Override
	public CoiDisclosureFcoiType getCoiDisclosureFcoiTypeByCode(String coiTypeCode) {
		return hibernateTemplate.get(CoiDisclosureFcoiType.class, coiTypeCode);
	}

	@Override
	public List<PersonEntityRelationship> getPersonEntityRelationshipByPersonEntityId(Integer personEntityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonEntityRelationship> query = builder.createQuery(PersonEntityRelationship.class);
		Root<PersonEntityRelationship> rootPersonEntityRelationship = query.from(PersonEntityRelationship.class);
		query.where(builder.equal(rootPersonEntityRelationship.get("personEntityId"), personEntityId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer getNumberOfProposalsBasedOnDisclosureId(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Long> query = builder.createQuery(Long.class);
		Root<CoiDisclEntProjDetails> rootCoiDisclEntProjDetails = query.from(CoiDisclEntProjDetails.class);
		query.where(builder.and(
			    builder.equal(rootCoiDisclEntProjDetails.get("moduleCode"), Constants.DEV_PROPOSAL_MODULE_CODE)),
			    builder.equal(rootCoiDisclEntProjDetails.get("disclosureId"), disclosureId));
		query.multiselect(builder.countDistinct(rootCoiDisclEntProjDetails.get("moduleItemKey")));
		Long numberOfProposals = session.createQuery(query).getSingleResult();
		return numberOfProposals.intValue();
	}

	@Override
	public Integer getNumberOfAwardsBasedOnDisclosureId(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Long> query = builder.createQuery(Long.class);
		Root<CoiDisclEntProjDetails> rootCoiDisclEntProjDetails = query.from(CoiDisclEntProjDetails.class);
		query.where(builder.and(
			    builder.equal(rootCoiDisclEntProjDetails.get("moduleCode"), Constants.AWARD_MODULE_CODE)),
			    builder.equal(rootCoiDisclEntProjDetails.get("disclosureId"), disclosureId));
		query.multiselect(builder.countDistinct(rootCoiDisclEntProjDetails.get("moduleItemKey")));
		Long numberOfAwards = session.createQuery(query).getSingleResult();
		return numberOfAwards.intValue();
	}

	@Override
	public boolean isSFIDefined(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = builder.createQuery(Long.class);
			Root<PersonEntity> rootCOIFinancialEntity = query.from(PersonEntity.class);
			query.select(builder.count(rootCOIFinancialEntity));
			query.where(builder.equal(rootCOIFinancialEntity.get("personId"), personId));
			Long count = session.createQuery(query).getSingleResult();
			return count.intValue() > 0 ? true : false;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public boolean isRelationshipDefined(Integer disclosureId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = builder.createQuery(Long.class);
			Root<CoiDisclEntProjDetails> rootCoiDisclEntProjDetails = query.from(CoiDisclEntProjDetails.class);
			query.select(builder.count(rootCoiDisclEntProjDetails));
			query.where(builder.and(builder.equal(rootCoiDisclEntProjDetails.get("disclosureId"), disclosureId),
					builder.isNotNull(rootCoiDisclEntProjDetails.get("projectConflictStatusCode"))));
			Long count = session.createQuery(query).getSingleResult();
			return count.intValue() > 0 ? true : false;
		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public boolean isMasterDisclosurePresent(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = builder.createQuery(Long.class);
			Root<CoiDisclosure> rootCoiDisclosure = query.from(CoiDisclosure.class);
			query.select(builder.count(rootCoiDisclosure));
			query.where(builder.and(builder.equal(rootCoiDisclosure.get("personId"), personId),
					builder.equal(rootCoiDisclosure.get("fcoiTypeCode"), "1"),
					builder.equal(rootCoiDisclosure.get("versionStatus"), "Active")));
			Long count = session.createQuery(query).getSingleResult();
			return count > 0 ? true : false;
		} catch (Exception ex) {
			return false;
		}
	}

	@Override
	public void archiveDisclosureOldVersions(Integer disclosureId, Integer disclosureNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE CoiDisclosure SET versionStatus = :archived where disclosureId != :disclosureId AND disclosureNumber = :disclosureNumber");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("archived", "Archived"); // set old disclosure to archived
		query.setParameter("disclosureId", disclosureId);
		query.setParameter("disclosureNumber", disclosureNumber);
		query.executeUpdate();
	}
	
	@Override
	public Integer fetchMaxPersonEntityId(String personId, Integer entityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT max(personEntityId) FROM PersonEntity WHERE personId=:personId and entityId=:entityId";
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("personId", personId);
		query.setParameter("entityId", entityId);
		return query.getSingleResult();
	}
	
	@Override
	public Integer generateMaxPersonEntityId() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
        CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
        Root<PersonEntity> root = query.from(PersonEntity.class);
        query.select(builder.max(root.get("personEntityId")));
        if(session.createQuery(query).getSingleResult() != null) {
             Integer result = session.createQuery(query).getSingleResult();
             return result+1;
        } else {
            return 1;
        }
	}
	
	@Override
	public void saveOrUpdateDisclComment(DisclComment disclComment) {
		hibernateTemplate.saveOrUpdate(disclComment);
	}

	@Override
	public DisclComment getDisclEntProjRelationComment(Integer disclosureDetailsId) {
		DisclComment disclComment = new DisclComment(); 
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<DisclComment> query = builder.createQuery(DisclComment.class);
			Root<DisclComment> rootDisclComment = query.from(DisclComment.class);
			query.where(builder.equal(rootDisclComment.get("componentReferenceId"), disclosureDetailsId));
			disclComment = session.createQuery(query).getSingleResult();
			return disclComment;
		} catch (Exception ex) {
			return disclComment;
		}
	}

	@Override
	public Map<String, Object> validateProjectDisclosure(String personId, Integer moduleCode, String moduleItemKey) {
		Map<String, Object> mapObj = new HashMap();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call VALIDATE_PROJECT_DISCLOSURE(?,?,?)}");
				statement.setString(1, personId);
				statement.setInt(2, moduleCode);
				statement.setString(3, moduleItemKey);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call VALIDATE_PROJECT_DISCLOSURE(?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, personId);
				statement.setInt(3, moduleCode);
				statement.setString(4, moduleItemKey);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset.next()) {
				mapObj.put("isExpired", rset.getBoolean("isExpired"));
				mapObj.put("pendingProject", rset.getInt("pendingProject") == 0 ? null : rset.getInt("pendingProject"));
				mapObj.put("fcoiProject", rset.getInt("fcoiProject") == 0 ? null : rset.getInt("fcoiProject"));
			}
		} catch (Exception e) {
			logger.error("Exception on fetchAllCoiRights {}", e.getMessage());
			throw new ApplicationException("Unable to fetch rights", e, Constants.JAVA_ERROR);
		}
		return mapObj;
	}


	@Override
	public List<PersonEntity> getSFIOfDisclosure(String personId, Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		List<PersonEntity> personEntities = new ArrayList<>();
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_PERSON_ENTITIES(?,?)}");
				if(personId == null) {
					statement.setNull(1, Types.VARCHAR);
				} else {
					statement.setString(1, personId);
				}
				if (disclosureId == null) {
					statement.setNull(2, Types.INTEGER);
				} else {
					statement.setInt(2, disclosureId);
				}
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String functionCall = "{call GET_PERSON_ENTITIES(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				if(personId == null) {
					statement.setNull(2, Types.VARCHAR);
				} else {
					statement.setString(2, personId);
				}
				if (disclosureId == null) {
					statement.setNull(3, Types.INTEGER);
				} else {
					statement.setInt(3, disclosureId);
				}
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset != null && rset.next()) {
				PersonEntity personEntity = new PersonEntity();
				personEntity.setPersonEntityId(rset.getInt("PERSON_ENTITY_ID"));
				personEntity.setPersonId(rset.getString("PERSON_ID"));
				personEntity.setEntityId(rset.getInt("ENTITY_ID"));
				personEntity.setEntityNumber(rset.getInt("ENTITY_NUMBER"));
				personEntity.setInvolvementStartDate(rset.getDate("INVOLVEMENT_START_DATE"));
				personEntity.setInvolvementEndDate(rset.getDate("INVOLVEMENT_END_DATE"));
				personEntity.setVersionStatus(rset.getString("VERSION_STATUS"));
				personEntity.setCoiEntity(new CoiEntity());
				personEntity.getCoiEntity().setEntityId(rset.getInt("ENTITY_ID"));
				personEntity.getCoiEntity().setEntityNumber(rset.getInt("ENTITY_NUMBER"));
				personEntity.getCoiEntity().setEntityName(rset.getString("ENTITY_NAME"));
				personEntity.getCoiEntity().setEntityTypeCode(rset.getString("ENTITY_TYPE_CODE"));
				personEntity.getCoiEntity().setCountry(new Country());
				personEntity.getCoiEntity().getCountry().setCountryName(rset.getString("COUNTRY_NAME"));
				personEntity.getCoiEntity().setEntityType(new EntityType());
				personEntity.getCoiEntity().getEntityType().setDescription(rset.getString("ENTITY_TYPE"));
				personEntities.add(personEntity);
			}
		} catch (Exception e) {
			logger.error("Exception on getSFIOfDisclosure {}", e.getMessage());
			throw new ApplicationException("Unable to fetch SFIs", e, Constants.JAVA_ERROR);
		}
		return personEntities;
	}

	@Override
	public void assignDisclosureAdmin(Integer adminGroupId, String adminPersonId, Integer disclosureId) {

		StringBuilder hqlQuery = new StringBuilder();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		hqlQuery.append("UPDATE CoiDisclosure c SET c.adminGroupId = :adminGroupId , c.adminPersonId = :adminPersonId, c.reviewStatusCode = 3 ");
		hqlQuery.append("WHERE c.disclosureId = : disclosureId");
		Query query = session.createQuery(hqlQuery.toString());
		query.setParameter("adminGroupId", adminGroupId);
		query.setParameter("adminPersonId", adminPersonId);
		query.setParameter("disclosureId", disclosureId);
		query.executeUpdate();
	}

	@Override
	public void updateReviewStatus(Integer disclosureId, String disclosureReviewInProgress) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<CoiDisclosure> criteriaUpdate = cb.createCriteriaUpdate(CoiDisclosure.class);
		Root<CoiDisclosure> root = criteriaUpdate.from(CoiDisclosure.class);
		criteriaUpdate.set("reviewStatusCode", disclosureReviewInProgress);
		criteriaUpdate.where(cb.equal(root.get("disclosureId"), disclosureId));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<EntityRiskCategory> fetchEntityRiskCategory() {
		return hibernateTemplate.loadAll(EntityRiskCategory.class);
	}

	@Override
	public Unit getUnitFromUnitNumber(String unitNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Unit> query = builder.createQuery(Unit.class);
		Root<Unit> rootDisclComment = query.from(Unit.class);
		query.where(builder.equal(rootDisclComment.get("unitNumber"), unitNumber));
		return session.createQuery(query).getSingleResult();
	}


	@Override
	public CoiConflictStatusTypeDto validateConflicts(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		try {
			CallableStatement statement = connection.prepareCall("{call COI_VALIDATE_DISCLOSURE_CONFLICTS(?,?,?)}");
			statement.setInt(1, disclosureId);
			statement.setString(2, AuthenticatedUser.getLoginPersonId());
			statement.setString(3, AuthenticatedUser.getLoginUserName());
			statement.execute();
			ResultSet rset = statement.getResultSet();
			if (rset != null && rset.next()) {
				CoiConflictStatusTypeDto  conflictStatusTypeDto = new CoiConflictStatusTypeDto();
				conflictStatusTypeDto.setConflictStatusCode(rset.getString(1));
				conflictStatusTypeDto.setDescription(rset.getString(2));
				return conflictStatusTypeDto;
			}
		} catch (Exception e) {
			logger.error("Exception on validateConflicts {}", e.getMessage());
			throw new ApplicationException("error in validate conflicts ", e, Constants.DB_FN_ERROR);
		}
		return null;
	}

	@Override
	public CoiConflictStatusType loadCoiConflictStatusType(String conflictStatusCode) {
		return hibernateTemplate.load(CoiConflictStatusType.class, conflictStatusCode);
	}
}
