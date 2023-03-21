package com.polus.fibicomp.evaluation.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.internal.SessionImpl;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.evaluation.pojo.EvaluationRecommendation;
import com.polus.fibicomp.evaluation.pojo.EvaluationStop;
import com.polus.fibicomp.evaluation.pojo.FinalEvaluationStatus;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanel;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationPanelPersons;
import com.polus.fibicomp.evaluation.pojo.ProposalEvaluationStatusFlow;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.evaluation.pojo.ReviewAttachment;
import com.polus.fibicomp.evaluation.pojo.ReviewComment;
import com.polus.fibicomp.evaluation.pojo.ReviewStatus;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.roles.pojo.Role;

import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "evaluationDao")
public class EvaluationDaoImpl implements EvaluationDao {

	protected static Logger logger = LogManager.getLogger(EvaluationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	private static final String PROPOSAL_ID = "proposalId";
	private static final String REVIEWER_PERSON_ID = "reviewerPersonId";
	private static final String REVIEW_STATUS_CODE = "reviewStatusCode";
	private static final String ROLE_ID = "roleId";
	private static final String REVIEWER_FULLNAME = "reviewerFullName";
	private static final String REVIEW_START_DATE = "reviewStartDate";
	private static final String REVIEW_END_DATE = "reviewEndDate";
	private static final String REVIEW_DEADLINE_DATE = "reviewDeadLineDate";
	private static final String PI_REVIEW_DEADLINE_DATE = "piReviewDeadLineDate";
	private static final String ACTIVITY_TYPE_CODE = "activityTypeCode";
	private static final String STATUS_CODE = "statusCode";

	@Override
	public List<ProposalReview> fetchProposalReviewsByCriteria(Integer proposalId, String personId, Integer preReviewStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> review = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(review.get(PROPOSAL_ID), proposalId);
		Predicate predicatePersonId = builder.equal(review.get(REVIEWER_PERSON_ID), personId);
		Predicate predicatePreReviewStatus = builder.equal(review.get(REVIEW_STATUS_CODE), preReviewStatus);
		query.where(builder.and(predicateProposalId, predicatePersonId, predicatePreReviewStatus));
		return session.createQuery(query).list();
	}

	@Override
	public ReviewStatus getReviewStatusByStatusCode(Integer statusCode) {
		logger.info("--------- getReviewStatusByStatusCode ---------");
		return hibernateTemplate.get(ReviewStatus.class, statusCode);
	}

	@Override
	public ProposalReview saveOrUpdateReview(ProposalReview proposalReview) {
		logger.info("--------- saveOrUpdateReview ---------");
		hibernateTemplate.saveOrUpdate(proposalReview);
		return proposalReview;
	}

	@Override
	public List<ProposalReview> loadAllProposalReviewsByProposalId(Integer proposalId) {
		logger.info("--------- loadAllProposalReviewsByProposalId ---------");
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> review = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(review.get(PROPOSAL_ID), proposalId);
		query.where(builder.and(predicateProposalId));
		return session.createQuery(query).list();
	}

	@Override
	public ReviewAttachment fetchAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(ReviewAttachment.class, attachmentId);
	}

	@Override
	public List<ProposalReview> getInprogressReviews(Integer proposalId, Integer roleId, Integer reviewStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> proposalReview = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(proposalReview.get(PROPOSAL_ID), proposalId);
		Predicate predicateRoleId = builder.equal(proposalReview.get(ROLE_ID), roleId);
		Predicate predicateReviewStatusCode = builder.equal(proposalReview.get(REVIEW_STATUS_CODE), reviewStatusCode);
		query.where(builder.and(predicateProposalId, predicateRoleId, predicateReviewStatusCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalReview> getProposalReviews(Integer proposalId, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> proposalReview = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(proposalReview.get(PROPOSAL_ID), proposalId);
		Predicate predicateRoleId = builder.equal(proposalReview.get(ROLE_ID), roleId);
		Predicate predicateReviewStatusCode = builder.equal(proposalReview.get(REVIEW_STATUS_CODE), Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		query.where(builder.and(predicateProposalId, predicateRoleId, predicateReviewStatusCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ReviewComment saveReviewComment(ReviewComment reviewComment) {
		hibernateTemplate.save(reviewComment);
		return reviewComment;
	}

	@Override
	public List<ProposalReview> fetchSortedReviews(Integer proposalId, String sortBy, String reverse) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> proposalReviewQuery = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> reviewRoot = proposalReviewQuery.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(reviewRoot.get(PROPOSAL_ID), proposalId);
		proposalReviewQuery.where(builder.and(predicateProposalId));
		if (sortBy.equals(ROLE_ID)) {
			if (reverse.equals("ASC")) {
				proposalReviewQuery.orderBy(builder.asc(reviewRoot.get("role").get("roleName")));
			} else {
				proposalReviewQuery.orderBy(builder.desc(reviewRoot.get("role").get("roleName")));
			}
		} else if (sortBy.equals(REVIEWER_FULLNAME)) {
			if (reverse.equals("ASC")) {
				proposalReviewQuery.orderBy(builder.asc(reviewRoot.get(REVIEWER_FULLNAME)));
			} else {
				proposalReviewQuery.orderBy(builder.desc(reviewRoot.get(REVIEWER_FULLNAME)));
			}
		} else if (sortBy.equals(REVIEW_STATUS_CODE)) {
			if (reverse.equals("ASC")) {
				proposalReviewQuery.orderBy(builder.asc(reviewRoot.get("reviewStatus").get("description")));
			} else {
				proposalReviewQuery.orderBy(builder.desc(reviewRoot.get("reviewStatus").get("description")));
			}
		} else if (sortBy.equals(REVIEW_START_DATE)) {
			if (reverse.equals("ASC")) {
				proposalReviewQuery.orderBy(builder.asc(reviewRoot.get(REVIEW_START_DATE)));
			} else {
				proposalReviewQuery.orderBy(builder.desc(reviewRoot.get(REVIEW_START_DATE)));
			}
		} else if (sortBy.equals(REVIEW_END_DATE)) {
			if (reverse.equals("ASC")) {
				proposalReviewQuery.orderBy(builder.asc(reviewRoot.get(REVIEW_END_DATE)));
			} else {
				proposalReviewQuery.orderBy(builder.desc(reviewRoot.get(REVIEW_END_DATE)));
			}
		} else if (sortBy.equals(REVIEW_DEADLINE_DATE)) {
			if (reverse.equals("ASC")) {
				proposalReviewQuery.orderBy(builder.asc(reviewRoot.get(REVIEW_DEADLINE_DATE)));
			} else {
				proposalReviewQuery.orderBy(builder.desc(reviewRoot.get(REVIEW_DEADLINE_DATE)));
			}
		} else if (sortBy.equals(PI_REVIEW_DEADLINE_DATE)) {
			if (reverse.equals("ASC")) {
				proposalReviewQuery.orderBy(builder.asc(reviewRoot.get(PI_REVIEW_DEADLINE_DATE)));
			} else {
				proposalReviewQuery.orderBy(builder.desc(reviewRoot.get(PI_REVIEW_DEADLINE_DATE)));
			}
		}
		return session.createQuery(proposalReviewQuery).getResultList();
	}

	@Override
	public Integer fetchProposalStatusBasedOnReview(ProposalReview newProposalReview) {
		logger.info("roleId : {}", newProposalReview.getRoleId());
		logger.info("stopNumber : {}", newProposalReview.getEvaluationStop().getStopNumber());
		logger.info("reviewStatusCode : {}", newProposalReview.getReviewStatusCode());
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalEvaluationStatusFlow> query = builder.createQuery(ProposalEvaluationStatusFlow.class);
		Root<ProposalEvaluationStatusFlow> rootProposalEvaluationStatusFlow = query.from(ProposalEvaluationStatusFlow.class);
		Predicate predicateRoleId = builder.equal(rootProposalEvaluationStatusFlow.get(ROLE_ID), newProposalReview.getRoleId());
		Predicate predicateStopNumber = builder.equal(rootProposalEvaluationStatusFlow.get("stopNumber"), newProposalReview.getEvaluationStop().getStopNumber());
		Predicate predicateReviewStatusCode = builder.equal(rootProposalEvaluationStatusFlow.get(REVIEW_STATUS_CODE), newProposalReview.getReviewStatusCode());
		query.where(builder.and(predicateRoleId, predicateStopNumber, predicateReviewStatusCode));
		return session.createQuery(query).uniqueResult().getStatusCode();
	}

	@Override
	public Boolean fetchProposalReviewIsCompletedOnce(ProposalReview newProposalReview) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> rootProposalReview = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(rootProposalReview.get(PROPOSAL_ID), newProposalReview.getProposalId());
		Predicate predicateRoleId = builder.equal(rootProposalReview.get(ROLE_ID), newProposalReview.getRoleId());
		Predicate predicateReviewStatusCode = builder.equal(rootProposalReview.get(REVIEW_STATUS_CODE),
				Constants.PROPOSAL_REVIEW_STATUS_COMPLETE);
		query.where(builder.and(predicateProposalId, predicateRoleId, predicateReviewStatusCode));
		List<ProposalReview> proposalReviews = session.createQuery(query).getResultList();
		if (proposalReviews != null && !proposalReviews.isEmpty()) {
			return false;
		}
		return true;
	}

	@Override
	public ProposalReview getProposalReviewByReviewId(Integer reviewId) {
		return hibernateTemplate.get(ProposalReview.class, reviewId);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Integer> getDistintRoleIdFromEvaluationsStop() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT DISTINCT roleId FROM EvaluationStop ";
		Query<Integer> query = session.createQuery(hqlQuery);
		return query.getResultList();
	}

	@Override
	public List<PersonRoles> fetchPersonRoleOnEvaluationBasedOnPersonId(String personId, List<Integer> roleIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PersonRoles> query = builder.createQuery(PersonRoles.class);
		Root<PersonRoles> personRole = query.from(PersonRoles.class);
		Predicate predicatePersonId = builder.equal(personRole.get("personId"), personId);
		Predicate predicateRoleId = personRole.get(ROLE_ID).in(roleIds);
		query.where(builder.and(predicatePersonId, predicateRoleId));
		return session.createQuery(query).list();
	}

	@Override
	public Boolean checkPersonHasRank(Integer proposalId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> rootProposalReview = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(rootProposalReview.get(PROPOSAL_ID), proposalId);
		Predicate predicateReviewStatusCode = builder.equal(rootProposalReview.get(REVIEW_STATUS_CODE),
				Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		Predicate predicateReviewerPersonId = builder.equal(rootProposalReview.get(REVIEWER_PERSON_ID), personId);
		Predicate predicateCompleteReviewerPersonId = builder.equal(rootProposalReview.get("completeReviewerPersonId"), personId);
		Predicate predicateHasRank = builder.equal(rootProposalReview.get("hasRank"), true);
		Predicate predicatePersonId = builder.or(predicateReviewerPersonId, predicateCompleteReviewerPersonId);
		query.where(builder.and(predicateProposalId, predicateReviewStatusCode, predicateHasRank, predicatePersonId));
		List<ProposalReview> proposalReviews = session.createQuery(query).list();
		if (proposalReviews != null && !proposalReviews.isEmpty()) {
			return true;
		}
		return false;
	}

	@Override
	public Boolean checkPersonHasRecommendation(Integer proposalId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> rootProposalReview = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(rootProposalReview.get(PROPOSAL_ID), proposalId);
		Predicate predicateReviewStatusCode = builder.equal(rootProposalReview.get(REVIEW_STATUS_CODE),
				Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		Predicate predicateReviewerPersonId = builder.equal(rootProposalReview.get(REVIEWER_PERSON_ID), personId);
		Predicate predicateCompleteReviewerPersonId = builder.equal(rootProposalReview.get("completeReviewerPersonId"), personId);
		Predicate predicateHasRecommendation = builder.equal(rootProposalReview.get("hasRecommendation"), true);
		Predicate predicatePersonId = builder.or(predicateReviewerPersonId, predicateCompleteReviewerPersonId);
		query.where(builder.and(predicateProposalId, predicateReviewStatusCode, predicateHasRecommendation, predicatePersonId));
		List<ProposalReview> proposalReviews = session.createQuery(query).list();
		if (proposalReviews != null && !proposalReviews.isEmpty()) {
			return true;
		}
		return false;
	}

	@Override
	public ProposalEvaluationStatusFlow getProposalEvaluationDetails(ProposalReview proposalReview) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalEvaluationStatusFlow> query = builder.createQuery(ProposalEvaluationStatusFlow.class);
		Root<ProposalEvaluationStatusFlow> rootProposalEvaluationStatusFlow = query
				.from(ProposalEvaluationStatusFlow.class);
		Predicate predicateRoleId = builder.equal(rootProposalEvaluationStatusFlow.get(ROLE_ID),
				proposalReview.getRoleId());
		Predicate predicateStopNumber = builder.equal(rootProposalEvaluationStatusFlow.get("stopNumber"),
				proposalReview.getEvaluationStop().getStopNumber());
		Predicate predicateStatusCode = builder.equal(rootProposalEvaluationStatusFlow.get(REVIEW_STATUS_CODE),
				proposalReview.getReviewStatusCode());
		query.where(builder.and(predicateRoleId, predicateStopNumber, predicateStatusCode));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<EvaluationRecommendation> getAllEvaluationRecomendation() {
		return hibernateTemplate.loadAll(EvaluationRecommendation.class);
	}

	@Override
	public List<FinalEvaluationStatus> getFinalEvaluationStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<FinalEvaluationStatus> query = builder.createQuery(FinalEvaluationStatus.class);
		Root<FinalEvaluationStatus> rootFinalEvaluationStatus = query.from(FinalEvaluationStatus.class);
		query.orderBy(builder.asc(rootFinalEvaluationStatus.get("finalEvaluationStatusCode")));
		return session.createQuery(query).list();
	}

	@Override
	public EvaluationStop fetchEvaluationStopDetails(String rcbfProposal, Integer statusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<EvaluationStop> query = builder.createQuery(EvaluationStop.class);
		Root<EvaluationStop> rootEvaluationStop = query.from(EvaluationStop.class);
		Predicate predicateActivityTypeCode = builder.equal(rootEvaluationStop.get(ACTIVITY_TYPE_CODE), rcbfProposal);
		Predicate predicateStatusCode = builder.equal(rootEvaluationStop.get(STATUS_CODE), statusCode);
		query.where(builder.and(predicateActivityTypeCode, predicateStatusCode));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public EvaluationRecommendation getEvaluationRecommendationByStatusCode(Integer statusCode) {
		return hibernateTemplate.get(EvaluationRecommendation.class, statusCode);
	}

	@Override
	public List<ProposalReview> fetchRevisionRequestedReview(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> review = query.from(ProposalReview.class);
		Predicate predicateProposalId = builder.equal(review.get(PROPOSAL_ID), proposalId);
		Predicate predicateReviewStatusCode = builder.equal(review.get(REVIEW_STATUS_CODE), Constants.PROPOSAL_REVIEW_STATUS_REVISION);
		query.where(builder.and(predicateProposalId, predicateReviewStatusCode));
		return session.createQuery(query).list();
	}

	/*@Override
	public List<EvaluationStop> getReviewStopEvaluvationBasedOnProposalStatus(Integer statusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<EvaluationStop> query = builder.createQuery(EvaluationStop.class);
		Root<EvaluationStop> rootReviewStop = query.from(EvaluationStop.class);
		Predicate predicateOne = builder.equal(rootReviewStop.get(STATUS_CODE), statusCode);
		query.groupBy(rootReviewStop.get(ROLE_ID));
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getResultList();
	}*/

	@Override
	public List<EvaluationStop> getReviewStopEvaluvation(Integer statusCode, String activityTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<EvaluationStop> query = builder.createQuery(EvaluationStop.class);
		Root<EvaluationStop> rootReviewStop = query.from(EvaluationStop.class);
		Predicate predicateOne = builder.equal(rootReviewStop.get(STATUS_CODE), statusCode);
		Predicate predicateTwo = builder.equal(rootReviewStop.get(ACTIVITY_TYPE_CODE), activityTypeCode);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getResultList();
	}

	@Override
	public EvaluationStop fetchEvaluationStopForReviewer(Integer statusCode, String activityTypeCode, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<EvaluationStop> query = builder.createQuery(EvaluationStop.class);
		Root<EvaluationStop> rootReviewStop = query.from(EvaluationStop.class);
		Predicate predicateOne = builder.equal(rootReviewStop.get(STATUS_CODE), statusCode);
		Predicate predicateTwo = builder.equal(rootReviewStop.get(ACTIVITY_TYPE_CODE), activityTypeCode);
		Predicate predicateThree = builder.equal(rootReviewStop.get(ROLE_ID), roleId);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public Boolean checkPersonHasReview(String reviewerPersonId, Integer proposalId, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> rootProposalReview = query.from(ProposalReview.class);
		Predicate predicate1 = builder.equal(rootProposalReview.get(PROPOSAL_ID), proposalId);
		Predicate predicate2 = builder.equal(rootProposalReview.get(REVIEW_STATUS_CODE),
				Constants.PROPOSAL_REVIEW_STATUS_INPROGRESS);
		Predicate predicate3 = builder.equal(rootProposalReview.get(REVIEWER_PERSON_ID), reviewerPersonId);
		Predicate predicate4 = builder.equal(rootProposalReview.get(ROLE_ID), roleId);
		query.where(builder.and(predicate1, predicate2, predicate4, predicate3));
		List<ProposalReview> proposalReviews = session.createQuery(query).list();
		if (proposalReviews != null && !proposalReviews.isEmpty()) {
			return true;
		}
		return false;
	}

	@Override
	public ProposalEvaluationPanel saveProposalEvaluationPanelDetails(ProposalEvaluationPanel evaluationPanel) {
		hibernateTemplate.saveOrUpdate(evaluationPanel);
		return evaluationPanel;
	}

	@Override
	public List<ProposalEvaluationPanel> fetchProposalEvaluationPanelsByProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalEvaluationPanel> proposalEvaluationPanel = builder.createQuery(ProposalEvaluationPanel.class);
		Root<ProposalEvaluationPanel> rootGrantCallPanel = proposalEvaluationPanel.from(ProposalEvaluationPanel.class);
		proposalEvaluationPanel.where(builder.equal(rootGrantCallPanel.get(PROPOSAL_ID), proposalId));
		return session.createQuery(proposalEvaluationPanel).getResultList();
	}

	@Override
	public void deleteProposalEvaluationPanels(List<ProposalEvaluationPanel> proposalEvaluationPanels) {
		hibernateTemplate.deleteAll(proposalEvaluationPanels);
	}

	@Override
	public boolean buildEvaluationPanel(String moduleItemKey, Integer moduleCode, String personId, String updateUser, String actionType, String subModuleItemKey, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			statement = connection.prepareCall("{ ? = call FN_RULE_BUILD_EVALUATION_PANEL(?,?,?,?,?,?,?) }");
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, moduleItemKey);
			statement.setInt(3, moduleCode);
			statement.setString(4, personId);
			statement.setString(5, updateUser);
			statement.setString(6, actionType);
			statement.setInt(7, subModuleCode);
			statement.setString(8, subModuleItemKey);
			statement.execute();
			int result = statement.getInt(1);
			if (result == 1) {
				return true;
			}
		} catch (SQLException e) {
			logger.error("exception in buildEvaluationPanel: {} ", e.getMessage());
		}
		return false;
	}

	@Override
	public Integer getMaxApproverNumber(Integer panelEvaluationId) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(ProposalEvaluationPanelPersons.class);
		criteria.setProjection(Projections.max("approverNumber"));
		criteria.add(Restrictions.eq("proposalEvaluationPanel.proposalEvaluationId", panelEvaluationId));
		Integer getMaxApproverNumber = (Integer)criteria.uniqueResult();
		if (criteria.uniqueResult() != null) {
			return getMaxApproverNumber;
		} else {
			return 0;
		}
	}

	@Override
	public ProposalEvaluationPanel fetchProposalEvaluationPanelById(Integer proposalEvaluationPanelId) {
			return hibernateTemplate.get(ProposalEvaluationPanel.class, proposalEvaluationPanelId);
		}

	@Override
	public ProposalEvaluationPanelPersons saveEvaluationPanelPerson(ProposalEvaluationPanelPersons proposalEvaluationPanelPerson) {
        hibernateTemplate.saveOrUpdate(proposalEvaluationPanelPerson);
		return proposalEvaluationPanelPerson;
	}

	@Override
	public ProposalEvaluationPanelPersons fetchProposalEvaluationPanelPersonById(Integer proposalEvaluationPanelPersonId) {
			return hibernateTemplate.get(ProposalEvaluationPanelPersons.class, proposalEvaluationPanelPersonId);
		}

	@Override
	public void deleteProposalEvaluationPanelPerson(ProposalEvaluationPanelPersons proposalEvaluationPanelPerson) {
		try {
			hibernateTemplate.delete(proposalEvaluationPanelPerson);
		} catch (Exception e) {
			logger.error("exception : {} ", e.getMessage());					
		}	
	}

	@Override
	public List<Integer> getEvaluationRolesBasedOnProposalStatusAndActivityTypeCode(Integer statusCode, String activityTypeCode) {
		List<Integer> roleIds = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<EvaluationStop> rootReviewStop = query.from(EvaluationStop.class);
		Predicate predicateOne = builder.equal(rootReviewStop.get("statusCode"), statusCode);
		Predicate predicateTwo = builder.equal(rootReviewStop.get("activityTypeCode"), activityTypeCode);
		query.where(builder.and(predicateOne, predicateTwo));
		query.select(rootReviewStop.get("roleId")).distinct(true);
		roleIds = session.createQuery(query).getResultList();
		return roleIds;
	}

	@Override
	public List<Role> getRolesBasedonRolesIds(Set<Integer> roleIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Role> query = builder.createQuery(Role.class);
		Root<Role> rootRoles = query.from(Role.class);
		Predicate predicateOne = rootRoles.get("roleId").in(roleIds);
		query.groupBy(rootRoles.get("roleId"));
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getResultList();

	}

	@Override
	public ResultSet getRemovedPersonListFromEvaluation(String moduleItemKey, Integer moduleCode, String submoduleItemKey, Integer submoduleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_EVAL_DELETED_PERSON_LIST(?,?,?,?)}");
				statement.setString(1, moduleItemKey);
				statement.setInt(2, moduleCode);
				statement.setInt(3, submoduleCode);
				statement.setString(4, submoduleItemKey);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_EVAL_DELETED_PERSON_LIST";
				String functionCall = "{call " + procedureName + "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, moduleItemKey);
				statement.setInt(3, moduleCode);
				statement.setInt(4, submoduleCode);
				statement.setString(5, submoduleItemKey);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(1);
			}
			return resultSet;
		} catch (SQLException e) {
			logger.error("exception in getRemovedPersonListFromEvaluation : {}", e.getMessage());
		}
		return resultSet;
	}

}
