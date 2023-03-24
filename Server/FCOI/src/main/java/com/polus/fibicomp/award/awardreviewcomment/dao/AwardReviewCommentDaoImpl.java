package com.polus.fibicomp.award.awardreviewcomment.dao;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardReviewComment;
import com.polus.fibicomp.constants.Constants;

@Transactional
@Service(value = "awardReviewCommentDao")
public class AwardReviewCommentDaoImpl implements AwardReviewCommentDao {

	protected static Logger logger = LogManager.getLogger(AwardReviewCommentDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private AwardDao awardDao;

	@Override
	public AwardReviewComment saveOrUpdateAwardReviewComment(AwardReviewComment awardReviewComment) {
		try {
			hibernateTemplate.saveOrUpdate(awardReviewComment);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateAwardReviewComment : {} ", e.getMessage());
		}
		return awardReviewComment;
	}

	@Override
	public AwardReviewComment getAwardReviewCommentById(Integer awardReviewCommentId) {
		return hibernateTemplate.get(AwardReviewComment.class, awardReviewCommentId);
	}

//	@Override
//	public List<AwardReviewComment> getAwardReviewCommentsByAwardId(Integer awardId, Boolean isViewPrivateComment, String userName) {
//		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
//		CriteriaBuilder builder = session.getCriteriaBuilder();
//		CriteriaQuery<AwardReviewComment> awardReviewComment = builder.createQuery(AwardReviewComment.class);
//		Root<AwardReviewComment> rootAwardReviewComment = awardReviewComment.from(AwardReviewComment.class);
//		Predicate awardIdPredicate = builder.equal(rootAwardReviewComment.get("awardId"), awardId);
//		if (userName != null && isViewPrivateComment != null) {
//			Predicate predicateUser = builder.equal(rootAwardReviewComment.get("updateUser"), userName);
//			Predicate predicateIsPrivate = builder.equal(rootAwardReviewComment.get("isPrivateComment"), Boolean.TRUE);
//			Predicate privateComments = builder.and(predicateIsPrivate, awardIdPredicate);
//			Predicate publicComments = builder.and(builder.not(predicateIsPrivate), awardIdPredicate);
//			if (Boolean.FALSE.equals(isViewPrivateComment)) {
//				privateComments = builder.and(privateComments, predicateUser);
//			}
//			awardReviewComment.where(builder.or(privateComments, publicComments));
//		} else {
//			awardReviewComment.where(builder.and(awardIdPredicate));
//		}
//		awardReviewComment.orderBy(builder.desc(rootAwardReviewComment.get("updateTimeStamp")));
//		return session.createQuery(awardReviewComment).getResultList();
//	}

	@Override
	public List<AwardReviewComment> getAwardReviewCommentsByAwardId(Integer awardId, Boolean isViewPrivateComment, String userName, String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardReviewComment> awardReviewComment = builder.createQuery(AwardReviewComment.class);
		Root<AwardReviewComment> rootAwardReviewComment = awardReviewComment.from(AwardReviewComment.class);
		Award activeAward = awardDao.fetchActiveAwardByAwardNumber(awardNumber);
		Predicate awardIdPredicate = builder.equal(rootAwardReviewComment.get("awardId"), awardId);
		if (activeAward != null && activeAward.getAwardId().equals(awardId)) {
			Subquery<String> subQuery = awardReviewComment.subquery(String.class);
			Root<Award> rootAward = subQuery.from(Award.class);
			Predicate predicateAwardNumber = builder.equal(rootAward.get("awardNumber"), awardNumber);
			Predicate predicateAwardSequenceStatus = builder.not(rootAward.get("awardSequenceStatus").in(Constants.AWARD_FINAL_STATUS_CANCELLED));
			subQuery.select(rootAward.get("awardId"));
			subQuery.where(builder.and(predicateAwardNumber, predicateAwardSequenceStatus));
			awardIdPredicate = builder.in(rootAwardReviewComment.get("awardId")).value(subQuery);
		}
		if (userName != null && isViewPrivateComment != null) {
			Predicate predicateUser = builder.equal(rootAwardReviewComment.get("updateUser"), userName);
			Predicate predicateIsPrivate = builder.equal(rootAwardReviewComment.get("isPrivateComment"), Boolean.TRUE);
			Predicate privateComments = builder.and(predicateIsPrivate, awardIdPredicate);
			Predicate publicComments = builder.and(builder.not(predicateIsPrivate), awardIdPredicate);
			if (Boolean.FALSE.equals(isViewPrivateComment)) {
				privateComments = builder.and(privateComments, predicateUser);
			}
			awardReviewComment.where(builder.or(privateComments, publicComments));
		} else {
			awardReviewComment.where(builder.and(awardIdPredicate));
		}
		awardReviewComment.orderBy(builder.desc(rootAwardReviewComment.get("updateTimeStamp")));
		return session.createQuery(awardReviewComment).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getReviewerPersonIdAndFullNameByAwardId(Integer awardId) {
		List<String> personIds = new ArrayList<String>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT distinct T1.reviewerPersonId FROM AwardReviewComment T1 WHERE T1.awardId = :awardId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		if (query.getResultList() != null) {
			personIds = query.getResultList();
		}
		return personIds;
	}

	@Override
	public String deleteAwardReviewComment(Integer awardReviewCommentId) {
		AwardReviewComment awardReviewComment = hibernateTemplate.get(AwardReviewComment.class, awardReviewCommentId);
		hibernateTemplate.delete(awardReviewComment);
		return "Comment deleted successfully";
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Integer> getChildAwardReviewCommentIdsByAwardReviewCommentId(Integer awardReviewCommentId) {
		List<Integer> allAwardReviewCommentIds = new ArrayList<Integer>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT T1.awardReviewCommentId FROM AwardReviewComment T1 WHERE T1.parentReviewCommentId = :parentReviewCommentId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("parentReviewCommentId", awardReviewCommentId);
		if (query.getResultList() != null) {
			allAwardReviewCommentIds = query.getResultList();
		}
		return allAwardReviewCommentIds;
	}

}
