package com.polus.fibicomp.prereview.dao;

import java.util.List;

import javax.persistence.LockModeType;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachment;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachmentFile;
import com.polus.fibicomp.prereview.pojo.PreReviewSectionType;
import com.polus.fibicomp.prereview.pojo.PreReviewStatus;
import com.polus.fibicomp.prereview.pojo.PreReviewType;
import com.polus.fibicomp.prereview.pojo.PreReviewer;
import com.polus.fibicomp.prereview.vo.PreReviewVO;

@Transactional
@Service(value = "proposalPreReviewDao")
public class PreReviewDaoImpl implements PreReviewDao {

	protected static Logger logger = LogManager.getLogger(PreReviewDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<PreReviewType> fetchAllPreReviewTypes() {
		return hibernateTemplate.loadAll(PreReviewType.class);
	}

	@Override
	public List<PreReviewStatus> fetchAllPreReviewStatus() {
		return hibernateTemplate.loadAll(PreReviewStatus.class);
	}

	@Override
	public PreReviewStatus getPreReviewStatusByCode(String statusCode) {
		return hibernateTemplate.get(PreReviewStatus.class, statusCode);
	}

	@Lock(LockModeType.PESSIMISTIC_WRITE)
	@Override
	public PreReview saveOrUpdatePreReview(PreReview preReview) {
		try {
			hibernateTemplate.saveOrUpdate(preReview);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return preReview;
	}

	@Override
	public List<PreReview> loadAllPreReviews(PreReview preReview) {
		Integer moduleItemCode = preReview.getModuleItemCode();
		Integer moduleSubItemCode = preReview.getModuleSubItemCode();
		String moduleItemKey = preReview.getModuleItemKey();
		String moduleSubItemKey = preReview.getModuleSubItemKey();
		String reviewTypeCode = preReview.getReviewTypeCode();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PreReview> query = builder.createQuery(PreReview.class);
		Root<PreReview> rootPreReview = query.from(PreReview.class);
		Predicate predicateOne = builder.equal(rootPreReview.get("moduleItemCode"), moduleItemCode);
		Predicate predicateTwo = builder.equal(rootPreReview.get("moduleItemKey"), moduleItemKey);
		Predicate predicateThree = builder.equal(rootPreReview.get("moduleSubItemCode"), moduleSubItemCode);
		Predicate predicateFour = builder.equal(rootPreReview.get("moduleSubItemKey"), moduleSubItemKey);
		Predicate predicate = builder.equal(rootPreReview.get("reviewTypeCode"), reviewTypeCode);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour, predicate));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<PreReview> fetchPreReviewsByCriteria(PreReview preReview) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PreReview> query = builder.createQuery(PreReview.class);
		Root<PreReview> rootPreReview = query.from(PreReview.class);
		Predicate predicateOne = builder.equal(rootPreReview.get("moduleItemCode"), preReview.getModuleItemCode());
		Predicate predicateTwo = builder.equal(rootPreReview.get("moduleItemKey"), preReview.getModuleItemKey());
		Predicate predicateThree = builder.equal(rootPreReview.get("moduleSubItemCode"), preReview.getModuleSubItemCode());
		Predicate predicateFour = builder.equal(rootPreReview.get("moduleSubItemKey"), preReview.getModuleSubItemKey());
		Predicate predicateFive = builder.equal(rootPreReview.get("reviewStatusCode"), Constants.PRE_REVIEW_STATUS_INPROGRESS);
		Predicate predicate = builder.equal(rootPreReview.get("reviewTypeCode"), preReview.getReviewTypeCode());
		Predicate predicateSix = builder.and(predicateOne, predicateTwo, predicateThree, predicateFour, predicateFive, predicate);
		if (preReview.getReviewerPersonId() != null) {
			Predicate predicateSeven = builder.equal(rootPreReview.get("reviewerPersonId"), preReview.getReviewerPersonId());
			Predicate predicateEight = builder.and(predicateSix, predicateSeven);
			query.where(predicateEight);
		} else {
			Predicate predicateEight = builder.and(predicateSix);
			query.where(predicateEight);
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public PreReviewAttachment fetchAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(PreReviewAttachment.class, attachmentId);
	}

	@Override
	public List<PreReviewer> fetchAllPreReviewer() {
		return hibernateTemplate.loadAll(PreReviewer.class);
	}

	@Override
	public List<PreReview> fetchSortedReviews(PreReviewVO preReviewVO) {
		Integer moduleItemCode = preReviewVO.getModuleItemCode();
		Integer moduleSubItemCode = preReviewVO.getModuleSubItemCode();
		String moduleItemKey = preReviewVO.getModuleItemKey();
		String moduleSubItemKey = preReviewVO.getModuleSubItemKey();
		String sortBy = preReviewVO.getSortBy();
		String reverse = preReviewVO.getReverse();
		String reviewTypeCode = preReviewVO.getReviewTypeCode();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PreReview> query = builder.createQuery(PreReview.class);
		Root<PreReview> reviewRoot = query.from(PreReview.class);
		Predicate predicate1 = builder.equal(reviewRoot.get("moduleItemCode"), moduleItemCode);
		Predicate predicate2 = builder.equal(reviewRoot.get("moduleSubItemCode"), moduleSubItemCode);
		Predicate predicate3 = builder.equal(reviewRoot.get("moduleItemKey"), moduleItemKey);
		Predicate predicate4 = builder.equal(reviewRoot.get("moduleSubItemKey"), moduleSubItemKey);
		Predicate predicate5 = builder.equal(reviewRoot.get("reviewTypeCode"), reviewTypeCode);
		query.where(builder.and(predicate1, predicate2, predicate3, predicate4, predicate5));
		if (sortBy.equals("reviewSectionTypeCode")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(reviewRoot.get("preReviewSectionType").get("description")));
			} else {
				query.orderBy(builder.desc(reviewRoot.get("preReviewSectionType").get("description")));
			}
		} else if (sortBy.equals("reviewerFullName")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(reviewRoot.get("reviewerFullName")));
			} else {
				query.orderBy(builder.desc(reviewRoot.get("reviewerFullName")));
			}
		} else if (sortBy.equals("reviewStatusCode")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(reviewRoot.get("preReviewStatus").get("description")));
			} else {
				query.orderBy(builder.desc(reviewRoot.get("preReviewStatus").get("description")));
			}
		} else if (sortBy.equals("requestDate")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(reviewRoot.get("requestDate")));
			} else {
				query.orderBy(builder.desc(reviewRoot.get("requestDate")));
			}
		} else if (sortBy.equals("completionDate")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(reviewRoot.get("completionDate")));
			} else {
				query.orderBy(builder.desc(reviewRoot.get("completionDate")));
			}
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<PreReviewSectionType> fetchAllPreReviewSectionTypes(String preReviewType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PreReviewSectionType> query = builder.createQuery(PreReviewSectionType.class);
		Root<PreReviewSectionType> reviewRoot = query.from(PreReviewSectionType.class);
		Predicate predicate1 = builder.equal(reviewRoot.get("preReviewTypeCode"), preReviewType);
		query.where(predicate1);
		query.orderBy(builder.asc(reviewRoot.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public PreReviewAttachmentFile saveFileData(PreReviewAttachmentFile fileData) {
		hibernateTemplate.save(fileData);
		return fileData;
	}

	@Override
	public PreReviewAttachmentFile getFileDataById(String fileDataId) {
		return hibernateTemplate.get(PreReviewAttachmentFile.class, fileDataId);
	}

	@Override
	public PreReview getPreReviewById(Integer preReviewId) {
		return hibernateTemplate.get(PreReview.class, preReviewId);
	}

	@Override
	public List<PreReview> loadPreReviewsBasedOnParams(Integer moduleItemCode, String moduleItemKey, String preReviewTypeCode, String preReviewStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PreReview> query = builder.createQuery(PreReview.class);
		Root<PreReview> reviewRoot = query.from(PreReview.class);
		Predicate predicate1 = builder.equal(reviewRoot.get("preReviewSectionType").get("preReviewTypeCode"), preReviewTypeCode);
		Predicate predicate2 = builder.equal(reviewRoot.get("moduleItemCode"), moduleItemCode);
		Predicate predicate3 = builder.equal(reviewRoot.get("moduleItemKey"), moduleItemKey);
		Predicate predicate4 = builder.equal(reviewRoot.get("reviewStatusCode"), preReviewStatusCode);
		query.where(predicate1, predicate2, predicate3, predicate4);
		return session.createQuery(query).getResultList();
	}

}
