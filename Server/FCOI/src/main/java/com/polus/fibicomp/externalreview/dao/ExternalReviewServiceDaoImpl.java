package com.polus.fibicomp.externalreview.dao;

import java.sql.Timestamp;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;

import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.externalreview.pojo.ExtReviewAttachmentFile;
import com.polus.fibicomp.externalreview.pojo.ExtReviewAttachmentType;
import com.polus.fibicomp.externalreview.pojo.ExtReviewAttachments;
import com.polus.fibicomp.externalreview.pojo.ExtReviewHistory;
import com.polus.fibicomp.externalreview.pojo.ExtReviewQuestionnaire;
import com.polus.fibicomp.externalreview.pojo.ExtReviewReviewer;
import com.polus.fibicomp.externalreview.pojo.ExtReviewScoringCriteria;
import com.polus.fibicomp.externalreview.pojo.ExtReviewServiceType;
import com.polus.fibicomp.externalreview.pojo.ExtReviewStatus;
import com.polus.fibicomp.externalreview.pojo.ExternalReview;

@Transactional
@Service
public class ExternalReviewServiceDaoImpl implements ExternalReviewServiceDao {

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Override
    public void saveQuestionnaire(ExtReviewQuestionnaire extReviewQuestionnaire) {
        hibernateTemplate.saveOrUpdate(extReviewQuestionnaire);
    }

    @Override
    public void deleteQuestionnaire(Integer extReviewQuestionnaireId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("DELETE FROM ExtReviewQuestionnaire que WHERE que.extReviewQuestionnaireId = :extReviewQuestionnaireId");
        query.setParameter("extReviewQuestionnaireId", extReviewQuestionnaireId);
        query.executeUpdate();
    }

    @Override
    public List<ExtReviewQuestionnaire> fetchExtQuestionnaires(Integer extReviewId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT que FROM ExtReviewQuestionnaire que WHERE que.extReviewID = :extReviewID");
        query.setParameter("extReviewID", extReviewId);
        return query.getResultList();
    }

    @Override
    public List<ExtReviewServiceType> fetchExtReviewServiceTypes() {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT sType FROM ExtReviewServiceType sType WHERE sType.isActive = 'Y'");
        return query.getResultList();
    }

    @Override
    public List<ExtReviewStatus> fetchExtReviewStatus() {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT status FROM ExtReviewStatus status WHERE status.isActive = 'Y'");
        return query.getResultList();
    }

    @Override
    public void saveExternalReview(ExternalReview externalReview) {
        hibernateTemplate.saveOrUpdate(externalReview);
    }

    @Override
    public List<ExternalReview> fetchExtReviewByDetails(ExternalReview externalReview) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder queryBuilder = new StringBuilder("SELECT extR FROM ExternalReview extR WHERE ");
        queryBuilder.append("extR.moduleItemCode = :moduleItemCode AND ");
        queryBuilder.append("extR.moduleSubItemCode = :moduleSubItemCode AND  ");
        queryBuilder.append("extR.moduleSubItemKey = :moduleSubItemKey AND ");
        queryBuilder.append("extR.moduleItemKey = :moduleItemKey ORDER BY extR.extReviewID DESC");
        Query query = session.createQuery(queryBuilder.toString());
        query.setParameter("moduleItemCode", externalReview.getModuleItemCode());
        query.setParameter("moduleSubItemCode", externalReview.getModuleSubItemCode());
        query.setParameter("moduleItemKey", externalReview.getModuleItemKey());
        query.setParameter("moduleSubItemKey", externalReview.getModuleSubItemKey());
        return query.getResultList();
    }

    @Override
    public void saveExtReviewScoringCriteria(ExtReviewScoringCriteria scoringCriteria) {
        hibernateTemplate.saveOrUpdate(scoringCriteria);
    }

    @Override
    public List<ExtReviewScoringCriteria> fetchExtReviewScoringCriteria(Integer extReviewID) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT sCriteria FROM ExtReviewScoringCriteria sCriteria WHERE sCriteria.extReviewID = :extReviewID");
        query.setParameter("extReviewID", extReviewID);
        return query.getResultList();
    }

    @Override
    public void saveExtReviewAttachmentFile(ExtReviewAttachmentFile extReviewAttachmentFile) {
        hibernateTemplate.saveOrUpdate(extReviewAttachmentFile);
    }

    @Override
    public void saveExtReviewAttachment(ExtReviewAttachments extReviewAttachment) {
        hibernateTemplate.saveOrUpdate(extReviewAttachment);
    }

    @Override
    public List<ExtReviewAttachments> fetchExtReviewAttachments(Integer extReviewID) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT attachment FROM ExtReviewAttachments attachment WHERE attachment.extReviewID = :extReviewID");
        query.setParameter("extReviewID", extReviewID);
        return query.getResultList();
    }

    @Override
    public List<ExtReviewAttachmentType> fetchExtReviewAttachmentTypes() {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT type FROM ExtReviewAttachmentType type");
        return query.getResultList();
    }

    @Override
    public ExtReviewAttachments fetchExtReviewAttachmentById(Integer extReviewAttachmentId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT attachment FROM ExtReviewAttachments attachment WHERE attachment.extReviewAttachmentId = :extReviewAttachmentId");
        query.setParameter("extReviewAttachmentId", extReviewAttachmentId);
        return (ExtReviewAttachments) query.getSingleResult();
    }

    @Override
    public void deleteExtReviewAttachment(Integer extReviewAttachmentId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("DELETE FROM ExtReviewAttachments attachment WHERE attachment.extReviewAttachmentId = :extReviewAttachmentId");
        query.setParameter("extReviewAttachmentId", extReviewAttachmentId);
        query.executeUpdate();
    }

    @Override
    public void deleteExtReviewAttachmentFile(String fileDataId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("DELETE FROM ExtReviewAttachmentFile attachment WHERE attachment.fileDataId = :fileDataId");
        query.setParameter("fileDataId", fileDataId);
        query.executeUpdate();
    }

    @Override
    public ExtReviewAttachmentType fetchExtReviewAttachmentTypeByID(Integer attachmentTypeCode) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT type FROM ExtReviewAttachmentType type WHERE type.attachmentTypeCode = :attachmentTypeCode");
        query.setParameter("attachmentTypeCode", attachmentTypeCode);
        return (ExtReviewAttachmentType) query.getSingleResult();
    }

    @Override
    public void updateExternalReviewStatus(Integer extReviewID, int sendForReviewStatusCode, Timestamp updateTimestamp, String updateUser, Integer preProposalExtReviewId) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder queryBuilder = new StringBuilder("UPDATE ExternalReview extR SET extR.extReviewStatusCode = :extReviewStatusCode, ");
        queryBuilder.append("extR.updateTimestamp = :updateTimestamp, extR.updateUser = :updateUser ");
        if (preProposalExtReviewId != null) {
        	queryBuilder.append(", extR.preProposalExtReviewId = :preProposalExtReviewId ");
        }
        queryBuilder.append("WHERE extR.extReviewID = :extReviewID");
        Query query = session.createQuery(queryBuilder.toString());
        query.setParameter("extReviewID", extReviewID);
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updateUser", updateUser);
        query.setParameter("extReviewStatusCode", sendForReviewStatusCode);
        if (preProposalExtReviewId != null) {
        	query.setParameter("preProposalExtReviewId", preProposalExtReviewId);
        }
        query.executeUpdate();
    }

    public ExtReviewStatus fetchExtReviewStatusByID(Integer extReviewStatusCode) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT status FROM ExtReviewStatus status WHERE status.extReviewStatusCode = :extReviewStatusCode");
        query.setParameter("extReviewStatusCode", extReviewStatusCode);
        return (ExtReviewStatus) query.getSingleResult();
    }

    @Override
    public ExternalReview fetchExtReviewByID(Integer extReviewID) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT extR FROM ExternalReview extR WHERE extR.extReviewID = :extReviewID");
        query.setParameter("extReviewID", extReviewID);
        return (ExternalReview) query.getSingleResult();
    }

    @Override
    public void updateExternalReview(ExternalReview externalReview) {
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        StringBuilder queryBuilder = new StringBuilder("UPDATE ExternalReview extR SET extR.extReviewServiceTypeCode = :extReviewServiceTypeCode, ");
        queryBuilder.append("extR.updateTimestamp = :updateTimestamp, extR.updateUser = :updateUser, ");
        queryBuilder.append("extR.description = :description, extR.deadlineDate = :deadlineDate ");
        queryBuilder.append("WHERE extR.extReviewID = :extReviewID");
        Query query = session.createQuery(queryBuilder.toString());
        query.setParameter("extReviewID", externalReview.getExtReviewID());
        query.setParameter("updateTimestamp", externalReview.getUpdateTimestamp());
        query.setParameter("updateUser", externalReview.getUpdateUser());
        query.setParameter("extReviewServiceTypeCode", externalReview.getExtReviewServiceTypeCode());
        query.setParameter("description", externalReview.getDescription());
        query.setParameter("deadlineDate", externalReview.getDeadlineDate());
        query.executeUpdate();
    }
    
	@Override
	public void deleteExtReviewScoringCriteria(Integer extReviewID) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<ExtReviewScoringCriteria> delete = builder.createCriteriaDelete(ExtReviewScoringCriteria.class);
		Root<ExtReviewScoringCriteria> root = delete.from(ExtReviewScoringCriteria.class);
		delete.where(builder.equal(root.get("extReviewID"), extReviewID));
		session.createQuery(delete).executeUpdate();
	}

	@SuppressWarnings({ "rawtypes" })
	@Override
	public Integer getPreProposalIdForProposal(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        Query query = session.createQuery("SELECT sourceProposalId FROM Proposal WHERE proposalId = :proposalId");
        query.setParameter("proposalId", proposalId);
        if (query.uniqueResult() == null) {
        	return null;
        }
        return (Integer) query.uniqueResult();
	}

	@SuppressWarnings("rawtypes")
	@Override
	public ExternalReview getLatestActivePreProposalExternalReviewBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM ExternalReview WHERE extReviewID = (select max(extReviewID) from ExternalReview where moduleItemKey =:moduleItemKey and extReviewStatusCode = :extReviewStatusCode and extReviewServiceTypeCode = :extReviewServiceTypeCode)";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("moduleItemKey", proposalId.toString());
		query.setParameter("extReviewStatusCode", Constants.EXT_REVIEW_STATUS_CODE_ACTIVE);
		query.setParameter("extReviewServiceTypeCode", Constants.EXTERNAL_REVIEW_TYPE_CODE_PRE_PROPOSAL);
		if (query.uniqueResult() == null) {
        	return null;
        }
		return (ExternalReview) query.uniqueResult();
	}

	@Override
	public List<ExtReviewReviewer> getExtReviewReviewersByExternalReviewId(Integer extReviewID) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ExtReviewReviewer> query = builder.createQuery(ExtReviewReviewer.class);
		Root<ExtReviewReviewer> extReviewReviewer = query.from(ExtReviewReviewer.class);
		Predicate predicateExtReviewId = builder.equal(extReviewReviewer.get("extReviewId"), extReviewID);
		query.where(builder.and(predicateExtReviewId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveOrUpdateExtReviewReviewer(ExtReviewReviewer reviewer) {
		hibernateTemplate.saveOrUpdate(reviewer);
	}

	@Override
	public void saveExtReviewHistory(ExtReviewHistory extReviewHistory) {
		hibernateTemplate.save(extReviewHistory);
	}

}
