package com.polus.fibicomp.reviewcomments.dao;

import com.polus.fibicomp.coi.dao.ConflictOfInterestDaoImpl;
import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.reviewcomments.dto.ReviewCommentsDto;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

@Repository
@Transactional
public class ReviewCommentDaoImpl implements ReviewCommentDao {

    protected static Logger logger = LogManager.getLogger(ReviewCommentDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;


    @Override
    public void saveObject(Object object) {
        hibernateTemplate.saveOrUpdate(object);
    }

    @Override
    public List<DisclComment> fetchReviewComments(ReviewCommentsDto reviewCommentsDto) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT rc FROM DisclComment rc WHERE rc.moduleCode = :moduleCode ");
        if (reviewCommentsDto.getSubModuleCode() != null) {
            hqlQuery.append("AND rc.subModuleCode = :subModuleCode ");
        }
        if (reviewCommentsDto.getModuleItemKey() != null) {
            hqlQuery.append("AND rc.moduleItemKey = :moduleItemKey ");
        } else {
            hqlQuery.append("AND rc.moduleItemNumber = :moduleItemNumber ");
        }
        if (reviewCommentsDto.getSubModuleItemKey() != null) {
            hqlQuery.append("AND rc.subModuleItemKey = :subModuleItemKey ");
        } else if (reviewCommentsDto.getSubModuleItemNumber() != null) {
            hqlQuery.append("AND rc.subModuleItemNumber = :subModuleItemNumber ");
        }
        if (reviewCommentsDto.getIsPrivate() != null) {
            hqlQuery.append("AND rc.isPrivate = :isPrivate ");
        }
        if (reviewCommentsDto.getCommentTypeCode() != null) {
            hqlQuery.append("AND rc.commentTypeCode = :commentTypeCode ");
        }
        if (reviewCommentsDto.getComponentTypeCode() != null) {
            hqlQuery.append("AND rc.componentTypeCode = :componentTypeCode ");
        } else {
            hqlQuery.append("AND rc.componentTypeCode NOT IN :componentTypeCodes ");
        }
        if (reviewCommentsDto.getFormBuilderId() != null) {
            hqlQuery.append("AND rc.formBuilderId = :formBuilderId ");
        }
        if (reviewCommentsDto.getFormBuilderSectionId() != null) {
            hqlQuery.append("AND rc.formBuilderSectionId = :formBuilderSectionId ");
            if (reviewCommentsDto.getFormBuilderComponentId() == null) {
                hqlQuery.append("AND rc.formBuilderComponentId IS NULL ");
            }
        }
        if (reviewCommentsDto.getFormBuilderComponentId() != null) {
            hqlQuery.append("AND rc.formBuilderComponentId = :formBuilderComponentId ");
        }
        logger.info("Fetch Review Comments : HQL : " + hqlQuery);
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("moduleCode", reviewCommentsDto.getModuleCode());
        if (reviewCommentsDto.getSubModuleCode() != null) {
            query.setParameter("subModuleCode", reviewCommentsDto.getSubModuleCode());
        }
        if (reviewCommentsDto.getModuleItemKey() != null) {
            query.setParameter("moduleItemKey", reviewCommentsDto.getModuleItemKey());
        } else {
            query.setParameter("moduleItemNumber", reviewCommentsDto.getModuleItemNumber());
        }
        if (reviewCommentsDto.getSubModuleItemKey() != null) {
            query.setParameter("subModuleItemKey", reviewCommentsDto.getSubModuleItemKey());
        } else if (reviewCommentsDto.getSubModuleItemNumber() != null) {
            query.setParameter("subModuleItemNumber", reviewCommentsDto.getSubModuleItemNumber());
        }
        if (reviewCommentsDto.getIsPrivate() != null) {
            query.setParameter("isPrivate", reviewCommentsDto.getIsPrivate());
        }
        if (reviewCommentsDto.getCommentTypeCode() != null) {
            query.setParameter("commentTypeCode", reviewCommentsDto.getCommentTypeCode());
        }
        if (reviewCommentsDto.getComponentTypeCode() != null) {
            query.setParameter("componentTypeCode", reviewCommentsDto.getComponentTypeCode());
        } else {
            query.setParameter("componentTypeCodes", Arrays.asList(Constants.COI_DISCL_CONFLICT_RELATION_COMPONENT_TYPE,
                    Constants.COI_TRAVEL_DISCL_CONFLICT_RELATION_COMPONENT_TYPE));
        }
        if (reviewCommentsDto.getFormBuilderId() != null) {
            query.setParameter("formBuilderId", reviewCommentsDto.getFormBuilderId());
        }
        if (reviewCommentsDto.getFormBuilderSectionId() != null) {
            query.setParameter("formBuilderSectionId", reviewCommentsDto.getFormBuilderSectionId());
        }
        if (reviewCommentsDto.getFormBuilderComponentId() != null) {
            query.setParameter("formBuilderComponentId", reviewCommentsDto.getFormBuilderComponentId());
        }
        return query.getResultList();
    }

    @Override
    public void deleteReviewComment(Integer commentId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("DELETE FROM DisclComment rc WHERE rc.parentCommentId = :commentId OR rc.commentId = :commentId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("commentId", commentId);
        query.executeUpdate();
    }

    @Override
    public List<DisclAttachment> loadDisclAttachmentByCommentId(Integer commentId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT da  FROM DisclAttachment da WHERE da.commentId = :commentId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("commentId", commentId);
        return query.getResultList();
    }

    @Override
    public List<Integer> getAllChildCommentId(Integer commentId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT dc.commentId FROM DisclComment dc WHERE dc.parentCommentId = :parentCommentId ");
        org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery.toString());
        query.setParameter("parentCommentId", commentId);
        return query.getResultList();
    }

    @Override
    public DisclComment fetchReviewCommentByCommentId(Integer commentId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT rc FROM DisclComment rc WHERE rc.commentId = :commentId ");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("commentId", commentId);
        List<DisclComment> resultList = query.getResultList();
        if (!resultList.isEmpty()) {
            return resultList.get(0);
        }
        return null;
    }

}
