package com.polus.fibicomp.opa.dao;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.opa.pojo.OPAReview;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.Query;
import java.sql.Timestamp;
import java.util.List;

@Repository
@Transactional
public class OPAReviewDaoImpl implements OPAReviewDao {

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

    @Autowired
    private PersonDao personDao;

    protected static Logger logger = LogManager.getLogger(OPADaoImpl.class.getName());

    @Override
    public void saveOrUpdate(Object entity) {
        hibernateTemplate.saveOrUpdate(entity);
    }

    @Override
    public Timestamp updateOPAReview(OPAReview opaReview) {
        Timestamp timesStamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPAReview r SET r.assigneePersonId = :assigneePersonId, ");
        hqlQuery.append("r.adminGroupId = :adminGroupId, r.reviewStatusTypeCode = :reviewStatusTypeCode, ");
        hqlQuery.append("r.locationTypeCode = :locationTypeCode, ");
        hqlQuery.append("r.startDate = :startDate, r.endDate = :endDate, ");
        hqlQuery.append("r.updateTimestamp = :updateTimestamp, r.updateUser = :updateUser ");
        hqlQuery.append("WHERE r.opaReviewId = :opaReviewId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("assigneePersonId", opaReview.getAssigneePersonId());
        query.setParameter("adminGroupId", opaReview.getAdminGroupId());
        query.setParameter("reviewStatusTypeCode", opaReview.getReviewStatusTypeCode());
        query.setParameter("locationTypeCode", opaReview.getLocationTypeCode());
        query.setParameter("startDate", opaReview.getStartDate());
        query.setParameter("endDate", opaReview.getEndDate());
        query.setParameter("updateTimestamp", timesStamp);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.setParameter("opaReviewId", opaReview.getOpaReviewId());
        query.executeUpdate();
        return timesStamp;
    }

    @Override
    public Long numberOfReviewOfStatuesIn(Integer opaDisclosureId, List<String> reviewStatusTypeCodes) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT count(r.opaReviewId) FROM  OPAReview r ");
        hqlQuery.append("WHERE r.opaDisclosureId = :opaDisclosureId AND r.reviewStatusTypeCode IN :reviewStatusTypeCode");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId", opaDisclosureId);
        query.setParameter("reviewStatusTypeCode", reviewStatusTypeCodes);
        return (Long) query.getSingleResult();
    }

    @Override
    public List<OPAReview> fetchAllOPAReviewByDisId(Integer opaDisclosureId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT r FROM  OPAReview r ");
        hqlQuery.append("WHERE r.opaDisclosureId = :opaDisclosureId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaDisclosureId", opaDisclosureId);
        return query.getResultList();
    }

    @Override
    public Timestamp updateReviewStatus(Integer opaReviewId, String reviewStatus) {
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("UPDATE OPAReview r SET r.reviewStatusTypeCode = :reviewStatusTypeCode, ");
        hqlQuery.append("r.updateTimestamp = :updateTimestamp, r.updateUser = :updateUser ");
        hqlQuery.append("WHERE r.opaReviewId = :opaReviewId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("reviewStatusTypeCode", reviewStatus);
        query.setParameter("opaReviewId", opaReviewId);
        query.setParameter("updateUser", AuthenticatedUser.getLoginUserName());
        query.setParameter("updateTimestamp", updateTimestamp);
        query.executeUpdate();
        return updateTimestamp;
    }

    @Override
    public OPAReview getOPAReview(Integer opaReviewId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT r FROM  OPAReview r ");
        hqlQuery.append("WHERE r.opaReviewId = :opaReviewId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaReviewId", opaReviewId);
        List<OPAReview> resultData = query.getResultList();
        if(resultData != null  && !resultData.isEmpty()) {
            return resultData.get(0);
        }
        return null;
    }

    @Override
    public void deleteOPAReview(Integer opaReviewId) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("DELETE FROM OPAReview r ");
        hqlQuery.append("WHERE r.opaReviewId = :opaReviewId");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("opaReviewId", opaReviewId);
        query.executeUpdate();
    }
}
