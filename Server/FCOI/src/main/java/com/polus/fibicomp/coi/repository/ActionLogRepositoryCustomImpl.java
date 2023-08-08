package com.polus.fibicomp.coi.repository;

import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dao.GeneralDaoImpl;
import com.polus.fibicomp.coi.pojo.DisclosureActionLog;
import com.polus.fibicomp.coi.pojo.EntityActionLog;
import com.polus.fibicomp.coi.pojo.TravelDisclosureActionLog;

@Repository
@Primary
@Transactional
public class ActionLogRepositoryCustomImpl implements ActionLogRepositoryCustom{

    protected static Logger logger = LogManager.getLogger(GeneralDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Override
    public List<EntityActionLog> fetchEntityActionLog(Integer entityId, String actionTypeCode) {
        StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT ea FROM EntityActionLog ea WHERE ea.entityId = :entityId AND ea.actionTypeCode = :actionTypeCode ORDER BY updateTimestamp DESC");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("actionTypeCode", actionTypeCode);
        query.setParameter("entityId", entityId);
        return query.getResultList();
    }

    @Override
	public List<DisclosureActionLog> fetchDisclosureActionLogsBasedOnDisclosureId(Integer disclosureId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<DisclosureActionLog> query = builder.createQuery(DisclosureActionLog.class);
		Root<DisclosureActionLog> root = query.from(DisclosureActionLog.class);
        query.where(builder.equal(root.get("disclosureId"), disclosureId));
        query.orderBy(builder.desc(root.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}
    
    @Override
	public List<TravelDisclosureActionLog> fetchTravelDisclosureActionLog(Integer travelDisclosureId, String actionTypeCode) {
		StringBuilder hqlQuery = new StringBuilder();
        Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
        hqlQuery.append("SELECT tda FROM TravelDisclosureActionLog tda WHERE tda.travelDisclosureId = :travelDisclosureId AND tda.actionTypeCode = :actionTypeCode ORDER BY updateTimestamp DESC");
        Query query = session.createQuery(hqlQuery.toString());
        query.setParameter("actionTypeCode", actionTypeCode);
        query.setParameter("entityId", travelDisclosureId);
        return query.getResultList();
	}

}
