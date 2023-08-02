package com.polus.fibicomp.coi.repository;

import com.polus.fibicomp.coi.dao.GeneralDaoImpl;
import com.polus.fibicomp.coi.pojo.EntityActionLog;
import com.polus.fibicomp.common.dao.CommonDao;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.Query;
import java.util.List;

@Repository
@Transactional
public class ActionLogRepositoryCustomImpl implements ActionLogRepositoryCustom{

    protected static Logger logger = LogManager.getLogger(GeneralDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

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
}
