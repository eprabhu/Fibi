package com.polus.fibicomp.globalentity.dao;

import java.util.List;

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

import com.polus.fibicomp.globalentity.pojo.EntityActionLog;
import com.polus.fibicomp.globalentity.pojo.EntityActionType;

@Repository
@Primary
@Transactional
public class EntityActionLogDaoImpl implements EntityActionLogDao {

    protected static Logger logger = LogManager.getLogger(EntityActionLogDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Override
    public void saveObject(Object e) {
        hibernateTemplate.saveOrUpdate(e);
    }

    @Override
	public EntityActionType getEntityActionType(String actionLogTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder criteriaBuilder = session.getCriteriaBuilder();
		CriteriaQuery<EntityActionType> criteriaQuery = criteriaBuilder.createQuery(EntityActionType.class);
		Root<EntityActionType> root = criteriaQuery.from(EntityActionType.class);
		criteriaQuery.select(root).where(criteriaBuilder.equal(root.get("actionTypeCode"), actionLogTypeCode));
		return session.createQuery(criteriaQuery).getSingleResult();

	}

    @Override
	public List<EntityActionLog> fetchAllEntityActionLog(Integer entityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder criteriaBuilder = session.getCriteriaBuilder();
		CriteriaQuery<EntityActionLog> criteriaQuery = criteriaBuilder.createQuery(EntityActionLog.class);
		Root<EntityActionLog> root = criteriaQuery.from(EntityActionLog.class);
		criteriaQuery.select(root).where(criteriaBuilder.equal(root.get("entityId"), entityId))
				.orderBy(criteriaBuilder.desc(root.get("updateTimestamp")));
		return session.createQuery(criteriaQuery).getResultList();
	}

}
