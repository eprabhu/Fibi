package com.polus.fibicomp.grantcall.dao;

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.pojo.ScoringCriteria;

@Transactional
@Service(value = "grantCallScoringDao")
public class GrantCallScoringDaoImpl implements GrantCallScoringDao {

	protected static Logger logger = LogManager.getLogger(GrantCallScoringDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<ScoringCriteria> fetchAllScoringCriteria() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ScoringCriteria> query = builder.createQuery(ScoringCriteria.class);
		Root<ScoringCriteria> rootAgreementSponsorType = query.from(ScoringCriteria.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<GrantCallScoringCriteria> fetchScoringCriteriaGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallScoringCriteria> query = builder.createQuery(GrantCallScoringCriteria.class);
		Root<GrantCallScoringCriteria> rootGrantCallScoringCriteria = query.from(GrantCallScoringCriteria.class);
		query.where(builder.equal(rootGrantCallScoringCriteria.get("grantCallId"), grantCallId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallScoringCriteria saveOrUpdateGrantCallScoringCriteria(GrantCallScoringCriteria grantCallScoringCriteria) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallScoringCriteria);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallScoringCriteria : {}", e.getMessage());
		}
		return grantCallScoringCriteria;
	}

	@Override
	public String deleteGrantCallScoringCriteria(Integer grantScoringCriteriaId) {
		hibernateTemplate.delete(hibernateTemplate.get(GrantCallScoringCriteria.class, grantScoringCriteriaId));
		return "GrantCall Scoring Criteria deleted successfully";
	}

}
