package com.polus.fibicomp.grantcall.dao;

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPICriteria;
import com.polus.fibicomp.grantcall.pojo.KPIType;

@Transactional
@Service(value = "grantCallKPIDao")
public class GrantCallKPIDaoImpl implements GrantCallKPIDao {

	protected static Logger logger = LogManager.getLogger(GrantCallKPIDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<KPIType> fetchAllKPIs() {
		return hibernateTemplate.loadAll(KPIType.class);		
	}

	@Override
	public List<GrantCallKPI> fetchKPIByGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallKPI> query = builder.createQuery(GrantCallKPI.class);
		Root<GrantCallKPI> rootGrantCallKpi = query.from(GrantCallKPI.class);
		query.where(builder.equal(rootGrantCallKpi.get("grantCall").get("grantCallId"), grantCallId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallKPI saveOrUpdateGrantCallKPI(GrantCallKPI grantCallKpi) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallKpi);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateGrantCallKPI : {}", e.getMessage());
		}
		return grantCallKpi;
	}


	@Override
	public List<GrantCallKPICriteria> fetchgrantCallKPICriteria(Integer grantCallKpiId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallKPICriteria> query = builder.createQuery(GrantCallKPICriteria.class);
		Root<GrantCallKPICriteria> rootgrantCallKpiCriteria = query.from(GrantCallKPICriteria.class);
		query.where(builder.equal(rootgrantCallKpiCriteria.get("grantCallKpi").get("grantCallKpiId"), grantCallKpiId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String deleteGrantCallKPI(Integer grantCallId, Integer grantCallKpiId, Integer grantCallKpiCriteriaId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallKPI> query = builder.createQuery(GrantCallKPI.class);
		Root<GrantCallKPI> rootGrantCallKPI = query.from(GrantCallKPI.class);
		Predicate predicate1 = builder.equal(rootGrantCallKPI.get("grantCallId"), grantCallId);
		query.where(builder.and(predicate1));
		if (grantCallKpiCriteriaId == null && grantCallKpiId == null && grantCallId != null) {
			List<GrantCallKPI> grantCallKPI = session.createQuery(query).getResultList();
			hibernateTemplate.deleteAll(grantCallKPI);
		} else if (grantCallKpiId != null && grantCallKpiCriteriaId == null && grantCallId != null) {
			hibernateTemplate.delete(hibernateTemplate.get(GrantCallKPI.class, grantCallKpiId));
		} else if (grantCallKpiCriteriaId != null) {
			hibernateTemplate.delete(hibernateTemplate.get(GrantCallKPICriteria.class, grantCallKpiCriteriaId));
		}
		return "GrantCall KPI deleted successfully";
	}

	public GrantCallKPI deleteGrantCallKPI(GrantCallKPI grantCallKPI) {
		try {
			hibernateTemplate.delete(grantCallKPI);
		} catch (Exception e) {
			logger.error("exception in deleteGrantCallKPI : {}", e.getMessage());
		}
		return grantCallKPI;
	}

}
