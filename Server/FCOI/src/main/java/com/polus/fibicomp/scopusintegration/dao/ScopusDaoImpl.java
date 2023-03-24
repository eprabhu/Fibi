package com.polus.fibicomp.scopusintegration.dao;

import java.util.ArrayList;
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

import com.polus.fibicomp.scopusintegration.pojo.AwardScopus;
import com.polus.fibicomp.scopusintegration.pojo.Scopus;
import com.polus.fibicomp.scopusintegration.pojo.ScopusConfigurationData;

@Transactional
@Service(value = "scopusDao")
public class ScopusDaoImpl implements ScopusDao {

	protected static Logger logger = LogManager.getLogger(ScopusDaoImpl.class.getName());

	private static final String TITLE = "title";
	private static final String AWARD_ID = "awardId";

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public void saveOrUpdateScopusInfo(Scopus scopus) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			if (session.getTransaction().isActive() == false) {
				session.getTransaction().begin();
			}
			hibernateTemplate.saveOrUpdate(scopus);
			session.getTransaction().commit();
		} catch (Exception e) {
			// e.printStackTrace();
			logger.info("Exception in saving scopus from feed : Scopus ID {}", scopus.getScopusId());
		}
	}

	@Override
	public List<Scopus> findScopus(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Scopus> scopus = builder.createQuery(Scopus.class);
		Root<Scopus> rootScopus = scopus.from(Scopus.class);
		Predicate predicateOne = builder.like(rootScopus.get(TITLE), "%" + searchString.toLowerCase() + "%");
		Predicate predicateTwo = builder.like(rootScopus.get("scopusId"), "%" + searchString.toLowerCase() + "%");
		Predicate predicateThree = builder.like(rootScopus.get("creator"), "%" + searchString.toLowerCase() + "%");
		Predicate predicateDoi = builder.like(rootScopus.get("doi"), "%" + searchString.toLowerCase() + "%");
		scopus.where(builder.or(predicateOne, predicateTwo, predicateThree, predicateDoi));
		return session.createQuery(scopus).setMaxResults(25).getResultList();
	}

	@Override
	public AwardScopus saveOrUpdateAwardScopus(AwardScopus awardScopus) {
		try {
			hibernateTemplate.saveOrUpdate(awardScopus);
		} catch (Exception e) {
			logger.error("Error occured in saveOrUpdateAwardScopus : {}", e.getMessage());
		}
		return awardScopus;
	}

	@Override
	public AwardScopus getAwardScopusBasedOnId(Integer awardScopusId) {
		return hibernateTemplate.load(AwardScopus.class, awardScopusId);
	}

	@Override
	public void deleteAwardScopus(AwardScopus awardScopus) {
		try {
			hibernateTemplate.delete(awardScopus);
		} catch (Exception e) {
			logger.error("Error occured in delete AwardScopus : {}", e.getMessage());
		}
	}

	@Override
	public List<AwardScopus> fetchAllAwardScopus(Integer awardId) {
		List<AwardScopus> awardScopuses = new ArrayList<>();
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardScopus> awardScopus = builder.createQuery(AwardScopus.class);
		Root<AwardScopus> rootAwardPublications = awardScopus.from(AwardScopus.class);
		Predicate awardIdPredicate = builder.equal(rootAwardPublications.get(AWARD_ID), awardId);
		awardScopus.where(builder.and(awardIdPredicate));
		return session.createQuery(awardScopus).getResultList();
		} catch (Exception e) {
			logger.error("Error occured in fetch AllAwardScopus : {}", e.getMessage());
		}
		return awardScopuses;
	}

	@Override
	public String getConfigurationValue(String configurationKey) {	
		ScopusConfigurationData scopusConfigurationData;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ScopusConfigurationData> query = builder.createQuery(ScopusConfigurationData.class);
		Root<ScopusConfigurationData> parameterBo = query.from(ScopusConfigurationData.class);
		Predicate predicateParameterName = builder.equal(parameterBo.get("configurationKey"), configurationKey);
		query.where(builder.and(predicateParameterName));
		if (session.createQuery(query).uniqueResult() != null) {
		 scopusConfigurationData = session.createQuery(query).uniqueResult();
		 return scopusConfigurationData.getConfigurationValue();
		}
		return null;
	}
}
