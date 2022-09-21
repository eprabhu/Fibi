package com.polus.fibicomp.awardbudgetapiintegration.dao;

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.awardbudgetapiintegration.pojo.BudgetAPIConfigurationData;
import com.polus.fibicomp.awardbudgetapiintegration.pojo.BudgetAPIResponse;

@Transactional
@Service(value = "budgetAPIDao")
public class BudgetAPIDaoImpl implements BudgetAPIDao {

	protected static Logger logger = LogManager.getLogger(BudgetAPIDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public BudgetAPIResponse saveBudgetAPIResponse(BudgetAPIResponse budgetAPIResponse) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Transaction transaction = session.getTransaction();
			if (transaction.isActive() == false) {
				transaction.begin();
			}
			hibernateTemplate.saveOrUpdate(budgetAPIResponse);
		} catch (Exception e) {
			e.printStackTrace();
			logger.info("Exception in saving budgetAPIResponse");
			return budgetAPIResponse;
		}
		return budgetAPIResponse;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<BudgetAPIResponse> fetchBudgetAPIResponse(String projectNumber, Integer year) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM BudgetAPIResponse WHERE projectNumber=:projectNumber AND year=:year AND (actual > 0 OR commitment > 0 OR fundAvailable > 0 OR budget > 0)";
		Query<BudgetAPIResponse> query = session.createQuery(hqlQuery);
		query.setParameter("projectNumber", projectNumber);
		query.setParameter("year", year);
		return query.getResultList();
	}

	@Override
	public String getBudgetIntegrationConfigurationValue(String configurationKey) {
		BudgetAPIConfigurationData budgetAPIConfigurationData;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetAPIConfigurationData> query = builder.createQuery(BudgetAPIConfigurationData.class);
		Root<BudgetAPIConfigurationData> rootConfiguration = query.from(BudgetAPIConfigurationData.class);
		Predicate predicateConfigurationKey = builder.equal(rootConfiguration.get("configurationKey"), configurationKey);
		query.where(builder.and(predicateConfigurationKey));
		budgetAPIConfigurationData = session.createQuery(query).uniqueResult();
		if (budgetAPIConfigurationData != null) {
			return budgetAPIConfigurationData.getConfigurationValue();
		}
		return null;
	}
}
