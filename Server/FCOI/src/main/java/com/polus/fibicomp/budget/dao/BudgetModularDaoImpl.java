package com.polus.fibicomp.budget.dao;

import java.io.Serializable;
import java.util.List;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
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
import com.polus.fibicomp.budget.pojo.BudgetModular;
import com.polus.fibicomp.budget.pojo.BudgetModularIDC;

@Transactional
@Service(value = "budgetModularDao")
public class BudgetModularDaoImpl implements BudgetModularDao {

	protected static Logger logger = LogManager.getLogger(BudgetModularDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<BudgetModular> fetchBudgetModular(Integer budgetId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetModular> query = builder.createQuery(BudgetModular.class);
		Root<BudgetModular> root = query.from(BudgetModular.class);
		Predicate predicate1 = builder.equal(root.get("budgetId"), budgetId);
		query.where(builder.and(predicate1));
		List<BudgetModular> budgetModular = session.createQuery(query).list();
		return budgetModular;
	}

	@Override
	public BudgetModular insertBudgetModular(BudgetModular budgetModular) {
		try {
			hibernateTemplate.save(budgetModular);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return budgetModular;
	}

	@Override
	public BudgetModularIDC insertBudgetModularIDC(BudgetModularIDC budgetModularIDC) {
		try {
			hibernateTemplate.save(budgetModularIDC);
			Serializable id = hibernateTemplate.save(budgetModularIDC);
			budgetModularIDC.setBudgetModularIDCId(Integer.parseInt(id.toString()));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return budgetModularIDC;
	}

	@Override
	public BudgetModular saveBudgetModular(BudgetModular budgetModular) {
		hibernateTemplate.saveOrUpdate(budgetModular);
		return budgetModular;
	}

	@Override
	public void deleteBudgetModularIDCLine(Integer budgetModularIDCId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<BudgetModularIDC> delete = builder.createCriteriaDelete(BudgetModularIDC.class);
		Root<BudgetModularIDC> root = delete.from(BudgetModularIDC.class);
		delete.where(builder.equal(root.get("budgetModularIDCId"), budgetModularIDCId));
		session.createQuery(delete).executeUpdate();

	}

}
