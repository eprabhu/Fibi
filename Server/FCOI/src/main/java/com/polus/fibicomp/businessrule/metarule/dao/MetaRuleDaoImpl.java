package com.polus.fibicomp.businessrule.metarule.dao;

import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.businessrule.metarule.pojo.MetaRule;
import com.polus.fibicomp.businessrule.metarule.pojo.MetaRuleDetail;
import com.polus.fibicomp.constants.Constants;

@Transactional
@Service(value = "metaRuleDao")
public class MetaRuleDaoImpl implements MetaRuleDao {


	protected static Logger logger = LogManager.getLogger(MetaRuleDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	private static final String META_RULE_ID = "metaRuleId";
	private static final String META_RULE = "metaRule";
	private static final String NODE_NUMBER = "nodeNumber";

	@Override
	public MetaRule saveOrUpdateMetaRule(MetaRule metaRule) {
		hibernateTemplate.saveOrUpdate(metaRule);
		return metaRule;
	}

	@Override
	public MetaRuleDetail saveOrUpdateMetaRuleDetail(MetaRuleDetail metaRuleDetail) {
		hibernateTemplate.saveOrUpdate(metaRuleDetail);
		return metaRuleDetail;
	}

	@Override
	public Integer generateNextNodeNumber(Integer metaRuleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<MetaRuleDetail> rootAward = query.from(MetaRuleDetail.class);
		Predicate predicateOne = builder.equal(rootAward.get(META_RULE).get(META_RULE_ID), metaRuleId);
		query.select(builder.max(rootAward.get(NODE_NUMBER)));
		query.where(builder.and(predicateOne));
		Integer nodeNumber = session.createQuery(query).getSingleResult();
		return nodeNumber != null ? nodeNumber + 1 : 1;
	}

	@Override
	public MetaRule fetchMetaRulesByParams(String unitNumber, Integer moduleCode, Integer subModuleCode,
			String metaRuleType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<MetaRule> query = builder.createQuery(MetaRule.class);
		Root<MetaRule> rootMetaRule = query.from(MetaRule.class);
		Predicate predicateUnitNumber = builder.equal(rootMetaRule.get("unitNumber"), unitNumber);
		Predicate predicateModuleCode = builder.equal(rootMetaRule.get("moduleCode"), moduleCode);
		Predicate predicateSubModuleCode = builder.equal(rootMetaRule.get("subModuleCode"), subModuleCode);
		Predicate predicateMetaRuleType = builder.equal(rootMetaRule.get("metaRuleType"), metaRuleType);
		query.where(builder.and(predicateUnitNumber, predicateModuleCode, predicateSubModuleCode, predicateMetaRuleType));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<MetaRuleDetail> getMetaRuleDetailByMetaRuleId(Integer metaRuleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<MetaRuleDetail> query = builder.createQuery(MetaRuleDetail.class);
		Root<MetaRuleDetail> rootMetaRuleDetail = query.from(MetaRuleDetail.class);
		Predicate predicateMetaRuleId = builder.equal(rootMetaRuleDetail.get(META_RULE).get(META_RULE_ID), metaRuleId);
		query.where(builder.and(predicateMetaRuleId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String getRuleNameByRuleId(Integer ruleId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Query query = session.createSQLQuery("SELECT DESCRIPTION FROM BUSINESS_RULES where RULE_ID = :ruleId");
			query.setParameter("ruleId", ruleId);
			return query.getSingleResult().toString();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public List<MetaRuleDetail> getMetaRuleDetailsByParams(Integer nodeNumber, Integer metaRuleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<MetaRuleDetail> query = builder.createQuery(MetaRuleDetail.class);
		Root<MetaRuleDetail> rootMetaRule = query.from(MetaRuleDetail.class);
		Predicate predicateParentNodeNumber = builder.equal(rootMetaRule.get("parentNode"), nodeNumber);
		Predicate predicateMetaRuleId = builder.equal(rootMetaRule.get(META_RULE).get(META_RULE_ID), metaRuleId);
		query.where(builder.and(predicateParentNodeNumber, predicateMetaRuleId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteMetaRuleNode(Integer metaRuleDetailId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaDelete<MetaRuleDetail> criteria = builder.createCriteriaDelete(MetaRuleDetail.class);
			Root<MetaRuleDetail> root = criteria.from(MetaRuleDetail.class);				
			criteria.where(builder.equal(root.get("metaRuleDetailId"), metaRuleDetailId));
			session.createQuery(criteria).executeUpdate();
		} catch (Exception e) {
			logger.error("Error in deleteMetaRuleNode method", e);
		}
	}

	@Override
	public void deleteMetaRule(Integer metaRuleId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaDelete<MetaRule> criteria = builder.createCriteriaDelete(MetaRule.class);
			Root<MetaRule> root = criteria.from(MetaRule.class);				
			criteria.where(builder.equal(root.get(META_RULE_ID), metaRuleId));
			session.createQuery(criteria).executeUpdate();
		} catch (Exception e) {
			logger.error("Error in deleteMetaRule method", e);
		}
	}

	@Override
	public void updateParentNodeDetails(Integer nodeNumber, String parentNodeNumber, Integer metaRuleId, String nodeCondition) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaUpdate<MetaRuleDetail> criteriaUpdate = cb.createCriteriaUpdate(MetaRuleDetail.class);
			Root<MetaRuleDetail> root = criteriaUpdate.from(MetaRuleDetail.class);
			Predicate predicateMetaRuleId = cb.equal(root.get(META_RULE).get(META_RULE_ID), metaRuleId);
			Predicate parentNode = cb.equal(root.get(NODE_NUMBER), parentNodeNumber);
			if (Constants.NEXT_NODE.equals(nodeCondition)) {
				criteriaUpdate.set("nextNode", nodeNumber);
			} else if (Constants.NODE_IF_TRUE.equals(nodeCondition)) {
				criteriaUpdate.set("nodeIfTrue", nodeNumber);
			} else if (Constants.NODE_IF_FALSE.equals(nodeCondition)) {
				criteriaUpdate.set("nodeIfFalse", nodeNumber);
			}
			criteriaUpdate.where(cb.and(predicateMetaRuleId, parentNode));
			session.createQuery(criteriaUpdate).executeUpdate();
		} catch (Exception e) {
			logger.error("Error in updateParentNodeDetails method", e);
		}
	}

	@Override
	public void updateNodeDetailInParent(String parentNodeNumber, Integer metaRuleId, String nodeCondition) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaUpdate<MetaRuleDetail> criteriaUpdate = cb.createCriteriaUpdate(MetaRuleDetail.class);
			Root<MetaRuleDetail> root = criteriaUpdate.from(MetaRuleDetail.class);
			Predicate predicateMetaRuleId = cb.equal(root.get(META_RULE).get(META_RULE_ID), metaRuleId);
			Predicate parentNode = cb.equal(root.get(NODE_NUMBER), parentNodeNumber);
			if (Constants.NEXT_NODE.equals(nodeCondition)) {
				criteriaUpdate.set("nextNode", null);
			} else if (Constants.NODE_IF_TRUE.equals(nodeCondition)) {
				criteriaUpdate.set("nodeIfTrue", null);
			} else if (Constants.NODE_IF_FALSE.equals(nodeCondition)) {
				criteriaUpdate.set("nodeIfFalse", null);
			}
			criteriaUpdate.where(cb.and(predicateMetaRuleId, parentNode));
			session.createQuery(criteriaUpdate).executeUpdate();
		} catch (Exception e) {
			logger.error("Error in updateParentNodeDetails method", e);
		}
	}
	

}
