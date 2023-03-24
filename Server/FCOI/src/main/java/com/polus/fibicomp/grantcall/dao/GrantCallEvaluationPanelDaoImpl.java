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

import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.workflow.pojo.WorkflowMap;

@Transactional
@Service(value = "grantCallEvaluationPanelDao")
public class GrantCallEvaluationPanelDaoImpl implements GrantCallEvaluationPanelDao {

	protected static Logger logger = LogManager.getLogger(GrantCallEvaluationPanelDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<WorkflowMap> fetchAllEvaluationPanels(Integer mapId, String mapType) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkflowMap> query = builder.createQuery(WorkflowMap.class);
		Root<WorkflowMap> rootWorkflowMap = query.from(WorkflowMap.class);
		Predicate predicate1 = builder.equal(rootWorkflowMap.get("mapType"), mapType);
		Predicate predicate2 = builder.notEqual(rootWorkflowMap.get("mapId"), mapId);
		if (mapId != null) {
			query.where(builder.and(predicate1, predicate2));
		} else {
			query.where(builder.and(predicate1));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<GrantCallEvaluationPanel> fetchEvaluationPanelByGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallEvaluationPanel> query = builder.createQuery(GrantCallEvaluationPanel.class);
		Root<GrantCallEvaluationPanel> rootGrantCallEvaluationPanel = query.from(GrantCallEvaluationPanel.class);
		query.where(builder.equal(rootGrantCallEvaluationPanel.get("grantCallId"), grantCallId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallEvaluationPanel saveOrUpdateGrantCallEvaluationPanel(GrantCallEvaluationPanel grantCallEvaluationPanel) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallEvaluationPanel);
		} catch (Exception e) {
			logger.error("Exception while saving GrantCallEvaluationPanel {}" , e.getMessage());
		}
		return grantCallEvaluationPanel;
	}

	@Override
	public String deleteGrantCallEvaluationPanel(Integer grantCallEvaluationPanelId) {
		hibernateTemplate.delete(hibernateTemplate.get(GrantCallEvaluationPanel.class, grantCallEvaluationPanelId));
		return "Grant Call Evaluation Panel deleted successfully";
	}

}
