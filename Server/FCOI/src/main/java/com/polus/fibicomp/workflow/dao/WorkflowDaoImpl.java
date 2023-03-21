package com.polus.fibicomp.workflow.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.LockModeType;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.pojo.PersonRoleType;
import com.polus.fibicomp.pojo.UnitAdministrator;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowAttachment;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowDetailExt;
import com.polus.fibicomp.workflow.pojo.WorkflowFeedbackType;
import com.polus.fibicomp.workflow.pojo.WorkflowMapDetail;
import com.polus.fibicomp.workflow.pojo.WorkflowMapType;
import com.polus.fibicomp.workflow.pojo.WorkflowStatus;

@Transactional
@Service(value = "workflowDao")
public class WorkflowDaoImpl implements WorkflowDao {

	protected static Logger logger = LogManager.getLogger(WorkflowDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private DBEngine dbEngine;

	@Lock(LockModeType.PESSIMISTIC_WRITE)
	@Override
	public Workflow saveWorkflow(Workflow workflow) {
		try {
			hibernateTemplate.saveOrUpdate(workflow);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return workflow;
	}

	@Override
	public List<WorkflowMapDetail> fetchWorkflowMapDetail() {
		List<WorkflowMapDetail> workflowMapDetails = hibernateTemplate.loadAll(WorkflowMapDetail.class);
		return workflowMapDetails;
	}

	@Override
	public WorkflowStatus fetchWorkflowStatusByStatusCode(String approveStatusCode) {
		return hibernateTemplate.get(WorkflowStatus.class, approveStatusCode);
	}

	@Override
	public Workflow fetchActiveWorkflowByParams(String moduleItemId, Integer moduleCode, String subModuleItemId, Integer subModuleCode) {					
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Workflow> query = builder.createQuery(Workflow.class);
		Root<Workflow> root = query.from(Workflow.class);
		Predicate predicateModuleItemId = builder.equal(root.get("moduleItemId"), moduleItemId);
		Predicate predicateIsWorkflowActive = builder.equal(root.get("isWorkflowActive"), true);
		Predicate predicateModuleCode = builder.equal(root.get("moduleCode"), moduleCode);
		Predicate predicateSubModuleItemId = builder.equal(root.get("subModuleItemId"), subModuleItemId);
		Predicate predicateSubModuleCode = builder.equal(root.get("subModuleCode"), subModuleCode);
		query.where(builder.and(predicateModuleItemId, predicateIsWorkflowActive, predicateModuleCode, predicateSubModuleItemId, predicateSubModuleCode));
		List<Workflow> workFlows = session.createQuery(query).list();
		if(workFlows == null || workFlows.isEmpty() ) {
			return new Workflow();
		}
		return workFlows.get(0);
	}

	@Override
	public WorkflowDetail findUniqueWorkflowDetailByCriteria(Integer workflowId, String personId, boolean isSuperUser, Integer approverStopNumber) {
		WorkflowDetail workflowDetail = null;
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		criteria.add(Restrictions.eq("approverPersonId", personId));
		if (approverStopNumber != null) {
			criteria.add(Restrictions.eq("approvalStopNumber", approverStopNumber));
		}
		if (isSuperUser) {
			@SuppressWarnings("unchecked")
			List<WorkflowDetail> workflowDetails = criteria.list();
			for (WorkflowDetail wfwDetail : workflowDetails) {
				if (wfwDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
					workflowDetail = wfwDetail;
				}
			}
		} else {
			workflowDetail = (WorkflowDetail) criteria.uniqueResult();
		}
		return workflowDetail;
	}

	@Lock(LockModeType.PESSIMISTIC_WRITE)
	@Override
	public WorkflowDetail saveWorkflowDetail(WorkflowDetail workflowDetail) {
		try {
			hibernateTemplate.saveOrUpdate(workflowDetail);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return workflowDetail;
	}

	@Override
	public WorkflowAttachment fetchWorkflowAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(WorkflowAttachment.class, attachmentId);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<WorkflowDetail> fetchWorkflowDetailListByApprovalStopNumber(Integer workflowId, Integer approvalStopNumber, String approvalStatusCode) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		if (approvalStopNumber != null) {
			criteria.add(Restrictions.eq("approvalStopNumber", approvalStopNumber));
		}
		criteria.add(Restrictions.eq("approvalStatusCode", approvalStatusCode));
		List<WorkflowDetail> workflowDetailList = criteria.list();
		return workflowDetailList;
	}

	@Override
	public Integer getMaxStopNumber(Integer workflowId) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.setProjection(Projections.max("approvalStopNumber"));
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		Integer maxApprovalStopNumber = (Integer)criteria.uniqueResult();
		return maxApprovalStopNumber;
	}

	@Override
	public WorkflowDetail fetchWorkflowByParams(Integer workflowId, String personId, Integer stopNumber) {
		List<String> approvalStatusCodes = new ArrayList<String>();
		approvalStatusCodes.add("A");
		approvalStatusCodes.add("R");
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		criteria.add(Restrictions.eq("approvalStopNumber", stopNumber));
		//criteria.add(Restrictions.eq("approverPersonId", personId));
		criteria.add(Restrictions.in("approvalStatusCode", approvalStatusCodes));
		WorkflowDetail workflowDetail = null;
		@SuppressWarnings("unchecked")
		List<WorkflowDetail> workflowDetails = criteria.list();
		if (workflowDetails != null && !workflowDetails.isEmpty()) {
			workflowDetail = workflowDetails.get(0);
		}
		return workflowDetail;
	}

	@Override
	public WorkflowDetail fetchWorkflowDetailById(Integer workflowId) {
		return hibernateTemplate.get(WorkflowDetail.class, workflowId);
	}

	@Override
	public Long activeWorkflowCountByModuleItemId(String moduleItemId) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(Workflow.class);
		criteria.add(Restrictions.eq("moduleItemId", moduleItemId));
		Long workflowCount = (Long) criteria.setProjection(Projections.rowCount()).uniqueResult();
		return workflowCount;
	}

	@Override
	public WorkflowDetail getCurrentWorkflowDetail(Integer workflowId, String personId, Integer roleCode) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		criteria.add(Restrictions.eq("approverPersonId", personId));
		criteria.add(Restrictions.eq("roleTypeCode", roleCode));
		WorkflowDetail workflowDetail = (WorkflowDetail) criteria.list().get(0);
		return workflowDetail;
	}

	@Override
	public List<WorkflowDetail> fetchWorkflowDetailByWorkflowId(Integer workflowId) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		@SuppressWarnings("unchecked")
		List<WorkflowDetail> workflowDetails = criteria.list();
		return workflowDetails;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Set<String> fetchEmailAdressByUserType(String roleTypeCode) {
		Set<String> mailAdressList = new HashSet<String>();
		Set<String> personIdList = new HashSet<String>();
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession()
				.createCriteria(UnitAdministrator.class);
		Criteria personCriteria = hibernateTemplate.getSessionFactory().getCurrentSession()
				.createCriteria(Person.class);
		criteria.add(Restrictions.eq("unitAdministratorTypeCode", roleTypeCode));
		List<UnitAdministrator> grantPersons = criteria.list();
		if (grantPersons != null && !grantPersons.isEmpty()) {
			for (UnitAdministrator grantPerson : grantPersons) {
				personIdList.add(grantPerson.getPersonId());
			}
		}
		personCriteria.add(Restrictions.in("personId", personIdList));
		List<Person> personDetailsViews = personCriteria.list();
		if (personDetailsViews != null && !personDetailsViews.isEmpty()) {
			for (Person personDetailsView : personDetailsViews) {
				mailAdressList.add(personDetailsView.getEmailAddress());
			}
		}
		return mailAdressList;
	}

	@Override
	public List<WorkflowMapDetail> fetchWorkflowMapDetailByRoleType(Integer roleTypeCode) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowMapDetail.class);
		criteria.add(Restrictions.eq("roleTypeCode", roleTypeCode));
		@SuppressWarnings("unchecked")
		List<WorkflowMapDetail> workflowMapDetails = criteria.list();
		return workflowMapDetails;
	}

	@Override
	public List<WorkflowDetail> fetchFinalApprover(Integer workflowId, Integer approvalStopNumber) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		criteria.add(Restrictions.eq("approvalStatusCode", "W"));
		criteria.add(Restrictions.eq("approvalStopNumber", approvalStopNumber));
		@SuppressWarnings("unchecked")
		List<WorkflowDetail> workflowDetails = criteria.list();
		return workflowDetails;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Workflow> fetchWorkflowsByParams(String moduleItemId, Integer moduleCode, String subModuleItemId, Integer subModuleCode) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(Workflow.class);
		criteria.add(Restrictions.eq("moduleItemId", moduleItemId));
		criteria.add(Restrictions.eq("moduleCode", moduleCode));
		criteria.add(Restrictions.eq("subModuleItemId", subModuleItemId));
		criteria.add(Restrictions.eq("subModuleCode", subModuleCode));
		return criteria.list();
	}

	@Override
	public Integer getWaitingForApprovalStopNumber(String statusCode, Integer workflowId) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(WorkflowDetail.class);
		criteria.add(Restrictions.eq("workflow.workflowId", workflowId));
		criteria.add(Restrictions.eq("approvalStatusCode", statusCode));
		WorkflowDetail workflowDetail = (WorkflowDetail) criteria.list().get(0);
		return workflowDetail.getApprovalStopNumber();
	}

	@Override
	public Person getPersonDetail(String personId) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(Person.class);
		criteria.add(Restrictions.eq("personId", personId));
		Person personDetail = (Person) criteria.list().get(0);
		return personDetail;
	}

	@Override
	public PersonRoleType getWorkflowRoleTypeById(Integer roleTypeId) {
		return hibernateTemplate.get(PersonRoleType.class, roleTypeId);
	}

	@Override
	public Integer getMaxApproverNumber(Integer workFlowId, Integer mapId, Integer mapNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<WorkflowDetail> rootWorkflowDetail = query.from(WorkflowDetail.class);
		Predicate predicateOne = builder.equal(rootWorkflowDetail.get("workflow").get("workflowId"),workFlowId);
		Predicate predicateTwo = builder.equal(rootWorkflowDetail.get("mapId"),mapId);
		Predicate predicateThree = builder.equal(rootWorkflowDetail.get("mapNumber"),mapNumber);
		query.select(builder.max(rootWorkflowDetail.get("approverNumber")));
		query.where(builder.and(predicateOne, predicateTwo,predicateThree));
		if(session.createQuery(query).getSingleResult() != null) {
			return  session.createQuery(query).getSingleResult()+1;
		} else {
			return 1;
		}
	}
    
	public List<WorkflowMapType> fetchAllWorkflowMapTypes() {
		return hibernateTemplate.loadAll(WorkflowMapType.class);
     }

	@Override
	public List<WorkflowDetail> getWorkflowDetailByUserRole(Integer workflowId, Integer roleTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkflowDetail> query = builder.createQuery(WorkflowDetail.class);
		Root<WorkflowDetail> rootWorkflowDetail = query.from(WorkflowDetail.class);
		Predicate predicateOne = builder.equal(rootWorkflowDetail.get("workflow").get("workflowId"), workflowId);
		Predicate predicateTwo = builder.equal(rootWorkflowDetail.get("personRoleType").get("roleTypeCode"), roleTypeCode);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String fetchStopNameBasedMapIdAndStop(Integer mapId, Integer approvalStopNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT stopName FROM WorkflowMapDetail WHERE approvalStopNumber=:approvalStopNumber AND mapId=: mapId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("approvalStopNumber", approvalStopNumber);
		query.setParameter("mapId", mapId);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			return query.getResultList().get(0);
		}
		else {
			return "";
		}
	}

	@Override
	public WorkflowDetail getWorkflowDetailBasedOnParams(Integer workflowId, String workFlowStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkflowDetail> query = builder.createQuery(WorkflowDetail.class);
		Root<WorkflowDetail> rootWorkflowDetail = query.from(WorkflowDetail.class);
		Predicate predicateOne = builder.equal(rootWorkflowDetail.get("workflow").get("workflowId"), workflowId);
		Predicate predicateTwo = builder.equal(rootWorkflowDetail.get("approvalStatusCode"), workFlowStatusCode);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public Integer fetchWorkflowDetailId(Integer proposalId, String personId) {
		Integer workflowDetailId = null;
		ArrayList<HashMap<String, Object>> output = new ArrayList<HashMap<String, Object>>();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<PROPOSAL_ID>>", DBEngineConstants.TYPE_INTEGER, proposalId));
			inParam.add(new Parameter("<<PROPOSAL_ID>>", DBEngineConstants.TYPE_INTEGER, proposalId));
			inParam.add(new Parameter("<<APPROVER_PERSON_ID>>", DBEngineConstants.TYPE_STRING, personId));
			output = dbEngine.executeQuery(inParam, "GET_WORKFLOW_DETAIL_ID");
			if (output != null && !output.isEmpty()) {
				for (HashMap<String, Object> hmRules : output) {
					if (hmRules.get("WORKFLOW_DETAIL_ID") != null) {
						return Integer.parseInt(hmRules.get("WORKFLOW_DETAIL_ID").toString());
					}
				}
			}	
		} catch (Exception e) {
			logger.error("Exception in getWorkflowdetailId : {}" ,e.getMessage());
		}
		return workflowDetailId;
	}

	@Override
	public Integer getNextStopNumberBasedOnMap(Integer mapId, Integer workflowId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT MAX(W.approvalStopNumber) FROM WorkflowDetail W WHERE W.workflowId = :workflowId and W.mapId = :mapId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("workflowId", workflowId);
		query.setParameter("mapId", mapId);
		return query.getSingleResult() == null ? null : Integer.parseInt(query.getSingleResult().toString()) +1;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getLatestWorkflowPersonIdsByParams(String moduleItemId, Integer moduleCode) {
		List<String> personIds = new ArrayList<String>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
 		String hqlQuery = "select distinct T1.approverPersonId from WorkflowDetail T1 inner join Workflow T2 ON T1.workflowId = T2.workflowId where T2.moduleItemId = :moduleItemId AND T2.moduleCode = :moduleCode AND T2.isWorkflowActive = :isWorkflowActive";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("moduleItemId", moduleItemId);
		query.setParameter("moduleCode", moduleCode);
		query.setParameter("isWorkflowActive", true);
		if (query.getResultList() != null) {
			personIds = query.getResultList();
		}
        return personIds;
	}

	@Override
	public Workflow getActiveWorkFlow(String moduleItemId, Integer moduleCode, String subModuleItemId, Integer subModuleCode) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(Workflow.class);
		criteria.add(Restrictions.eq("moduleItemId", moduleItemId));
		criteria.add(Restrictions.eq("moduleCode", moduleCode));
		criteria.add(Restrictions.eq("subModuleItemId", subModuleItemId));
		criteria.add(Restrictions.eq("subModuleCode", subModuleCode));
		criteria.add(Restrictions.eq("isWorkflowActive", true));
		return (Workflow) criteria.uniqueResult();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<WorkflowDetail> getScoringWorkflowDetails(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = new StringBuilder("SELECT new WorkflowDetail(t2.workflowDetailId,t2.approverPersonId,t2.approverPersonName, t3.canScore, t2.workflowMap) FROM Workflow t1 left join ")
				.append("WorkflowDetail t2 on t1.workflowId = t2.workflowId inner join ")
				.append("ProposalEvaluationPanel t3 on t3.mapId = t2.mapId and t3.canScore is not null and t3.proposalId=:proposalId")
				.append(" WHERE t1.moduleItemId=:proposalId AND t1.isWorkflowActive=:isActive and t1.moduleCode = 3 and t1.subModuleCode = 0").toString();
		Query query = session.createQuery(hqlQuery);
		query.setParameter("proposalId", proposalId);
		query.setParameter("isActive", true);
		return query.getResultList();
	}

	@Override
	public List<WorkflowFeedbackType> fetchWorkflowFeedbackTypeBasedOnParam(Integer moduleCode, Integer subModuleCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<WorkflowFeedbackType> query = builder.createQuery(WorkflowFeedbackType.class);
		Root<WorkflowFeedbackType> rootWorkflowFeedbackType = query.from(WorkflowFeedbackType.class);
		Predicate predicateOne = builder.equal(rootWorkflowFeedbackType.get("moduleCode"), moduleCode);
		Predicate predicateTwo = builder.equal(rootWorkflowFeedbackType.get("isActive"), true);
		Predicate predicateThree = builder.equal(rootWorkflowFeedbackType.get("subModuleCode"), subModuleCode);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		return session.createQuery(query).getResultList();
	}

	@Override
	public WorkflowFeedbackType fetchWorkflowFeedbackTypeBasedOnCode(String feedbackTypeCode) {
		return hibernateTemplate.get(WorkflowFeedbackType.class, feedbackTypeCode);
	}

	@Override
	public WorkflowDetailExt fetchWorkflowExtBasedOnWorkflowDetailId(Integer workflowDetailId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<WorkflowDetailExt> query = builder.createQuery(WorkflowDetailExt.class);
			Root<WorkflowDetailExt> rootWorkflowDetailExt = query.from(WorkflowDetailExt.class);
			query.where(builder.equal(rootWorkflowDetailExt.get("workflowDetailId"), workflowDetailId));
			return session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public void saveOrUpdateWorkflowDetailExt(WorkflowDetailExt workflowDetailExt) {
		hibernateTemplate.saveOrUpdate(workflowDetailExt);
	}

	@Override
	public WorkflowDetail getWorkFlowDetails(Integer workflowdetailId) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<WorkflowDetail> query = builder.createQuery(WorkflowDetail.class);
			Root<WorkflowDetail> rootWorkflowDetail = query.from(WorkflowDetail.class);
			query.where(builder.equal(rootWorkflowDetail.get("workflowDetailId"), workflowdetailId));
			return session.createQuery(query).uniqueResult();
	}

}
