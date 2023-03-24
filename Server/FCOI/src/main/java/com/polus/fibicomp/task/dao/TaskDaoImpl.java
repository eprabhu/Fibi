package com.polus.fibicomp.task.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.NoResultException;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Conjunction;
import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.pojo.TaskActionLog;
import com.polus.fibicomp.task.pojo.TaskAssigneeHistory;
import com.polus.fibicomp.task.pojo.TaskAttachment;
import com.polus.fibicomp.task.pojo.TaskComment;
import com.polus.fibicomp.task.pojo.TaskCommentAttachment;
import com.polus.fibicomp.task.pojo.TaskFileData;
import com.polus.fibicomp.task.pojo.TaskStatus;
import com.polus.fibicomp.task.pojo.TaskStatusHistory;
import com.polus.fibicomp.task.pojo.TaskType;
import com.polus.fibicomp.task.vo.TaskVO;

@Transactional
@Service(value = "taskDao")
public class TaskDaoImpl implements TaskDao {

	protected static Logger logger = LogManager.getLogger(TaskDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	private static final String TASKID = "taskId";
	private static final String UPDATETIMESTAMP = "updateTimestamp";
	private static final String CREATETIMESTAMP = "createTimestamp";
	private static final String MODULEITEMKEY = "moduleItemKey";
	private static final String TASKSTATUSCODE = "taskStatusCode";
	private static final String  MODULECODE = "moduleCode";
	private static final String ASSIGNEEPERSONID = "assigneePersonId";
	private static final String MODULEITEMID = "moduleItemId";

	@Override
	public List<TaskType> fetchAllTaskType() {
		return hibernateTemplate.loadAll(TaskType.class);
	}

	@Override
	public List<TaskStatus> fetchAllTaskStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TaskStatus> query = builder.createQuery(TaskStatus.class);
		Root<TaskStatus> rootAgreementSponsorType = query.from(TaskStatus.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Task saveOrUpdateTask(Task task) {
		try {
			hibernateTemplate.saveOrUpdate(task);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateTask :{}", e.getMessage());
		}
		return task;
	}

	@Override
	public TaskFileData getFileDataById(String fileDataId) {
		return hibernateTemplate.get(TaskFileData.class, fileDataId);
	}

	@Override
	public TaskFileData saveFileData(TaskFileData fileData) {
		hibernateTemplate.save(fileData);
		return fileData;
	}

	@Override
	public TaskActionLog saveOrUpdateTaskActionLog(TaskActionLog taskActionLog) {
		try {
			hibernateTemplate.saveOrUpdate(taskActionLog);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateTaskActionLog :{}", e.getMessage());
		}
		return taskActionLog;
	}

	@Override
	public TaskStatusHistory saveOrUpdateTaskStatusHistory(TaskStatusHistory taskStatusHistory) {
		try {
			hibernateTemplate.saveOrUpdate(taskStatusHistory);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateTaskStatusHistory :{}", e.getMessage());
		}
		return taskStatusHistory;
	}

	@Override
	public Task fetchTaskByTaskId(Integer taskId) {
		return hibernateTemplate.get(Task.class, taskId);
	}
	
	@Override
	public Task fetchTaskByTaskIdAndModuleItemId(String moduleItemKey, Integer taskId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Task> query = builder.createQuery(Task.class);
			Root<Task> rootTask = query.from(Task.class);
			Predicate predicateTaskId = builder.equal(rootTask.get("taskId"), taskId);
			Predicate predicatemoduleItemKey = builder.equal(rootTask.get("moduleItemKey"), moduleItemKey);
			query.where(builder.and(predicateTaskId, predicatemoduleItemKey));
			return session.createQuery(query).getSingleResult();
		} catch (NoResultException e) {
			return null;
		}
	}

	@Override
	public TaskType fetchTaskTypeByTaskTypeCode(String taskTypeCode) {
		return hibernateTemplate.get(TaskType.class, taskTypeCode);
	}

	@Override
	public TaskAttachment fetchTaskAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(TaskAttachment.class, attachmentId);
	}

	@Override
	public void deleteTaskFileData(TaskFileData fileData) {
		hibernateTemplate.delete(fileData);
	}

	@Override
	public List<TaskComment> fetchTaskCommentBasedOnTaskId(Integer taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TaskComment> query = builder.createQuery(TaskComment.class);
		Root<TaskComment> rootTaskAttachment = query.from(TaskComment.class);
		query.where(builder.equal(rootTaskAttachment.get("task").get(TASKID), taskId));
		query.orderBy(builder.desc(rootTaskAttachment.get(UPDATETIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<TaskAttachment> fetchTaskAttachmentBasedOnTaskId(Integer taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TaskAttachment> query = builder.createQuery(TaskAttachment.class);
		Root<TaskAttachment> rootTaskAttachment = query.from(TaskAttachment.class);
		query.where(builder.equal(rootTaskAttachment.get("task").get(TASKID), taskId));
		query.orderBy(builder.desc(rootTaskAttachment.get(UPDATETIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public TaskComment saveOrUpdateTaskComment(TaskComment taskComment) {
		try {
			hibernateTemplate.saveOrUpdate(taskComment);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateTaskComment :{}", e.getMessage());
		}
		return taskComment;
	}

	@Override
	public void deleteTaskAttachment(Integer taskAttachmentId) {
		hibernateTemplate.delete(hibernateTemplate.get(TaskAttachment.class, taskAttachmentId));
	}

	@Override
	public void deleteTaskComment(Integer taskCommentId) {
		hibernateTemplate.delete(hibernateTemplate.get(TaskComment.class, taskCommentId));
	}

	@Override
	public TaskCommentAttachment fetchTaskCommentAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(TaskCommentAttachment.class, attachmentId);
	}

	@Override
	public void deleteTaskCommentAttachment(Integer taskCommentAttachmentId) {
		hibernateTemplate.delete(hibernateTemplate.get(TaskCommentAttachment.class, taskCommentAttachmentId));
	}

	@Override
	public TaskStatus fetchTaskStatusByTaskStatusCode(String taskStatusCode) {
		return hibernateTemplate.get(TaskStatus.class, taskStatusCode);
	}

	@Override
	public TaskActionLog fetchTaskActionLogBasedOnTaskId(Integer taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TaskActionLog> query = builder.createQuery(TaskActionLog.class);
		Root<TaskActionLog> rootTaskActionLog = query.from(TaskActionLog.class);
		query.where(builder.equal(rootTaskActionLog.get(TASKID), taskId));
		query.orderBy(builder.desc(rootTaskActionLog.get(UPDATETIMESTAMP)));
		return session.createQuery(query).getResultList().get(0);
	}

	@Override
	public Integer fetchTaskCountBasedOnModuleItemKeyAndTaskStatus(String moduleItemKey, List<String> statusCodes) {
		Integer count = 0;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT count(*) FROM Task WHERE moduleItemKey=:moduleItemKey and taskStatusCode IN (:taskStatusCode)";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Long> query = session.createQuery(hqlQuery);
		query.setParameter(MODULEITEMKEY, moduleItemKey);
		query.setParameter(TASKSTATUSCODE, statusCodes);
		count = query.uniqueResult().intValue();
		return count;
	}

	@Override
	public TaskActionLog saveTaskActionLog(TaskActionLog taskActionLog) {
		hibernateTemplate.save(taskActionLog);
		return taskActionLog;
	}

	@Override
	public TaskAssigneeHistory saveTaskAssigneeHistory(TaskAssigneeHistory taskAssigneeHistory) {
		hibernateTemplate.save(taskAssigneeHistory);
		return taskAssigneeHistory;
	}

	@SuppressWarnings({ "deprecation", "unchecked" })
	@Override
	public List<Task> fetchAdvancedSearchTasks(TaskVO vo) {
		List<Task> tasks = new ArrayList<>();
		Integer moduleCode = vo.getModuleCode();
		String moduleItemKey = vo.getModuleItemKey();
		String assigneePersonId = vo.getAssigneePersonId();
		List<String> taskStatus = vo.getTaskStatus();
		Timestamp dueDateFrom = vo.getDueDateFrom();
		Timestamp dueDateTo = vo.getDueDateTo();
		Conjunction and = Restrictions.conjunction();
		Disjunction or = Restrictions.disjunction();
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			Criteria searchCriteria = session.createCriteria(Task.class);
			searchCriteria.createAlias("taskStatus", "taskStatus");
			searchCriteria.addOrder(Order.desc(CREATETIMESTAMP));
			if (moduleCode != null) {
				and.add(Restrictions.like(MODULECODE, moduleCode));
			}
			if (moduleItemKey != null && !moduleItemKey.isEmpty()) {
				and.add(Restrictions.like(MODULEITEMKEY, moduleItemKey));
			}
			if (assigneePersonId != null && !assigneePersonId.isEmpty()) {
				and.add(Restrictions.like(ASSIGNEEPERSONID, assigneePersonId));
			}
			if (taskStatus != null && !taskStatus.isEmpty()) {
				for (String status : taskStatus) {
					if (status != null && !status.isEmpty()) {
						or.add(Restrictions.like("taskStatus.description", status).ignoreCase());
					}
				}
			}
			if (dueDateFrom != null) {
				and.add(Restrictions.ge("dueDate", dueDateFrom));
			}
			if (dueDateTo != null) {
				and.add(Restrictions.le("dueDate", dueDateTo));
			}
			searchCriteria.add(and);
			searchCriteria.add(or);
			searchCriteria.setResultTransformer(Criteria.DISTINCT_ROOT_ENTITY);
			tasks = searchCriteria.list();
		} catch (Exception e) {
			logger.error("Exception in advancedSearchResult : {}", e.getMessage());
		}
		return tasks;
	}

	@Override
	public List<TaskActionLog> fetchTaskActionHistoryByTaskId(Integer taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<TaskActionLog> query = builder.createQuery(TaskActionLog.class);
		Root<TaskActionLog> rootTaskActionLog = query.from(TaskActionLog.class);
		query.where(builder.equal(rootTaskActionLog.get(TASKID), taskId));
		query.orderBy(builder.desc(rootTaskActionLog.get("actionLogId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer fetchAwardIdByAwardNumberAndSequenceNumber(String awardNumber, String sequenceNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT awardId FROM Award T1 LEFT OUTER JOIN Task T2 ON T2.moduleItemKey = T1.awardNumber  WHERE T2.startModuleSubItemKey = 1 and T1.awardNumber=:awardNumber ";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		query.setParameter("sequenceNumber", sequenceNumber);
		return query.uniqueResult();
	}

	@Override
	public String fetchTaskAssigneeByTaskId(Integer taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT assigneePersonId FROM Task WHERE taskId=:taskId ";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(TASKID, taskId);
		return query.uniqueResult();
	}

	@Override
	public List<Task> fetchTasksByModuleCodeAndModuleItemId(Integer moduleCode, Integer moduleItemId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Task> query = builder.createQuery(Task.class);
		Root<Task> rootTask = query.from(Task.class);
		Predicate predicateModuleCode = builder.equal(rootTask.get(MODULECODE), moduleCode);
		Predicate predicateModuleItemId = builder.equal(rootTask.get(MODULEITEMID), moduleItemId);
		query.where(builder.and(predicateModuleCode, predicateModuleItemId));
		query.orderBy(builder.desc(rootTask.get(CREATETIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Task> fetchTasksByParams(Integer moduleCode, Integer moduleItemId, List<String> taskStatusCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Task> query = builder.createQuery(Task.class);
		Root<Task> rootTask = query.from(Task.class);
		Predicate predicateModuleCode = builder.equal(rootTask.get(MODULECODE), moduleCode);
		Predicate predicateModuleItemId = builder.equal(rootTask.get(MODULEITEMID), moduleItemId);
		Predicate predicateTaskStatusCode = rootTask.get(TASKSTATUSCODE).in(taskStatusCodes);
		query.where(builder.and(predicateModuleCode, predicateModuleItemId, predicateTaskStatusCode));
		query.orderBy(builder.desc(rootTask.get(CREATETIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Task> fetchTasksByModuleCodeAndModuleItemKey(Integer moduleCode, String moduleItemKey) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Task> query = builder.createQuery(Task.class);
		Root<Task> rootTask = query.from(Task.class);
		Predicate predicateModuleCode = builder.equal(rootTask.get(MODULECODE), moduleCode);
		Predicate predicateModuleItemKey = builder.equal(rootTask.get(MODULEITEMKEY), moduleItemKey);
		query.where(builder.and(predicateModuleCode, predicateModuleItemKey));
		query.orderBy(builder.desc(rootTask.get(CREATETIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<String> getSectionCodeBasedOnTaskTypeCode(String taskTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String taskSectionCode = "SELECT sectionCode FROM TaskSectionMapping WHERE taskTypeCode=:taskTypeCode";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(taskSectionCode);
		query.setParameter("taskTypeCode", taskTypeCode);
		return query.getResultList();
	}

	@Override
	public Integer checkPersonInTask(String personId, String taskStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String personCount = "SELECT count(*) FROM Task WHERE assigneePersonId=:assigneePersonId and taskStatusCode =:taskStatusCode";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Long> query = session.createQuery(personCount);
		query.setParameter(ASSIGNEEPERSONID, personId);
		query.setParameter(TASKSTATUSCODE, taskStatusCode);
		return query.uniqueResult().intValue();
	}

	@Override
	public String fetchTaskTypeCodeByTaskId(Integer taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT taskTypeCode FROM Task WHERE taskId=:taskId ";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(TASKID, taskId);
		return query.uniqueResult();
	}

	@Override
	public List<Integer> getPersonTaskIds(String moduleItemId, Integer moduleCode, String personId, String taskStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String personCount = "SELECT taskId FROM Task WHERE assigneePersonId=:assigneePersonId and taskStatusCode =:taskStatusCode and moduleItemId =: moduleItemId and moduleCode=:moduleCode ";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Integer> query = session.createQuery(personCount);
		query.setParameter(ASSIGNEEPERSONID, personId);
		query.setParameter(TASKSTATUSCODE, taskStatusCode);
		query.setParameter(MODULEITEMID, moduleItemId);
		query.setParameter(MODULECODE, moduleCode);
		return query.getResultList();
	}

	@Override
	public String fetchTaskAssigneeHistoryByActionLogId(Integer actionLogId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String newAssigneePersonId = "SELECT newAssigneePersonId FROM TaskAssigneeHistory WHERE actionLogId=:actionLogId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(newAssigneePersonId);
		query.setParameter("actionLogId", actionLogId);
		return query.uniqueResult();
	}

	@Override
	public Integer checkEditableSectionByTaskId(String personId, String taskId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String sectionCount = "SELECT count(*) FROM ModuleVariableSection WHERE personId=:personId and subModuleItemKey =:taskId";
		Query query = session.createQuery(sectionCount);
		query.setParameter("personId", personId);
		query.setParameter(TASKID, taskId);
		return ((Long) query.getSingleResult()).intValue();
	}

	@Override
	public Integer checkCountOfAssignedTaskByParams(String personId, List<String> taskStatusCodes, String taskTypeCode, String moduleItemId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String personCount = "SELECT count(*) FROM Task WHERE moduleItemId =:moduleItemId and assigneePersonId =:assigneePersonId and taskTypeCode =:taskTypeCode and taskStatusCode in (:taskStatusCode)";
		Query query = session.createQuery(personCount);
		query.setParameter(MODULEITEMID, moduleItemId);
		query.setParameter(ASSIGNEEPERSONID, personId);
		query.setParameter("taskTypeCode", taskTypeCode);
		query.setParameter(TASKSTATUSCODE, taskStatusCodes);
		return ((Long) query.getSingleResult()).intValue();
	}

	@Override
	public List<Task> fetchAllTasksByModuleCodeAndTaskStatusCodes(Integer moduleCode, List<String> taskStatusCodes) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Task> query = builder.createQuery(Task.class);
		Root<Task> rootTask = query.from(Task.class);
		Predicate predicateModuleCode = builder.equal(rootTask.get(MODULECODE), moduleCode);
		Predicate predicateTaskStatusCode = builder.not(rootTask.get(TASKSTATUSCODE).in(taskStatusCodes));
		query.where(builder.and(predicateModuleCode, predicateTaskStatusCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer checkTaskExisted(Integer moduleCode, String moduleItemId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "select count(1) from Task where moduleItemId=:moduleItemId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter(MODULEITEMID, moduleItemId);
		return Integer.parseInt(query.getSingleResult().toString());
	}
    
}
