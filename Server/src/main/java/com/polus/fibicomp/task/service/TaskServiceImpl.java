package com.polus.fibicomp.task.service;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardTaskTypeMapping;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.businessrule.dao.BusinessRuleDao;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.businessrule.vo.EvaluateValidationRuleVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.inbox.dao.InboxDao;
import com.polus.fibicomp.inbox.service.InboxService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.roles.service.AuthorizationService;
import com.polus.fibicomp.sectionwiseedit.dao.SectionWiseEditDao;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.task.dao.TaskDao;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.pojo.TaskActionLog;
import com.polus.fibicomp.task.pojo.TaskAssigneeHistory;
import com.polus.fibicomp.task.pojo.TaskAttachment;
import com.polus.fibicomp.task.pojo.TaskComment;
import com.polus.fibicomp.task.pojo.TaskCommentAttachment;
import com.polus.fibicomp.task.pojo.TaskFileData;
import com.polus.fibicomp.task.pojo.TaskStatusHistory;
import com.polus.fibicomp.task.pojo.TaskType;
import com.polus.fibicomp.task.vo.TaskVO;
import com.polus.fibicomp.workflow.comparator.WorkflowComparator;
import com.polus.fibicomp.workflow.comparator.WorkflowDetailComparator;
import com.polus.fibicomp.workflow.dao.WorkflowDao;
import com.polus.fibicomp.workflow.pojo.Workflow;
import com.polus.fibicomp.workflow.pojo.WorkflowDetail;
import com.polus.fibicomp.workflow.service.WorkflowService;

@Transactional
@Service(value = "taskService")
public class TaskServiceImpl implements TaskService {

	protected static Logger logger = LogManager.getLogger(TaskServiceImpl.class.getName());

	@Autowired
	private TaskDao taskDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private EmailService emailService;

	@Autowired
	private PrintService printService;

	@Autowired
	private BusinessRuleService businessRuleService;

	@Autowired
	private WorkflowDao workflowDao;

	@Autowired
	public BusinessRuleDao businessRuleDao;

	@Autowired
	private WorkflowService workflowService;

	@Autowired
	private AwardDao awardDao;

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private ServiceRequestDao serviceRequestDao;

	@Autowired
	private SectionWiseEditDao sectionWiseEditDao;

	@Autowired
	public InboxService inboxService;

	@Autowired
	public InboxDao inboxDao;

	@Autowired
	public AuthorizationService authorizationService;

	@Autowired
	public AwardService awardService;

	@Override
	public String getTaskLookUpData() {
 		TaskVO vo = new TaskVO();
		List<TaskType> taskTypes = taskDao.fetchAllTaskType();
		for (TaskType taskType : taskTypes) {
			List<AwardTaskTypeMapping> awardTaskTypeMappings = taskType.getAwardTaskTypeMappings();
			if (awardTaskTypeMappings != null && !awardTaskTypeMappings.isEmpty()) {
				for (AwardTaskTypeMapping awardTaskTypeMapping : awardTaskTypeMappings) {
					if (awardTaskTypeMapping.getTypeCode() != null) {
						awardTaskTypeMapping.setServiceRequestType(serviceRequestDao.fetchServiceRequestTypeById(awardTaskTypeMapping.getTypeCode()));
					}
				}
			}
		}
		vo.setTaskTypes(taskTypes);
		vo.setTaskStatusList(taskDao.fetchAllTaskStatus());
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateTask(MultipartFile[] files, String formDataJSON, TaskVO taskVO) {
		try {
			if (formDataJSON != null && !formDataJSON.isEmpty()) {
				ObjectMapper mapper = new ObjectMapper();
				taskVO = mapper.readValue(formDataJSON, TaskVO.class);
			}
			Task task = taskVO.getTask();
			List<String> taskStatusCodes = new ArrayList<>();
			taskStatusCodes.add(Constants.TASK_STATUS_CODE_OPEN);
			taskStatusCodes.add(Constants.TASK_STATUS_CODE_IN_PROGRESS);
			taskStatusCodes.add(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS);
			taskStatusCodes.add(Constants.TASK_STATUS_CODE_RETURNED);
			Boolean isInsert = Boolean.FALSE;
			Integer personCount = taskDao.checkCountOfAssignedTaskByParams(task.getAssigneePersonId(), taskStatusCodes, task.getTaskTypeCode(), task.getModuleItemId());
			if (task.getTaskId() != null) {
				String taskType = task.getTaskType().getDescription();
				String taskTypeCode = taskDao.fetchTaskTypeCodeByTaskId(task.getTaskId());
				String assigneePersonId = taskDao.fetchTaskAssigneeByTaskId(task.getTaskId());
				if (personCount > 0) {
					if (!task.getAssigneePersonId().equals(assigneePersonId) && (task.getTaskTypeCode().equals(taskTypeCode)) || (task.getAssigneePersonId().equals(assigneePersonId) && !task.getTaskTypeCode().equals(taskTypeCode))) {
						taskVO.setMessage("Task already exists");
						taskVO.setIsTaskExist(true);
					}
					task.setTaskTypeCode(taskTypeCode);
					task.setTaskType(taskDao.fetchTaskTypeByTaskTypeCode(taskTypeCode));
					task.setAssigneePersonId(assigneePersonId);
					addTaskAttachments(taskVO, files);
				} else {
					addTaskAttachments(taskVO, files);
					if (!assigneePersonId.equals(task.getAssigneePersonId())) {
						taskVO.setOldAssigneePersonId(assigneePersonId);
						taskVO.setNewAssigneePersonId(task.getAssigneePersonId());
						taskVO.setModuleItemId(Integer.parseInt(task.getModuleItemId()));
						taskVO.setIsGmUpdate("Y");
						taskReassign(taskVO);
					}
				}
				if (!taskTypeCode.equals(task.getTaskTypeCode())) {
					inboxDao.markReadMessage(Constants.AWARD_MODULE_CODE, task.getModuleItemId(), assigneePersonId, Constants.MESSAGE_TYPE_ASSIGN_TASK, task.getTaskId().toString(), Constants.AWARD_TASK_SUBMODULE_CODE);
					deleteAllSections(task.getModuleItemId(), task.getTaskId().toString(), Constants.AWARD_MODULE_CODE, Constants.AWARD_TASK_SUBMODULE_CODE, assigneePersonId, Constants.TASK_EDITABLE_SECTION_VARIABLE_TYPE, taskTypeCode);
					taskVO.setModuleCode(Constants.AWARD_MODULE_CODE);
					taskVO.setSubModuleCode(Constants.AWARD_TASK_SUBMODULE_CODE);
					taskVO.setModuleItemKey(task.getModuleItemKey());
					taskVO.setSubModuleItemKey(task.getTaskId().toString());
					taskVO.setModuleItemId(Integer.parseInt(task.getModuleItemId()));
					taskVO.setTaskTypeCode(task.getTaskTypeCode());
					taskVO.setPersonId(task.getAssigneePersonId());
					addTaskEditableFields(taskVO);
				}
				if (!taskTypeCode.equals(task.getTaskTypeCode()) || !assigneePersonId.equals(task.getAssigneePersonId())) {
					String userMessage = getUserMessageForTak(task);		
					inboxService.addTaskMessageToInbox(task, AuthenticatedUser.getLoginUserName(), Constants.MESSAGE_TYPE_ASSIGN_TASK, "T", taskVO.getSubModuleCode(), userMessage);
					updateEditableFields(task.getModuleItemId(), task.getTaskId(), task.getAssigneePersonId(), assigneePersonId);
				}
				String systemComment = taskType + " Task";
				saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_UPDATED, systemComment, AuthenticatedUser.getLoginUserName());
			}
			else {
				isInsert = Boolean.TRUE;
				if (personCount > 0) {
					taskVO.setMessage("Task already exists");
					taskVO.setIsTaskExist(true);
				} else {
					task.setTaskStatusCode(Constants.TASK_STATUS_CODE_OPEN);
					task = addTaskAttachments(taskVO, files);
					TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
					String systemComment = taskType.getDescription() + " Task";
					TaskActionLog taskActionLog = saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_CREATED, systemComment, AuthenticatedUser.getLoginUserName());
					saveTaskStatusHistory(task.getTaskId(), Constants.TASK_STATUS_CODE_OPEN, taskActionLog.getActionLogId(), AuthenticatedUser.getLoginUserName());
					taskVO.setIsGmUpdate("N");
					taskVO.setTaskId(task.getTaskId());
					taskVO.setModuleItemId(Integer.parseInt(task.getModuleItemId()));
					String userMessage = getUserMessageForTak(task);
					inboxService.addTaskMessageToInbox(task, AuthenticatedUser.getLoginUserName(), Constants.MESSAGE_TYPE_ASSIGN_TASK, "T", taskVO.getSubModuleCode(), userMessage);
				}
			}
			//addAwardPersonRoles(Integer.parseInt(task.getModuleItemId()), Constants.VIEW_AWARD_ROLE_ID,	task.getAssigneePersonId(), taskVO.getUpdateUser());
			taskVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(task.getModuleItemId(), Constants.ZERO, Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.AWARD_SUBMODULE_CODE));
			List<Integer> taskIds = taskDao.getPersonTaskIds(task.getModuleItemId(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.TASK_STATUS_CODE_IN_PROGRESS);
			if (taskIds != null && !taskIds.isEmpty()) {
				for (Integer taskId : taskIds) {
					List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(task.getModuleItemId(), taskId.toString(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.AWARD_TASK_SUBMODULE_CODE);
					if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
						taskVO.getSectionTypeCodes().addAll(moduleVariableSections);
					}
				}
			}
			taskVO.setTaskCount(fetchTaskCountByModuleItemKey(task.getModuleItemKey()));
			commonDao.doflush();
			if ((personCount <= 0) && (Boolean.TRUE.equals(isInsert) || (Boolean.FALSE.equals(isInsert) && taskVO.getOldAssigneePersonId() !=null && !taskVO.getOldAssigneePersonId().equals(task.getAssigneePersonId())))) {
				sendNotificationsForTask(taskVO);
			}
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateTask :{}", e.getMessage());
			throw new ApplicationException("Exception in saveOrUpdateTask", e, Constants.JAVA_ERROR);
		}
		return commonDao.convertObjectToJSON(taskVO);
	}

	private void deleteAllSections(String moduleItemId, String subModuleItemKey, Integer moduleCode,
			Integer subModuleCode, String personId, String variableType, String taskTypeCode) {
		List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getModuleVariableSections(moduleItemId, subModuleItemKey, moduleCode, subModuleCode, personId, variableType, Integer.parseInt(taskTypeCode));
		if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
			for (ModuleVariableSection moduleVariableSection : moduleVariableSections) {
				sectionWiseEditDao.deleteModuleVariableSection(moduleVariableSection);
			}
		}
	}

	private void updateEditableFields(String moduleItemId, Integer taskId, String newPersonId, String oldPersonId) {
		List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getModuleVariableSections(moduleItemId, taskId.toString(), Constants.AWARD_MODULE_CODE, Constants.AWARD_TASK_SUBMODULE_CODE, oldPersonId, Constants.TASK_EDITABLE_SECTION_VARIABLE_TYPE, null);
		if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
			for (ModuleVariableSection moduleVariableSection : moduleVariableSections) {
				moduleVariableSection.setPersonId(newPersonId);
				sectionWiseEditDao.saveorUpdateModuleVariableSection(moduleVariableSection);
			}
		}
	}

	private Task addTaskAttachments(TaskVO taskVO, MultipartFile[] files) {
		Task task = taskVO.getTask();
		List<TaskAttachment> attachments = task.getTaskAttachments();
		List<TaskAttachment> newAttachments = taskVO.getTaskAttachments();
		if (taskVO.getTaskAttachment() != null) {
			newAttachments.add(taskVO.getTaskAttachment());
		}
		List<TaskAttachment> taskAttachments = new ArrayList<>();
		Boolean isReplaced = false;
		if (files != null) {
			for (int i = 0; i < files.length; i++) {
				for (TaskAttachment newAttachment : newAttachments) {
					String replaceFileName = newAttachment.getFileName();
					boolean isRenameRequired = false;
					int count = 1;
					isRenameRequired = checkForDuplication(newAttachment.getFileName(), attachments);
					while(isRenameRequired) {
						 replaceFileName = newAttachment.getFileName();
						 replaceFileName = generateFileName(replaceFileName, count);
						 count = count +1;
						 isRenameRequired = checkForDuplication(replaceFileName, attachments);
					}
					if (newAttachment.getAttachmentId() != null) {
						for (TaskAttachment attachment : attachments) {
							if (attachment.getAttachmentId() != null && attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
								isReplaced = true;
								File file = new File(files[i].getOriginalFilename());
								String fileName = file.getName();
								TaskAttachment taskAttachment = addNewTaskAttachment(newAttachment, files[i], fileName, replaceFileName, task);
								taskAttachment.setTask(task);
								taskAttachments.add(taskAttachment);
							}
						}
					} else {
						File file = new File(files[i].getOriginalFilename());
						String fileName = file.getName();
						if (newAttachment.getFileName().equals(fileName)) {
							TaskAttachment taskAttachment = addNewTaskAttachment(newAttachment, files[i], fileName, replaceFileName, task);
							taskAttachments.add(taskAttachment);
						}
					}
				}
			}
		}
		task.getTaskAttachments().addAll(taskAttachments);
		task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		if (!isReplaced) {
			task = taskDao.saveOrUpdateTask(task);
		}
		taskVO.setTask(task);
		taskVO.setStatus(true);
		return task;
	}

	@Override
	public String saveOrUpdateTaskComment(MultipartFile[] files, String formDataJSON, TaskVO taskVO) {
		try {
			if (formDataJSON != null && !formDataJSON.isEmpty()) {
				ObjectMapper mapper = new ObjectMapper();
				taskVO = mapper.readValue(formDataJSON, TaskVO.class);
			}
			TaskComment taskComment = taskVO.getTaskComment();
			Integer actionLogId = taskDao.fetchTaskActionLogBasedOnTaskId(taskComment.getTaskId()).getActionLogId();
			List<TaskCommentAttachment> attachments = taskComment.getTaskCommentAttachments();
			List<TaskCommentAttachment> newAttachments = taskVO.getTaskCommentAttachments();
			List<TaskCommentAttachment> taskCommentAttachments = new ArrayList<>();
			if (taskVO.getTaskCommentAttachment() != null) {
				newAttachments.add(taskVO.getTaskCommentAttachment());
			}
			Boolean isReplaced = false;
			if (files != null) {
				for (int i = 0; i < files.length; i++) {
					for (TaskCommentAttachment newAttachment : newAttachments) {
						String replaceFileName = newAttachment.getFileName();
						boolean isRenameRequired = false;
						int count = 1;
						isRenameRequired = checkForDuplicationTaskCommmentAttachment(newAttachment.getFileName(), attachments);
						while (isRenameRequired) {
							replaceFileName = newAttachment.getFileName();
							replaceFileName = generateFileName(replaceFileName, count);
							count = count + 1;
							isRenameRequired = checkForDuplicationTaskCommmentAttachment(replaceFileName, attachments);
						}
						if (newAttachment.getAttachmentId() != null) {
							for (TaskCommentAttachment attachment : attachments) {
								if (attachment.getAttachmentId() != null && attachment.getAttachmentId().equals(newAttachment.getAttachmentId())) {
									isReplaced = true;
									File file = new File(files[i].getOriginalFilename());
									String fileName = file.getName();
									TaskCommentAttachment taskCommentAttachment = addNewTaskCommentAttachment(newAttachment, files[i], fileName,
											replaceFileName, taskComment);
									taskCommentAttachment.setTaskId(taskVO.getTaskId());
									taskCommentAttachment.setTaskComment(taskComment);
									taskCommentAttachments.add(taskCommentAttachment);
								}
							}
						} else {
							File file = new File(files[i].getOriginalFilename());
							String fileName = file.getName();
							if (newAttachment.getFileName().equals(fileName)) {
								 TaskCommentAttachment  taskCommentAttachment = addNewTaskCommentAttachment(newAttachment, files[i], fileName,
										replaceFileName, taskComment);
								taskCommentAttachments.add(taskCommentAttachment);
							}
						}
					}
				}
			}
			taskComment.getTaskCommentAttachments().addAll(taskCommentAttachments);
			taskComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			taskComment.setActionLogId(actionLogId);
			taskComment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(taskVO.getUpdateUser()));
			if (!isReplaced) {
				taskComment = taskDao.saveOrUpdateTaskComment(taskComment);
			}
			taskVO.setTaskComment(taskComment);
			taskVO.setStatus(true);
			Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
			TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
			String systemComment = taskType.getDescription() + " Task";
			TaskActionLog actionLog = saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_ADDED_COMMENT_ATTACHMENT, systemComment, taskVO.getUpdateUser());
			saveTaskStatusHistory(task.getTaskId(), Constants.TASK_STATUS_CODE_IN_PROGRESS,	actionLog.getActionLogId(), taskVO.getUpdateUser());
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateTaskComment :{}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public String deleteTaskComment(TaskVO taskVO) {
		taskDao.deleteTaskComment(taskVO.getTaskCommentId());
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
		String systemComment = taskType.getDescription() + " Task";
		TaskActionLog actionLog = saveTaskActionLogDetails(taskVO.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_COMMENT_DELETED, systemComment, taskVO.getUpdateUser());
		saveTaskStatusHistory(taskVO.getTaskId(), task.getTaskStatusCode(),	actionLog.getActionLogId(), taskVO.getUpdateUser());
		return commonDao.convertObjectToJSON(taskVO);
	}

	private TaskAttachment addNewTaskAttachment(TaskAttachment attachment, MultipartFile file, String fileName, String replacedFileName, Task task) {
			TaskAttachment taskAttachment = new TaskAttachment();
			try {
				if (attachment.getFileName().equals(fileName)) {
					taskAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					taskAttachment.setUpdateUser(AuthenticatedUser.getLoginUserName());
					taskAttachment.setMimeType(file.getContentType());
					taskAttachment.setFileName(replacedFileName);
					taskAttachment.setMimeType(file.getContentType());
					taskAttachment.setTask(task);
					taskAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(attachment.getUpdateUser()));
					TaskFileData fileData = new TaskFileData();
					fileData.setData(file.getBytes());
					fileData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					fileData.setUpdateUser(attachment.getUpdateUser());
					fileData = taskDao.saveFileData(fileData);
					taskAttachment.setFileDataId(fileData.getFileDataId());
					taskAttachment.setAttachment(file.getBytes());
				}
			} catch (Exception e) {
				logger.error("Exception in addNewTaskAttachment :{}", e.getMessage());
			}
			return taskAttachment;
	}

	private TaskCommentAttachment addNewTaskCommentAttachment(TaskCommentAttachment attachment, MultipartFile file, String fileName, String replacedFileName, TaskComment taskComment) {
		TaskCommentAttachment taskCommentAttachment = new TaskCommentAttachment();
		try {
			if (attachment.getFileName().equals(fileName)) {
				taskCommentAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				taskCommentAttachment.setUpdateUser(attachment.getUpdateUser());
				taskCommentAttachment.setMimeType(file.getContentType());
				taskCommentAttachment.setTaskId(taskComment.getTaskId());
				taskCommentAttachment.setFileName(replacedFileName);
				taskCommentAttachment.setMimeType(file.getContentType());
				taskCommentAttachment.setTaskComment(taskComment);
				TaskFileData fileData = new TaskFileData();
				fileData.setData(file.getBytes());
				fileData.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				fileData.setUpdateUser(attachment.getUpdateUser());
				fileData = taskDao.saveFileData(fileData);
				taskCommentAttachment.setFileDataId(fileData.getFileDataId());
				taskCommentAttachment.setAttachment(file.getBytes());
			}
		} catch (Exception e) {
			logger.error("Exception in addNewTaskCommentAttachment :{}", e.getMessage());
		}
		return taskCommentAttachment;
	}

	private String generateFileName(String replaceFileName, int count) {
		String fileNameSplit = replaceFileName.split("\\.")[0];
		String extension = replaceFileName.split("\\.")[1];
		return fileNameSplit + "(" + count + ")" + "." + extension;
	}

	private boolean checkForDuplication(String fileName, List<TaskAttachment> attachments) {
		for (TaskAttachment attachment : attachments) {
			if (fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	private boolean checkForDuplicationTaskCommmentAttachment(String fileName, List<TaskCommentAttachment> attachments) {
		for (TaskCommentAttachment attachment : attachments) {
			if (fileName.equals(attachment.getFileName())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public TaskVO sendTaskNotification(TaskVO taskVO, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(taskVO.getModuleCode());
		emailServiceVO.setSubModuleCode(taskVO.getSubModuleCode().toString());
		emailServiceVO.setModuleItemKey(taskVO.getModuleItemId().toString());
		emailServiceVO.setSubModuleItemKey(taskVO.getTaskId().toString());
		emailServiceVO.setPlaceHolder(getDynamicPlaceholders(taskVO));
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		return taskVO;
	}

	private Map<String, String> getDynamicPlaceholders(TaskVO taskVO) {
		Map<String, String> placeHolder = new HashMap<>();
		if (taskVO.getOldAssigneePersonId() != null && taskVO.getNewAssigneePersonId() != null) {
			String oldAssigneeName = personDao.getPersonFullNameByPersonId(taskVO.getOldAssigneePersonId());
			String newAssigneeName = personDao.getPersonFullNameByPersonId(taskVO.getNewAssigneePersonId());
			placeHolder.put("{NEW_ASSIGNEE_NAME}", newAssigneeName);
			placeHolder.put("{OLD_ASSIGNEE_NAME}", oldAssigneeName);
		}
		if (taskVO.getTaskTypeCode() != null && taskVO.getTaskTypeCode().equals(Constants.TASK_CONFIRM_PROJECT_DETAILS)) {
			List<Task> tasks = taskDao.fetchTasksByModuleCodeAndModuleItemId(taskVO.getModuleCode(), taskVO.getModuleItemId());
			for (Task task : tasks) {
				if (task.getTaskTypeCode().equals(Constants.TASK_FINANCE_ACCOUNT_CREATION)) {
					String finaceAssigneeName = personDao.getPersonFullNameByPersonId(task.getAssigneePersonId());
					placeHolder.put("{FINANCE_ASSIGNEE_NAME}", finaceAssigneeName);
				}
			}
		}
		placeHolder.put("{WORKFLOW_COMMENT}", taskVO.getApproveComment() != null ? taskVO.getApproveComment() : "No Comments");
		String stopName = commonService.getPlaceHolderDataForRouting(taskVO.getApproverStopNumber(),taskVO.getMapId(),taskVO.getWorkFlowDetailId());
		placeHolder.put("{APPROVER_STOP_NAME}", stopName != null ?stopName : " ");
		return placeHolder;
	}

	@Override
	public ResponseEntity<String> loadTaskDetailsById(TaskVO taskVO) {
		if (taskVO.getTaskId() != null) {
			Task task = taskDao.fetchTaskByTaskIdAndModuleItemId(taskVO.getModuleItemKey(), taskVO.getTaskId());
			if(task == null) {
				return new ResponseEntity<>("No entity found", HttpStatus.FORBIDDEN);
			}
			List<TaskAttachment> attachments = task.getTaskAttachments();
			if (attachments != null && !attachments.isEmpty()) {
				for (TaskAttachment attachment : task.getTaskAttachments()) {
					attachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(attachment.getUpdateUser()));
				}
			}
			loadTaskUserFullNames(task);
			taskVO.setTask(task);
			taskVO.setTaskComments(taskDao.fetchTaskCommentBasedOnTaskId(taskVO.getTaskId()));
			Integer canApproveRouting = businessRuleDao.canApproveRouting(taskVO.getModuleItemId().toString(), taskVO.getPersonId(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode());
			taskVO.setCanApproveRouting(canApproveRouting.toString());
			Integer personCount = taskDao.checkPersonInTask(taskVO.getPersonId(), Constants.TASK_STATUS_CODE_IN_PROGRESS);
			if (personCount > 0) {
				taskVO.setSectionTypeCodes(sectionWiseEditDao.getTaskEditableSections(task.getModuleItemId(), task.getTaskId().toString(), task.getModuleCode(), Constants.AWARD_TASK_SUBMODULE_CODE, Constants.TASK_EDITABLE_SECTION_VARIABLE_TYPE, taskVO.getPersonId()));
			}
			taskVO.setTaskStatusCode(taskDao.fetchTaskByTaskId(task.getTaskId()).getTaskStatusCode());		
		}
		return new ResponseEntity<>(commonDao.convertObjectToJSON(taskVO), HttpStatus.OK);
	}

	@Override
	public ResponseEntity<byte[]> downloadTaskAttachment(Integer attachmentId) {
		TaskAttachment attachment = taskDao.fetchTaskAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			TaskFileData fileData = taskDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getData());
		} catch (Exception e) {
			logger.error("Exception in downloadTaskAttachment :{}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String deleteTaskAttachment(TaskVO taskVO) {
		taskDao.deleteTaskAttachment(taskVO.getTaskAttachmentId());
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public String fetchTaskCommentsByTaskId(TaskVO taskVO) {
		List<TaskComment> taskComments = taskDao.fetchTaskCommentBasedOnTaskId(taskVO.getTaskId());
		if (taskComments != null && !taskComments.isEmpty()) {
			for (TaskComment taskComment : taskComments) {
				if (taskComment.getUpdateUser() != null) {
					taskComment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(taskComment.getUpdateUser()));
				}
			}
		}
		taskVO.setAssigneePersonId(taskDao.fetchTaskByTaskId(taskVO.getTaskId()).getAssigneePersonId());
		taskVO.setTaskStatusCode(taskDao.fetchTaskByTaskId(taskVO.getTaskId()).getTaskStatusCode());
		taskVO.setTaskComments(taskComments);
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public ResponseEntity<byte[]> downloadTaskCommentAttachment(Integer attachmentId) {
		TaskCommentAttachment attachment = taskDao.fetchTaskCommentAttachmentById(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			TaskFileData fileData = taskDao.getFileDataById(attachment.getFileDataId());
			attachmentData = printService.setAttachmentContent(attachment.getFileName(), fileData.getData());
		} catch (Exception e) {
			logger.error("Exception in downloadTaskCommentAttachment :{}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String deleteTaskCommentAttachment(TaskVO taskVO) {
		taskDao.deleteTaskCommentAttachment(taskVO.getTaskCommentAttachmentId());
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public String startTask(TaskVO taskVO) {
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		taskVO.setStatus(true);
		taskVO.setTaskTypeCode(task.getTaskTypeCode());
		taskVO.setModuleCode(task.getModuleCode());
		taskVO.setPersonId(task.getAssigneePersonId());
		taskVO.setSubModuleItemKey(task.getTaskId().toString());
		taskVO.setSubModuleCode(Constants.AWARD_TASK_SUBMODULE_CODE);
		if (taskDao.checkEditableSectionByTaskId(task.getAssigneePersonId(), task.getTaskId().toString()).equals(0)) {
			addTaskEditableFields(taskVO);
		}
		task.setTaskStatusCode(Constants.TASK_STATUS_CODE_IN_PROGRESS);
		task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_IN_PROGRESS));
		task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		task.setUpdateUser(AuthenticatedUser.getLoginUserName());
		task.setModuleItemId(taskVO.getModuleItemId().toString());
		taskDao.saveOrUpdateTask(task);
		TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
		String systemComment = taskType.getDescription() + " Task";
		TaskActionLog taskActionLog = saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_STARTED, systemComment, AuthenticatedUser.getLoginUserName());
		saveTaskStatusHistory(task.getTaskId(), Constants.TASK_STATUS_CODE_IN_PROGRESS, taskActionLog.getActionLogId(), AuthenticatedUser.getLoginUserName());
		loadTaskUserFullNames(task);
		taskVO.setTask(task);
		taskVO.setSectionTypeCodes(sectionWiseEditDao.getTaskEditableSections(task.getModuleItemId(), task.getTaskId().toString(), task.getModuleCode(), Constants.AWARD_TASK_SUBMODULE_CODE, Constants.TASK_EDITABLE_SECTION_VARIABLE_TYPE, taskVO.getPersonId()));
//		inboxService.removeTaskMessageFromInbox(Integer.parseInt(task.getModuleItemId()), task.getTaskId(), task.getModuleCode(), taskVO.getSubModuleCode());
		taskVO.setTaskCount(fetchTaskCountByModuleItemKey(task.getModuleItemKey()));
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public String completeTaskDetails(TaskVO taskVO) {
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
		task.setTaskStatusCode(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS);
		task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS));
		task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		task.setUpdateUser(AuthenticatedUser.getLoginUserName());
		taskVO.setTask(taskDao.saveOrUpdateTask(task));
		String systemComment = taskType.getDescription() + " Task";
		TaskActionLog taskActionLog = saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_SUBMITTED, systemComment, AuthenticatedUser.getLoginUserName());
		saveTaskStatusHistory(task.getTaskId(), Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS, taskActionLog.getActionLogId(), AuthenticatedUser.getLoginUserName());
		loadTaskUserFullNames(task);
		taskVO = buildTaskWorkflow(taskVO);
		taskVO = fetchPreviousWorkFlowsList(taskVO);
		if (taskVO.getWorkflow() != null && taskVO.getWorkflow().getWorkflowDetails().isEmpty() || taskVO.getWorkflow() == null) {
			task.setTaskStatusCode(Constants.TASK_STATUS_CODE_COMPLETED);
			task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_COMPLETED));
			task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			task.setUpdateUser(AuthenticatedUser.getLoginUserName());
			task.setEndModuleSubItemKey(taskVO.getEndModuleSubItemKey());
			taskVO.setTask(taskDao.saveOrUpdateTask(task));
			String completeComment = taskType.getDescription() + " Task";
			TaskActionLog actionLog = saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_COMPLETED, completeComment, AuthenticatedUser.getLoginUserName());
			saveTaskStatusHistory(task.getTaskId(), Constants.TASK_STATUS_CODE_COMPLETED, actionLog.getActionLogId(), AuthenticatedUser.getLoginUserName());
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			commonService.setNotificationRecipients(task.getAssigneePersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
			taskVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(task.getModuleItemId(), Constants.ZERO, Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.AWARD_SUBMODULE_CODE));
			List<Integer> taskIds = taskDao.getPersonTaskIds(task.getModuleItemId(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.TASK_STATUS_CODE_IN_PROGRESS);
			if (taskIds != null && !taskIds.isEmpty()) {
				for (Integer taskId : taskIds) {
					List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(task.getModuleItemId(), taskId.toString(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.AWARD_TASK_SUBMODULE_CODE);
					if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
						taskVO.getSectionTypeCodes().addAll(moduleVariableSections);
					}
				}
			}
			inboxDao.markReadMessage(Constants.AWARD_MODULE_CODE, task.getModuleItemId(), null, Constants.MESSAGE_TYPE_ASSIGN_TASK, task.getTaskId().toString(), Constants.AWARD_TASK_SUBMODULE_CODE);
			inboxDao.markReadMessage(Constants.AWARD_MODULE_CODE, task.getModuleItemId(), null, Constants.MESSAGE_TYPE_TASK_REJECT, task.getTaskId().toString(), Constants.AWARD_TASK_SUBMODULE_CODE);
			taskVO.setTaskCount(fetchTaskCountByModuleItemKey(task.getModuleItemKey()));
			commonDao.doflush();
			if (task.getTaskTypeCode().equals(Constants.TASK_DMP_TYPE_CODE)) {
				sendTaskNotification(taskVO, Constants.TASK_DMP_NOTIFICATION_CODE, dynamicEmailRecipients);
			} else if (task.getTaskTypeCode().equals(Constants.TASK_FINANCE_ACCOUNT_CREATION)) {
				sendTaskNotification(taskVO, Constants.TASK_FINANCE_ACCOUNT_CREATION_NOTIFICATION_CODE, dynamicEmailRecipients);
			} else if (task.getTaskTypeCode().equals(Constants.TASK_CONFIRM_PROJECT_DETAILS)) {
				taskVO.setTaskTypeCode(task.getTaskTypeCode());
				sendNotficationForFinanceAssignee(taskVO, Constants.TASK_CONFIRM_PROJECT_DETAIS_COMPLETES_NOTIFICTION_CODE);
			} else {
				sendTaskNotification(taskVO, Constants.TASK_REVIEW_COMPLETE_NOTIFICATION_CODE, dynamicEmailRecipients);
			}
		} else if (taskVO.getWorkflow() != null) {
			commonDao.doflush();
			List<WorkflowDetail> workflowDetails = taskVO.getWorkflow().getWorkflowDetails();
			Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				for (WorkflowDetail workflowDetail : workflowDetails) {
					if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_WAITING)) {
						commonService.setNotificationRecipients(workflowDetail.getApproverPersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
						taskVO.setApproverStopNumber(workflowDetail.getApprovalStopNumber());
						taskVO.setMapId(workflowDetail.getMapId());
					}
				}
			}
			sendTaskNotification(taskVO, Constants.TASK_SUBMITTED_NOTIFICATION_CODE, dynamicEmailRecipients);
		}
		businessRuleService.evaluateAndSentNotification(Constants.AWARD_MODULE_CODE, Constants.AWARD_TASK_SUBMODULE_CODE, task.getModuleItemId(), task.getTaskId().toString(), taskVO.getPersonId(), taskVO.getUserName(),getDynamicPlaceholders(taskVO));
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public void sendNotficationForFinanceAssignee(TaskVO taskVO, Integer notificationCode) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		List<Task> tasks = taskDao.fetchTasksByModuleCodeAndModuleItemId(taskVO.getModuleCode(), taskVO.getModuleItemId());
		for (Task task : tasks) {
			if (task.getTaskTypeCode().equals(Constants.TASK_FINANCE_ACCOUNT_CREATION)) {
				commonService.setNotificationRecipients(task.getAssigneePersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				sendTaskNotification(taskVO, notificationCode, dynamicEmailRecipients);
			}
		}
	}

	private TaskVO buildTaskWorkflow(TaskVO taskVO) {
		Integer workflowStatus = null;
		EvaluateValidationRuleVO evaluateValidationRuleVO = new EvaluateValidationRuleVO();
		evaluateValidationRuleVO.setModuleCode(taskVO.getModuleCode());
		evaluateValidationRuleVO.setSubModuleCode(taskVO.getSubModuleCode());
		evaluateValidationRuleVO.setModuleItemKey(taskVO.getModuleItemId().toString());
		evaluateValidationRuleVO.setSubModuleItemKey(taskVO.getTaskId().toString());
		evaluateValidationRuleVO.setLogginPersonId(taskVO.getPersonId());
		evaluateValidationRuleVO.setUpdateUser(taskVO.getUserName());
		workflowStatus = businessRuleService.buildWorkFlow(evaluateValidationRuleVO);
		if (workflowStatus == 1) {
			taskVO.setWorkflow(workflowDao.fetchActiveWorkflowByParams(taskVO.getModuleItemId().toString(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode()));
		}
		String isFinalApprover = businessRuleDao.workflowfinalApproval(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), evaluateValidationRuleVO.getSubModuleItemKey(), evaluateValidationRuleVO.getSubModuleCode());
		Integer canApproveRouting = businessRuleDao.canApproveRouting(evaluateValidationRuleVO.getModuleItemKey(), evaluateValidationRuleVO.getLogginPersonId(), evaluateValidationRuleVO.getModuleCode(), evaluateValidationRuleVO.getSubModuleItemKey(), evaluateValidationRuleVO.getSubModuleCode());
		taskVO.setCanApproveRouting(canApproveRouting.toString());
		taskVO.setIsFinalApprover(isFinalApprover);
		return taskVO;
	}

	private TaskVO fetchPreviousWorkFlowsList(TaskVO taskVO) {
		List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(taskVO.getModuleItemId().toString(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode());
		if (workFlows != null && !workFlows.isEmpty()) {
			workflowService.prepareWorkflowDetailsList(workFlows);
			Collections.sort(workFlows, new WorkflowComparator());
			taskVO.setWorkflowList(workFlows);
		}
		return taskVO;
	}

	@Override
	public void canTaskTakeRoutingAction(TaskVO taskVO) {
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		Workflow workflow = taskVO.getWorkflow();
		if (workflow == null) {
			workflow = workflowDao.fetchActiveWorkflowByParams(taskVO.getModuleItemId().toString(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode());
		}
		if (workflow != null) {
			Integer maxApprovalStopNumber = workflowDao.getMaxStopNumber(workflow.getWorkflowId());
			List<WorkflowDetail> finalWorkflowDetails = workflowDao.fetchFinalApprover(workflow.getWorkflowId(),
					maxApprovalStopNumber);
			if (finalWorkflowDetails != null && !finalWorkflowDetails.isEmpty()) {
				for (WorkflowDetail finalWorkflowDetail : finalWorkflowDetails) {
					if (finalWorkflowDetail.getApproverPersonId().equals(taskVO.getPersonId())
							|| finalWorkflowDetail.getApprovalStopNumber().equals(maxApprovalStopNumber)) {
						taskVO.setFinalApprover(true);
					}
				}
			}
			List<WorkflowDetail> workflowDetails = workflow.getWorkflowDetails();
			if (workflowDetails != null && !workflowDetails.isEmpty()) {
				Collections.sort(workflowDetails, new WorkflowDetailComparator());
				boolean currentPerson = true;
				if (task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS)) {
					for (WorkflowDetail workflowDetail : workflowDetails) {
						if (currentPerson == true) {
							if (workflowDetail.getApproverPersonId().equals(taskVO.getPersonId())) {
								if (task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS)) {
									if (workflowDetail.getApprovalStatusCode().equals(Constants.WORKFLOW_STATUS_CODE_APPROVED)) {
										taskVO.setIsApproved(true);
									} else {
										taskVO.setIsApproved(false);
									}
									taskVO.setIsApprover(true);
								}
							}
						}
					}
				}
			}
		}
	}

	@Override
	public String fetchTasksByParams(TaskVO taskVO) {
		Integer moduleCode = taskVO.getModuleCode();
		String moduleItemKey = taskVO.getModuleItemKey();
		Integer moduleItemId = taskVO.getModuleItemId();
		List<Task> allTasks = new ArrayList<>();
		if (taskVO.getTaskTabName().equalsIgnoreCase("TASKS")) {
			Set<Integer> awardIds = awardDao.fetchAwardIdsByAwardNumberAndNotInSequenceStatus(moduleItemKey, Constants.AWARD_FINAL_STATUS_CANCELLED);
 			for (Integer awardId : awardIds) {
				if (moduleItemId.equals(awardId)) {
					allTasks.addAll(taskDao.fetchTasksByModuleCodeAndModuleItemId(moduleCode, awardId));
				} else {
					List<String> taskStatusCodes = new ArrayList<>();
					taskStatusCodes.add(Constants.TASK_STATUS_CODE_OPEN);
					taskStatusCodes.add(Constants.TASK_STATUS_CODE_IN_PROGRESS);
					allTasks.addAll(taskDao.fetchTasksByParams(moduleCode, awardId, taskStatusCodes));
				}
			}
		} else if (taskVO.getTaskTabName().equalsIgnoreCase("ALL_TASKS")) {
			allTasks.addAll(taskDao.fetchTasksByModuleCodeAndModuleItemKey(moduleCode, moduleItemKey));
		}
		if (allTasks != null && !allTasks.isEmpty()) {
			for (Task task : allTasks) {
				task.setAssigneeFullName(personDao.getPersonFullNameByPersonId(task.getAssigneePersonId()));
				if (task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_IN_PROGRESS)) {
					List<ModuleVariableSection> sectionTypecodes = sectionWiseEditDao.getTaskEditableSections(task.getModuleItemId(), task.getTaskId().toString(), task.getModuleCode(), Constants.AWARD_TASK_SUBMODULE_CODE, Constants.TASK_EDITABLE_SECTION_VARIABLE_TYPE, taskVO.getPersonId());
					if (sectionTypecodes != null && !sectionTypecodes.isEmpty()) {
						task.setSectionTypeCodes(sectionTypecodes);
					}
				}
				Integer canApproveRouting = businessRuleDao.canApproveRouting(task.getModuleItemId(), taskVO.getPersonId(), task.getModuleCode(), task.getTaskId().toString(), taskVO.getSubModuleCode());
				task.setCanApproveRouting(canApproveRouting.toString());
			}
			taskVO.setTasks(allTasks);
		}
		taskVO.setTaskCount(fetchTaskCountByModuleItemKey(moduleItemKey));
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public String reassignTask(TaskVO taskVO) {
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		List<String> taskStatusCodes = new ArrayList<>();
		taskStatusCodes.add(Constants.TASK_STATUS_CODE_OPEN);
		taskStatusCodes.add(Constants.TASK_STATUS_CODE_IN_PROGRESS);
		taskStatusCodes.add(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS);
		taskStatusCodes.add(Constants.TASK_STATUS_CODE_RETURNED);
		Integer personCount = taskDao.checkCountOfAssignedTaskByParams(taskVO.getNewAssigneePersonId(), taskStatusCodes, task.getTaskTypeCode(), task.getModuleItemId());
		if (personCount > 0) {
			taskVO.setMessage("Task already exists");
			taskVO.setIsTaskExist(true);
		} else {
			String newAssigneePersonId = "";
			newAssigneePersonId = taskVO.getNewAssigneePersonId();
			taskVO.setOldAssigneePersonId(task.getAssigneePersonId());
			taskVO.setTask(task);
			taskReassign(taskVO);
			//addAwardPersonRoles(taskVO.getModuleItemId(), Constants.VIEW_AWARD_ROLE_ID, taskVO.getNewAssigneePersonId(), AuthenticatedUser.getLoginUserName());
			String userMessage = getUserMessageForTak(task);
			inboxService.addTaskMessageToInbox(task, AuthenticatedUser.getLoginUserName(), Constants.MESSAGE_TYPE_ASSIGN_TASK, "T", taskVO.getSubModuleCode(), userMessage);
			loadTaskUserFullNames(task);
			updateEditableFields(task.getModuleItemId(), task.getTaskId(), task.getAssigneePersonId(), newAssigneePersonId);
			taskVO.setSectionTypeCodes(sectionWiseEditDao.getEditableSections(task.getModuleItemId(), Constants.ZERO, Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.AWARD_SUBMODULE_CODE));
			List<Integer> taskIds = taskDao.getPersonTaskIds(task.getModuleItemId(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.TASK_STATUS_CODE_IN_PROGRESS);
			if (taskIds != null && !taskIds.isEmpty()) {
				for (Integer taskId : taskIds) {
					List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(task.getModuleItemId(), taskId.toString(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.AWARD_TASK_SUBMODULE_CODE);
					if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
						taskVO.getSectionTypeCodes().addAll(moduleVariableSections);
					}
				}
			}
			commonDao.doflush();
			sendNotificationsForTask(taskVO);
		}
		return commonDao.convertObjectToJSON(taskVO);
	}

	public TaskVO taskReassign(TaskVO taskVO) {
		Task task = taskVO.getTask();
		String oldAssigneePersonId = taskVO.getOldAssigneePersonId();
		String newAssigneePersonId = taskVO.getNewAssigneePersonId();
		inboxDao.markReadMessage(Constants.AWARD_MODULE_CODE, task.getModuleItemId(), oldAssigneePersonId, Constants.MESSAGE_TYPE_ASSIGN_TASK, task.getTaskId().toString(), Constants.AWARD_TASK_SUBMODULE_CODE);
		task.setTaskStatusCode(Constants.TASK_STATUS_CODE_OPEN);
		task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_OPEN));
		task.setAssigneePersonId(newAssigneePersonId);
		task.setUpdateUser(AuthenticatedUser.getLoginUserName());
		task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		task = taskDao.saveOrUpdateTask(task);
		taskVO.setTask(task);
		taskVO.setTaskId(task.getTaskId());
		taskVO.setOldAssigneePersonId(oldAssigneePersonId);
		String systemComment = task.getTaskType().getDescription() + " Task";
		TaskActionLog taskActionLog = saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_REASSIGNED, systemComment, AuthenticatedUser.getLoginUserName());
		taskActionLog = taskDao.fetchTaskActionLogBasedOnTaskId(taskVO.getTaskId());
		saveTaskAssigneeHistory(task.getTaskId(), taskActionLog.getActionLogId(), oldAssigneePersonId, newAssigneePersonId, AuthenticatedUser.getLoginUserName());
		return taskVO;
	}

	public void sendNotificationsForTask(TaskVO taskVO) {
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		String personId = personDao.getPersonIdByUserName(task.getCreateUser());
		String oldAssigneePersonId = "";
		String newAssigneePersonId = "";
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
//		if (!task.getTaskTypeCode().equals(Constants.TASK_FINANCE_ACCOUNT_CREATION)) {
			if (taskVO.getIsGmUpdate() != null && taskVO.getIsGmUpdate().equalsIgnoreCase("N")) {
				String assigneePersonId = task.getAssigneePersonId();
				commonService.setNotificationRecipients(assigneePersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				sendTaskNotification(taskVO, Constants.TASK_CREATED_NOTIFICATION_CODE, dynamicEmailRecipients);
			} else {
				if (taskVO.getIsGmUpdate() != null && taskVO.getIsGmUpdate().equalsIgnoreCase("Y")) {
					oldAssigneePersonId = taskVO.getOldAssigneePersonId();
					newAssigneePersonId = task.getAssigneePersonId();
					Set<NotificationRecipient> dynamicEmailRecipient = new HashSet<>();
					commonService.setNotificationRecipients(oldAssigneePersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipient);
					sendTaskNotification(taskVO, Constants.TASK_ASSIGNEE_REMOVED_NOTIFICATION_CODE, dynamicEmailRecipient);
				} else {
					newAssigneePersonId = taskVO.getNewAssigneePersonId();
					commonService.setNotificationRecipients(personId, Constants.NOTIFICATION_RECIPIENT_TYPE_CC, dynamicEmailRecipients);
				}
				commonService.setNotificationRecipients(newAssigneePersonId, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				sendTaskNotification(taskVO, Constants.TASK_REASSIGN_NOTIFICATION_CODE, dynamicEmailRecipients);
			}
//		}
	}

	@Override
	public String getAdvancedSearchTasks(TaskVO taskVO) {
		List<Task> tasks = new ArrayList<>();
		try {
			tasks = taskDao.fetchAdvancedSearchTasks(taskVO);
		} catch (Exception e) {
			logger.error("Exception in getAdvancedSearchTasks :{}", e.getMessage());
		}
		if (tasks != null && !tasks.isEmpty()) {
			for (Task task : tasks) {
				task.setAssigneeFullName(personDao.getPersonFullNameByPersonId(task.getAssigneePersonId()));
			}
		}
		taskVO.setTasks(tasks);
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public String fetchTaskHistory(TaskVO taskVO) {
		try {
			if (taskVO.getTaskId() != null) {
				List<TaskActionLog> taskActionHistoryDetails = taskDao.fetchTaskActionHistoryByTaskId(taskVO.getTaskId());
				taskActionHistoryDetails.forEach(taskActionLog -> {
					String newAssigneePersonId = taskDao.fetchTaskAssigneeHistoryByActionLogId(taskActionLog.getActionLogId());
					if (newAssigneePersonId != null) {
						taskActionLog.setNewAssigneeFullName(personDao.getPersonFullNameByPersonId(newAssigneePersonId));
					}
					taskActionLog.setTaskTypeDescription(taskActionLog.getSystemComment());
				});
				getFullNameOfUpdateUser(taskActionHistoryDetails);
				taskVO.setTaskActionLogs(taskActionHistoryDetails);
			}
		} catch (Exception e) {
			logger.error("Exception in fetchTaskHistory {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(taskVO);
	}

	private void getFullNameOfUpdateUser(List<TaskActionLog> taskActionHistoryDetails) {
		Set<String> userName = taskActionHistoryDetails.stream().map(TaskActionLog::getUpdateUser).collect(Collectors.toSet());
		if(!userName.isEmpty()) {
			List<Person> personDetails = commonDao.getPersonDetailByUserName(new ArrayList<>(userName));
			Map<String, String> collect = personDetails.stream().collect(Collectors.toMap(person -> person.getPrincipalName().toUpperCase(), person -> person.getFullName()));
			taskActionHistoryDetails.stream().filter(item -> item.getUpdateUser() != null).filter(item -> collect.containsKey(item.getUpdateUser().toUpperCase())).forEach(item -> item.setUpdateUserFullName(collect.get(item.getUpdateUser().toUpperCase())));
		}
	}

	@Override
	public TaskActionLog saveTaskActionLogDetails(Integer taskId, String taskActionTypeCode, String systemComment, String updateUser) {
		TaskActionLog taskActionLog = new TaskActionLog();
		taskActionLog.setTaskId(taskId);
		taskActionLog.setActionTypeCode(taskActionTypeCode);
		taskActionLog.setSystemComment(systemComment);
		taskActionLog.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		taskActionLog.setUpdateUser(updateUser);
		return taskDao.saveOrUpdateTaskActionLog(taskActionLog);
	}

	@Override
	public void saveTaskStatusHistory(Integer taskId, String taskStatusCode, Integer actionLogId, String updateUser) {
		TaskStatusHistory taskStatusHistory = new TaskStatusHistory();
		taskStatusHistory.setTaskId(taskId);
		taskStatusHistory.setTaskStatusCode(taskStatusCode);
		taskStatusHistory.setActionLogId(actionLogId);
		taskStatusHistory.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		taskStatusHistory.setUpdateUser(updateUser);
		taskDao.saveOrUpdateTaskStatusHistory(taskStatusHistory);
	}

	@Override
	public String cancelTask(TaskVO taskVO) {
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		taskVO.setStatus(true);
		task.setTaskStatusCode(Constants.TASK_STATUS_CODE_CANCELLED);
		task.setTaskStatus(taskDao.fetchTaskStatusByTaskStatusCode(Constants.TASK_STATUS_CODE_CANCELLED));
		task.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		task.setUpdateUser(AuthenticatedUser.getLoginUserName());
		taskDao.saveOrUpdateTask(task);
		inboxDao.removeTaskMessageFromInbox(Integer.parseInt(task.getModuleItemId()), task.getTaskId(), task.getModuleCode(), Constants.AWARD_TASK_SUBMODULE_CODE);
		TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
		String systemComment = taskType.getDescription() + " Task";
		TaskActionLog taskActionLog = saveTaskActionLogDetails(task.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_CANCELLED, systemComment, AuthenticatedUser.getLoginUserName());
		saveTaskStatusHistory(task.getTaskId(), Constants.TASK_STATUS_CODE_CANCELLED, taskActionLog.getActionLogId(), AuthenticatedUser.getLoginUserName());
		taskVO.setTask(task);
		loadTaskUserFullNames(task);
		List<Integer> taskIds = taskDao.getPersonTaskIds(task.getModuleItemId(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.TASK_STATUS_CODE_IN_PROGRESS);
		if (taskIds != null && !taskIds.isEmpty()) {
			for (Integer taskId : taskIds) {
				List<ModuleVariableSection> moduleVariableSections = sectionWiseEditDao.getEditableSections(task.getModuleItemId(), taskId.toString(), Constants.AWARD_MODULE_CODE, taskVO.getPersonId(), Constants.AWARD_TASK_SUBMODULE_CODE);
				if (moduleVariableSections != null && !moduleVariableSections.isEmpty()) {
					taskVO.getSectionTypeCodes().addAll(moduleVariableSections);
				}
			}
		}
		taskVO.setTaskCount(fetchTaskCountByModuleItemKey(task.getModuleItemKey()));
		commonDao.doflush();
		sendTaskNotification(taskVO, Constants.TASK_REJECTED_NOTIFICATION_CODE, new HashSet<>());
		return commonDao.convertObjectToJSON(taskVO);
	}

	/*private void addAwardPersonRoles(Integer awardId, Integer roleId, String personId, String updateUser) {
		List<Integer> roleIds = new ArrayList<>();
		roleIds.add(roleId);
		List<AwardPersonRoles> awardPersonRoles = awardDao.fetchAwardPersonRolesByParams(personId, awardId, roleIds);
		if (awardPersonRoles == null || awardPersonRoles.isEmpty()) {
			Award award = awardDao.fetchAwardByAwardId(awardId.toString());
			AwardPersonRoles awardPersonRole = new AwardPersonRoles();
			awardPersonRole.setAwardId(awardId);
			awardPersonRole.setAwardNumber(award.getAwardNumber());
			awardPersonRole.setSequenceNumber(award.getSequenceNumber());
			awardPersonRole.setPersonId(personId);
			awardPersonRole.setRoleId(roleId);
			awardPersonRole.setUpdateUser(updateUser);
			awardPersonRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			awardDao.saveOrUpdateAwardPersonRoles(awardPersonRole);
		}
	}*/

	@Override
	public void loadTaskUserFullNames(Task task) {
		if (task.getCreateUser() != null) {
			task.setCreateUserFullName(personDao.getUserFullNameByUserName(task.getCreateUser()));
		}
		if (task.getUpdateUser() != null) {
			task.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(task.getUpdateUser()));
		}
		if (task.getUpdateUser() != null) {
			task.setSubmitUserFullName(personDao.getUserFullNameByUserName(task.getUpdateUser()));
		}
		if (task.getAssigneePersonId() != null) {
			task.setAssigneeFullName(personDao.getPersonFullNameByPersonId(task.getAssigneePersonId()));
		}
	}

	@Override
	public String loadTaskWorkflowDetailsById(TaskVO taskVO) {
		Task task = taskDao.fetchTaskByTaskId(taskVO.getTaskId());
		loadTaskUserFullNames(task);
		if (task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_COMPLETED)
				|| task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_RETURNED)
				|| task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_REVIEW_IN_PROGRESS)
				|| (commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING)
						&& (!task.getTaskStatusCode().equals(Constants.TASK_STATUS_CODE_IN_PROGRESS)))) {
			canTaskTakeRoutingAction(taskVO);
			Workflow workflow = workflowDao.fetchActiveWorkflowByParams(taskVO.getModuleItemId().toString(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode());
			if (workflow != null) {
				workflowService.prepareWorkflowDetails(workflow);
				taskVO.setWorkflow(workflow);
				List<Workflow> workFlows = workflowDao.fetchWorkflowsByParams(taskVO.getModuleItemId().toString(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode());
				if (workFlows != null && !workFlows.isEmpty()) {
					workflowService.prepareWorkflowDetailsList(workFlows);
					Collections.sort(workFlows, new WorkflowComparator());
					taskVO.setWorkflowList(workFlows);
				}
			}
		}
		Integer canApproveRouting = businessRuleDao.canApproveRouting(taskVO.getModuleItemId().toString(),taskVO.getPersonId(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode());
		taskVO.setCanApproveRouting(canApproveRouting.toString());
		taskVO.setIsFinalApprover(businessRuleDao.workflowfinalApproval(taskVO.getModuleItemId().toString(), taskVO.getPersonId(), taskVO.getModuleCode(), taskVO.getTaskId().toString(), taskVO.getSubModuleCode()));
		taskVO.setIsSubmit("1");
		taskVO.setTask(task);
		taskVO.setTaskStatusCode(taskDao.fetchTaskByTaskId(task.getTaskId()).getTaskStatusCode());
		return commonDao.convertObjectToJSON(taskVO);
	}

	private void saveTaskAssigneeHistory(Integer taskId, Integer actionLogId, String oldAssigneePersonId, String newAssigneePersonId, String updateUser) {
		TaskAssigneeHistory taskAssigneeHistory = new TaskAssigneeHistory();
		taskAssigneeHistory.setTaskId(taskId);
		taskAssigneeHistory.setActionLogId(actionLogId);
		taskAssigneeHistory.setOldAssigneePersonId(oldAssigneePersonId);
		taskAssigneeHistory.setNewAssigneePersonId(newAssigneePersonId);
		taskAssigneeHistory.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		taskAssigneeHistory.setUpdateUser(updateUser);
		taskDao.saveTaskAssigneeHistory(taskAssigneeHistory);
	}

	private void addTaskEditableFields(TaskVO taskVO) {
		List<String> sectionTypeCodes = taskDao.getSectionCodeBasedOnTaskTypeCode(taskVO.getTaskTypeCode());
		for (String sectionCode : sectionTypeCodes) {
			if (!commonDao.getParameterValueAsBoolean(Constants.ENABLE_SPECIAL_REVIEW) && !sectionCode.equals(Constants.SPECIAL_REVIEW_SECTION_TYPE_CODE)
					|| commonDao.getParameterValueAsBoolean(Constants.ENABLE_SPECIAL_REVIEW)) {
				ModuleVariableSection moduleVariableSection = new ModuleVariableSection();
				moduleVariableSection.setModuleCode(taskVO.getModuleCode());
				moduleVariableSection.setSubModuleCode(taskVO.getSubModuleCode());
				moduleVariableSection.setModuleItemKey(taskVO.getModuleItemId().toString());
				moduleVariableSection.setSectionCode(sectionCode);
				moduleVariableSection.setSectionType(sectionWiseEditDao.getSectionTypebySectionTypeId(sectionCode));
				moduleVariableSection.setTypeCode(Integer.parseInt(taskVO.getTaskTypeCode()));
				moduleVariableSection.setUpdateTimestamp(commonDao.getCurrentTimestamp());
				moduleVariableSection.setUpdateUser(AuthenticatedUser.getLoginUserName());
				moduleVariableSection.setPersonId(taskVO.getPersonId());
				moduleVariableSection.setSubModuleItemKey(taskVO.getSubModuleItemKey());
				moduleVariableSection.setVariableType(Constants.TASK_EDITABLE_SECTION_VARIABLE_TYPE);
				sectionWiseEditDao.saveorUpdateModuleVariableSection(moduleVariableSection);
			}
		}
	}

	@Override
	public String taskInvitation(TaskVO taskVO) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		String assigneePersonId = taskVO.getAssigneePersonId();
		if (assigneePersonId != null) {
			commonService.setNotificationRecipients(assigneePersonId, "TO", dynamicEmailRecipients);
		}
		TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(taskVO.getTaskTypeCode());
		String systemComment = taskType.getDescription() + " Task";
		TaskActionLog taskActionLog = saveTaskActionLogDetails(taskVO.getTaskId(), Constants.TASK_ACTION_TYPE_CODE_NOTIFIED, systemComment, AuthenticatedUser.getLoginUserName());
		saveTaskStatusHistory(taskVO.getTaskId(), taskVO.getTaskStatusCode(), taskActionLog.getActionLogId(), AuthenticatedUser.getLoginUserName());
		commonDao.doflush();
		sendTaskNotification(taskVO, Constants.TASK_ASSIGNEE_REMAINDER_NOTIFICATION_CODE, dynamicEmailRecipients);
		return commonDao.convertObjectToJSON(taskVO);
	}

	@Override
	public Integer fetchTaskCountByModuleItemKey(String moduleItemKey) {
		List<String> statusCodes = new ArrayList<>();
		statusCodes.add(Constants.TASK_STATUS_CODE_OPEN);
		statusCodes.add(Constants.TASK_STATUS_CODE_IN_PROGRESS);
		statusCodes.add(Constants.TASK_STATUS_CODE_RETURNED);
		return taskDao.fetchTaskCountBasedOnModuleItemKeyAndTaskStatus(moduleItemKey, statusCodes);
	}

	private String getUserMessageForTak(Task task) {
		Award award = awardDao.getAwardDetailsById(Integer.parseInt(task.getModuleItemId()));
		return task.getTaskType().getDescription() + " for #" + award.getAwardNumber() + " : " + award.getTitle();
	}

	@Override
	public String saveOrUpdateTaskForWAF(TaskVO taskVO) {
		String response = null;
		MultipartFile multipartFile = null;
		MultipartFile[] files = new MultipartFile[1]; 
		String name = taskVO.getFileName();
		Integer remaining = taskVO.getRemaining();
		Integer length = taskVO.getLength();
		String userId = taskVO.getPersonId();
		String contentType = taskVO.getContentType();
		String splicedFile = taskVO.getFileContent();
		String timestamp = taskVO.getFileTimestamp();
		if (splicedFile != null) {
			multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
		}
		if (multipartFile != null && !multipartFile.isEmpty() || splicedFile == null) {
			if (splicedFile == null) {
				files = null;
			} else {
				files[0] = multipartFile;
			}
			response = saveOrUpdateTask(files, null, taskVO);
		}
		return response;
	}

	@Override
	public String saveOrUpdateTaskCommentForWAF(TaskVO taskVO) {
		String response = null;
		MultipartFile multipartFile = null;
		MultipartFile[] files = new MultipartFile[1]; 
		String name = taskVO.getFileName();
		Integer remaining = taskVO.getRemaining();
		Integer length = taskVO.getLength();
		String userId = taskVO.getPersonId();
		String contentType = taskVO.getContentType();
		String splicedFile = taskVO.getFileContent();
		String timestamp = taskVO.getFileTimestamp();
		if (splicedFile != null) {
			multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, contentType);
		}
		if (multipartFile != null && !multipartFile.isEmpty() || splicedFile == null) {
			if (splicedFile == null) {
				files = null;
			} else {
				files[0] = multipartFile;
			}
			response = saveOrUpdateTaskComment(files, null, taskVO);
		}
		return response;
	}

}
