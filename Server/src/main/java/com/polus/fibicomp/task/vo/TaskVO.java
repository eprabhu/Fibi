package com.polus.fibicomp.task.vo;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.pojo.TaskActionLog;
import com.polus.fibicomp.task.pojo.TaskActionType;
import com.polus.fibicomp.task.pojo.TaskAttachment;
import com.polus.fibicomp.task.pojo.TaskComment;
import com.polus.fibicomp.task.pojo.TaskCommentAttachment;
import com.polus.fibicomp.task.pojo.TaskStatus;
import com.polus.fibicomp.task.pojo.TaskType;
import com.polus.fibicomp.workflow.pojo.Workflow;

public class TaskVO {

	private Integer taskId;

	private String message;

	private String updateUser;

	private Task task;

	private List<TaskType> taskTypes;

	private List<TaskStatus> taskStatusList;

	private Integer moduleCode;

	private Integer subModuleCode;

	private String moduleItemKey;

	private List<Task> tasks;

	private List<TaskActionType> taskActionTypes;

	private List<TaskAttachment> taskAttachments;

	private TaskAttachment taskAttachment;

	private Integer taskAttachmentId;

	private List<TaskCommentAttachment> taskCommentAttachments;

	private TaskCommentAttachment taskCommentAttachment;

	private Integer taskCommentAttachmentId;

	private List<TaskComment> taskComments;

	private TaskComment taskComment;

	private boolean status = false;

	private String personId;

	private String userName;

	private Workflow workflow;

	private String isFinalApprover;

	private String canApproveRouting;

	private List<Workflow> workflowList;

	private String workFlowPersonId;

	private String actionType;

	private String approveComment;

	private List<ModuleVariableSection> sectionTypeCodes;

	private Boolean finalApprover = false;

	private Boolean isApproved = false;

	private Boolean isApprover = false;

	private String newAssigneePersonId;

	private String assigneePersonId;

	private Integer taskCommentId;

	private String oldAssigneePersonId;

	private Integer moduleItemId;

	private String isSubmit;

	private Integer awardSequenceNumber;

	private List<TaskActionLog> taskActionLogs;

	private String isGmUpdate;

	private Integer activeAwardId;

	private Integer pendingAwardId;

	private Integer approverStopNumber;

	private String acType;

	private Timestamp dueDateFrom;

	private Timestamp dueDateTo;

	private String taskStatusCode;

	private List<String> taskStatus;

	private String taskTabName;

	private Integer endModuleSubItemKey;

	private String taskTypeCode;

	private Integer workFlowDetailId;

	private String subModuleItemKey;

	private String leadUnitNumber;

	private Boolean isTaskExist = false;

	private Integer taskCount;

	private Integer mapId;

	private Integer mapNumber;

	private Integer approverNumber;

	private List<String> availableRights;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String contentType;

	private String fileName;

	private String fileTimestamp;

	public TaskVO() {
		sectionTypeCodes = new ArrayList<>();
	}

	public Integer getTaskId() {
		return taskId;
	}

	public void setTaskId(Integer taskId) {
		this.taskId = taskId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Task getTask() {
		return task;
	}

	public void setTask(Task task) {
		this.task = task;
	}

	public List<TaskType> getTaskTypes() {
		return taskTypes;
	}

	public void setTaskTypes(List<TaskType> taskTypes) {
		this.taskTypes = taskTypes;
	}

	public List<TaskStatus> getTaskStatusList() {
		return taskStatusList;
	}

	public void setTaskStatusList(List<TaskStatus> taskStatusList) {
		this.taskStatusList = taskStatusList;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public List<Task> getTasks() {
		return tasks;
	}

	public void setTasks(List<Task> tasks) {
		this.tasks = tasks;
	}

	public List<TaskActionType> getTaskActionTypes() {
		return taskActionTypes;
	}

	public void setTaskActionTypes(List<TaskActionType> taskActionTypes) {
		this.taskActionTypes = taskActionTypes;
	}

	public List<TaskAttachment> getTaskAttachments() {
		return taskAttachments;
	}

	public void setTaskAttachments(List<TaskAttachment> taskAttachments) {
		this.taskAttachments = taskAttachments;
	}

	public TaskAttachment getTaskAttachment() {
		return taskAttachment;
	}

	public void setTaskAttachment(TaskAttachment taskAttachment) {
		this.taskAttachment = taskAttachment;
	}

	public Integer getTaskAttachmentId() {
		return taskAttachmentId;
	}

	public void setTaskAttachmentId(Integer taskAttachmentId) {
		this.taskAttachmentId = taskAttachmentId;
	}

	public boolean isStatus() {
		return status;
	}

	public void setStatus(boolean status) {
		this.status = status;
	}

	public List<TaskCommentAttachment> getTaskCommentAttachments() {
		return taskCommentAttachments;
	}

	public void setTaskCommentAttachments(List<TaskCommentAttachment> taskCommentAttachments) {
		this.taskCommentAttachments = taskCommentAttachments;
	}

	public TaskCommentAttachment getTaskCommentAttachment() {
		return taskCommentAttachment;
	}

	public void setTaskCommentAttachment(TaskCommentAttachment taskCommentAttachment) {
		this.taskCommentAttachment = taskCommentAttachment;
	}

	public Integer getTaskCommentAttachmentId() {
		return taskCommentAttachmentId;
	}

	public void setTaskCommentAttachmentId(Integer taskCommentAttachmentId) {
		this.taskCommentAttachmentId = taskCommentAttachmentId;
	}

	public List<TaskComment> getTaskComments() {
		return taskComments;
	}

	public void setTaskComments(List<TaskComment> taskComments) {
		this.taskComments = taskComments;
	}

	public TaskComment getTaskComment() {
		return taskComment;
	}

	public void setTaskComment(TaskComment taskComment) {
		this.taskComment = taskComment;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public String getIsFinalApprover() {
		return isFinalApprover;
	}

	public void setIsFinalApprover(String isFinalApprover) {
		this.isFinalApprover = isFinalApprover;
	}

	public String getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(String canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public String getWorkFlowPersonId() {
		return workFlowPersonId;
	}

	public void setWorkFlowPersonId(String workFlowPersonId) {
		this.workFlowPersonId = workFlowPersonId;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public String getApproveComment() {
		return approveComment;
	}

	public void setApproveComment(String approveComment) {
		this.approveComment = approveComment;
	}

	public Boolean getFinalApprover() {
		return finalApprover;
	}

	public void setFinalApprover(Boolean finalApprover) {
		this.finalApprover = finalApprover;
	}

	public Boolean getIsApproved() {
		return isApproved;
	}

	public void setIsApproved(Boolean isApproved) {
		this.isApproved = isApproved;
	}

	public Boolean getIsApprover() {
		return isApprover;
	}

	public void setIsApprover(Boolean isApprover) {
		this.isApprover = isApprover;
	}

	public String getNewAssigneePersonId() {
		return newAssigneePersonId;
	}

	public void setNewAssigneePersonId(String newAssigneePersonId) {
		this.newAssigneePersonId = newAssigneePersonId;
	}

	public String getAssigneePersonId() {
		return assigneePersonId;
	}

	public void setAssigneePersonId(String assigneePersonId) {
		this.assigneePersonId = assigneePersonId;
	}

	public Integer getTaskCommentId() {
		return taskCommentId;
	}

	public void setTaskCommentId(Integer taskCommentId) {
		this.taskCommentId = taskCommentId;
	}

	public String getOldAssigneePersonId() {
		return oldAssigneePersonId;
	}

	public void setOldAssigneePersonId(String oldAssigneePersonId) {
		this.oldAssigneePersonId = oldAssigneePersonId;
	}

	public Integer getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(Integer moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public String getIsSubmit() {
		return isSubmit;
	}

	public void setIsSubmit(String isSubmit) {
		this.isSubmit = isSubmit;
	}

	public Integer getAwardSequenceNumber() {
		return awardSequenceNumber;
	}

	public void setAwardSequenceNumber(Integer awardSequenceNumber) {
		this.awardSequenceNumber = awardSequenceNumber;
	}

	public List<TaskActionLog> getTaskActionLogs() {
		return taskActionLogs;
	}

	public void setTaskActionLogs(List<TaskActionLog> taskActionLogs) {
		this.taskActionLogs = taskActionLogs;
	}

	public String getIsGmUpdate() {
		return isGmUpdate;
	}

	public void setIsGmUpdate(String isGmUpdate) {
		this.isGmUpdate = isGmUpdate;
	}

	public Integer getActiveAwardId() {
		return activeAwardId;
	}

	public void setActiveAwardId(Integer activeAwardId) {
		this.activeAwardId = activeAwardId;
	}

	public Integer getPendingAwardId() {
		return pendingAwardId;
	}

	public void setPendingAwardId(Integer pendingAwardId) {
		this.pendingAwardId = pendingAwardId;
	}

	public Integer getApproverStopNumber() {
		return approverStopNumber;
	}

	public void setApproverStopNumber(Integer approverStopNumber) {
		this.approverStopNumber = approverStopNumber;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Timestamp getDueDateFrom() {
		return dueDateFrom;
	}

	public void setDueDateFrom(Timestamp dueDateFrom) {
		this.dueDateFrom = dueDateFrom;
	}

	public Timestamp getDueDateTo() {
		return dueDateTo;
	}

	public void setDueDateTo(Timestamp dueDateTo) {
		this.dueDateTo = dueDateTo;
	}

	public String getTaskStatusCode() {
		return taskStatusCode;
	}

	public void setTaskStatusCode(String taskStatusCode) {
		this.taskStatusCode = taskStatusCode;
	}

	public List<String> getTaskStatus() {
		return taskStatus;
	}

	public void setTaskStatus(List<String> taskStatus) {
		this.taskStatus = taskStatus;
	}

	public String getTaskTabName() {
		return taskTabName;
	}

	public void setTaskTabName(String taskTabName) {
		this.taskTabName = taskTabName;
	}

	public Integer getEndModuleSubItemKey() {
		return endModuleSubItemKey;
	}

	public void setEndModuleSubItemKey(Integer endModuleSubItemKey) {
		this.endModuleSubItemKey = endModuleSubItemKey;
	}

	public String getTaskTypeCode() {
		return taskTypeCode;
	}

	public void setTaskTypeCode(String taskTypeCode) {
		this.taskTypeCode = taskTypeCode;
	}

	public Integer getWorkFlowDetailId() {
		return workFlowDetailId;
	}

	public void setWorkFlowDetailId(Integer workFlowDetailId) {
		this.workFlowDetailId = workFlowDetailId;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public List<ModuleVariableSection> getSectionTypeCodes() {
		return sectionTypeCodes;
	}

	public void setSectionTypeCodes(List<ModuleVariableSection> sectionTypeCodes) {
		this.sectionTypeCodes = sectionTypeCodes;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public Boolean getIsTaskExist() {
		return isTaskExist;
	}

	public void setIsTaskExist(Boolean isTaskExist) {
		this.isTaskExist = isTaskExist;
	}

	public Integer getTaskCount() {
		return taskCount;
	}

	public void setTaskCount(Integer taskCount) {
		this.taskCount = taskCount;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public Integer getMapNumber() {
		return mapNumber;
	}

	public void setMapNumber(Integer mapNumber) {
		this.mapNumber = mapNumber;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

}
