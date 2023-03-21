package com.polus.fibicomp.task.dao;

import java.util.List;

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
@Service
public interface TaskDao {

	/**
	 * This method is used to fetch all task types.
	 * @return list of task type
	 */
	public List<TaskType> fetchAllTaskType();

	/**
	 * This method is used to fetch all task status.
	 * @return list of task status
	 */
	public List<TaskStatus> fetchAllTaskStatus();

	/**
	 * This method is used to fetch FileData object.
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public TaskFileData getFileDataById(String fileDataId);

	/**
	 * This method is used to save TaskFileData object.
	 * @param fileData - TaskFileData object.
	 * @return TaskFileData.
	 */
	public TaskFileData saveFileData(TaskFileData fileData);

	/**
	 * This method is used to save or update a task
	 * @param proposal - Object of a task.
	 * @return An object of task.
	 */
	public Task saveOrUpdateTask(Task task);

	/**
	 * This method is used to save or update a taskActionLog
	 * @param proposal - Object of a taskActionLog.
	 * @return An object of taskActionLog.
	 */
	public TaskActionLog saveOrUpdateTaskActionLog(TaskActionLog taskActionLog);

	/**
	 * This method is used to save or update a task status history
	 * @param proposal - Object of a taskStatusHistory.
	 * @return An object of taskStatusHistory.
	 */
	public TaskStatusHistory saveOrUpdateTaskStatusHistory(TaskStatusHistory taskStatusHistory);

	/**
	 * This method used to fetch task type by taskTypeCode
	 * @param taskTypeCode
	 * @return details of task type
	 */
	public TaskType fetchTaskTypeByTaskTypeCode(String taskTypeCode);

	/**
	 * This method used to fetch fetchTaskBy by TaskId
	 * @param TaskId
	 * @return details of task
	 */
	public Task fetchTaskByTaskId(Integer taskId);

	/**
	 * This method is used to fetch attachment by Id.
	 * @param attachmentId - Id of the attachment.
	 * @return an object of TaskAttachment.
	 */
	public TaskAttachment fetchTaskAttachmentById(Integer attachmentId);

	/**
	 * This method is used to delete TaskFileData object.
	 * @param fileData - TaskFileData object.
	 */
	public void deleteTaskFileData(TaskFileData fileData);

	/**
	 * This method is used to fetch Task Comments Based On Task Id.
	 * @param taskId - taskId of the task.
	 * @return An object of TaskComment.
	 */
	public List<TaskComment> fetchTaskCommentBasedOnTaskId(Integer taskId);

	/**
	 * This method is used to fetch Task AttachmentBased On Task Id.
	 * @param taskId - taskId of the task.
	 * @return An object of TaskAttachment.
	 */
	public List<TaskAttachment> fetchTaskAttachmentBasedOnTaskId(Integer taskId);

	/**
	 * This method is used to delete attachment from task.
	 * @param vo - Object of taskAttachment class. 
	 */
	public void deleteTaskAttachment(Integer taskAttachmentId);

	/**
	 * This method is used to save or update a taskComment
	 * @param taskComment - Object of a taskComment.
	 * @return An object of task comment.
	 */
	public TaskComment saveOrUpdateTaskComment(TaskComment taskComment);

	/**
	 * This method is used to delete Task Comment based on task comment id.
	 * @param taskCommentId - Id of the TaskComment.
	 */
	public void deleteTaskComment(Integer taskCommentId);

	/**
	 * This method is used to fetch task comment attachment by Id.
	 * @param attachmentId - Id of the attachment.
	 * @return an object of TaskCommentAttachment.
	 */
	public TaskCommentAttachment fetchTaskCommentAttachmentById(Integer attachmentId);

	/**
	 * This method is used to delete taskCommentAttachment from task.
	 * @param taskCommentAttachmentId - taskCommentAttachmentId of taskCommentAttachment class. 
	 */
	public void deleteTaskCommentAttachment(Integer taskCommentAttachmentId);

	/**
	 * This method used to fetch task status by taskStatusCode
	 * @param taskStatusCode
	 * @return details of task status
	 */
	public TaskStatus fetchTaskStatusByTaskStatusCode(String taskStatusCode);

	/**
	 * This method is used to fetch Task ActionLogBased On Task Id.
	 * @param taskId - taskId of the task.
	 * @return An object of TaskActionLog.
	 */
	public TaskActionLog fetchTaskActionLogBasedOnTaskId(Integer taskId);

	/**
	* This method is used to fetch the task count based on moduleItemKey and status
	* @param moduleItemKey - moduleItemKey of the task
	* @param statusCodes - List of statuscodes
	* @return Integer - count
	*/
	public Integer fetchTaskCountBasedOnModuleItemKeyAndTaskStatus(String moduleItemKey, List<String> statusCodes);

	/**
	* This method is used to save TaskActionLog of a Task
	* @param taskActionLog - object of TaskActionLog
	* @return TaskActionLog - object of TaskActionLog
	*/
	public TaskActionLog saveTaskActionLog(TaskActionLog taskActionLog);

	/**
	* This method is used to save TaskAssigneeHistory of a Task
	* @param taskAssigneeHistory - object of TaskAssigneeHistory
	* @return taskAssigneeHistory - object of TaskAssigneeHistory
	*/
	public TaskAssigneeHistory saveTaskAssigneeHistory(TaskAssigneeHistory taskAssigneeHistory);

	/**
	* This method is used to fetch advanced search result of tasks.
	* @param vo - object of TaskVO
	* @return Tasks
	*/
	public List<Task> fetchAdvancedSearchTasks(TaskVO vo);

	/**
	* This method is used to fetch details of task history by id
	* @param taskId
	* @return List of task history details.
	*/
	public List<TaskActionLog> fetchTaskActionHistoryByTaskId(Integer taskId);

	/**
	 * This method is used to fetch Task Comments Based On Task Id.
	 * @param taskId - taskId of the task.
	 * @return An object of TaskComment.
	 */
	public Integer fetchAwardIdByAwardNumberAndSequenceNumber(String awardNumber, String sequenceNumber);

	/**
	 * This method is used to fetch Task Assignee Based On Task Id.
	 * @param taskId - taskId of the task.
	 * @return assignee.
	 */
	public String fetchTaskAssigneeByTaskId(Integer taskId);

	/**
	 * This method is used to fetch the tasks by moduleCode and moduleItemId
	 * @param moduleCode - moduleCode of the module
	 * @param moduleItemId - moduleItemId of the module
	 * @return List<Task> - list of Task
	 */
	public List<Task> fetchTasksByModuleCodeAndModuleItemId(Integer moduleCode, Integer moduleItemId);

	/**
	 * This method is used to fetch the tasks by moduleCode and moduleItemId
	 * @param moduleCode - moduleCode of the module
	 * @param moduleItemId - moduleItemId of the module
	 * @param taskStatusCodes - list of task status codes
	 * @return List<Task> - list of Task
	 */
	public List<Task> fetchTasksByParams(Integer moduleCode, Integer awardId, List<String> taskStatusCodes);

	/**
	 * This method is used to fetch tasks by moduleCode and moduleItemKey
	 * @param moduleCode - moduleCode of the module
	 * @param moduleItemKey - moduleItemKey of the module
	 * @return List<Task> - list of Task
	 */
	public List<Task> fetchTasksByModuleCodeAndModuleItemKey(Integer moduleCode, String moduleItemKey);

	/**
	* This method is used to check the login person is in task.
	* @param personId - personId
	* @param taskStatusCode - taskStatusCode
	* @return count.
	*/
	public Integer checkPersonInTask(String personId, String taskStatusCode);

	/**
	 * This method is used to get section code based on task type code.
	 * @param taskTypeCode - taskTypeCode
	 * @return list section codes 
	 */
	public List<String> getSectionCodeBasedOnTaskTypeCode(String taskTypeCode);

	/**
	 * This method is used to fetch Task Assignee Based On Task Id.
	 * @param taskId - taskId of the task.
	 * @return typecode.
	 */
	public String fetchTaskTypeCodeByTaskId(Integer taskId);

	public List<Integer> getPersonTaskIds(String moduleItemkey, Integer moduleCode, String personId, String taskStatusCode);

	/**
	* This method is used to fetch details of task assignee history by actionLogId
	* @param actionLogId
	* @return List of task assignee person Id.
	*/
	public String fetchTaskAssigneeHistoryByActionLogId(Integer actionLogId);

	/**
	 * This method is used to fetch the count of editable sections already available in task by params
	 * @param personId
	 * @param taskId
	 * @return count of editable section.
	 */
	public Integer checkEditableSectionByTaskId(String personId, String taskId);

	/**
	 * This method is used to fetch the count of assigned tasks which are not completed for the current assignee by params
	 * @param personId
	 * @param taskStatusCode
	 * @param taskTypeCode
	 * @param moduleItemId
	 * @return count of assigned tasks which are not completed for the current assignee.
	 */
	public Integer checkCountOfAssignedTaskByParams(String personId, List<String> taskStatusCode, String taskTypeCode, String moduleItemId);

	/**
	 * This method is used to fetch the tasks by moduleCode and taskStatusCodes
	 * @param moduleCode - moduleCode of the module
	 * @param taskStatusCodes - list of task status codes
	 * @return List<Task> - list of Task
	 */
	public Integer checkTaskExisted(Integer moduleCode, String moduleItemId);
	
	/**
	 * This method check for task associated with moduleItemKey
	 * @param moduleCode
	 * @param moduleItemId
	 */
	public List<Task> fetchAllTasksByModuleCodeAndTaskStatusCodes(Integer moduleCode, List<String> taskStatusCodes);

	/**
	 * @param moduleItemId
	 * @param taskId
	 * @return Task
	 */
	public Task fetchTaskByTaskIdAndModuleItemId(String moduleItemKey, Integer taskId);

}
