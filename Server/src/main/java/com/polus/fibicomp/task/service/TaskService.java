package com.polus.fibicomp.task.service;

import java.util.Set;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.pojo.TaskActionLog;
import com.polus.fibicomp.task.vo.TaskVO;

@Transactional
@Service
public interface TaskService {

	/**
	 * This method is used to get all task types , task status 
	 * @return List of lookups
	 */
	public String getTaskLookUpData();

	/**
	 * This method is used to create task with list of attachments.
	 * @param files        - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @param taskVO - task details.
	 * @return a String of details of task data with list of attachments.
	 */
	public String saveOrUpdateTask(MultipartFile[] files, String formDataJSON, TaskVO taskVO);

	/**
	 * This method is used to fetch the details of task by id
	 * @param vo
	 * @return object of TaskVO as string
	 */
	public ResponseEntity<String> loadTaskDetailsById(TaskVO vo);

	/**
	 * This method is used to download task attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadTaskAttachment(Integer attachmentId);

	/**
	 * This method is used to delete attachment from task.
	 * @param vo - Object of TaskVO class. 
	 * @return a String of details of task with updated list of attachments.
	 */
	public String deleteTaskAttachment(TaskVO vo);

	/**
	 * This method is used to fetch the task comments of task by id
	 * @param vo
	 * @return object of TaskVO as string
	 */
	public String fetchTaskCommentsByTaskId(TaskVO vo);

	/**
	 * This method is used to save or update  task comments with list of attachments.
	 * @param files        - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @param taskVO - task details.
	 * @return a String of details of task comments data with list of attachments.
	 */
	public String saveOrUpdateTaskComment(MultipartFile[] files, String formDataJSON, TaskVO taskVO);

	/**
	 * This method is used to delete TaskComment based on taskcomment id.
	 * @param vo - TaskVO.
	 * @return success message.
	 */
	public String deleteTaskComment(TaskVO vo);

	/**
	 * This method is used to download task comment attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadTaskCommentAttachment(Integer attachmentId);

	/**
	 * This method is used to delete attachment from task comment.
	 * @param vo - Object of TaskVO class. 
	 * @return a String of details of task comment with updated list of attachments.
	 */
	public String deleteTaskCommentAttachment(TaskVO vo);

	/**
	 * This method is used to edit the task details.
	 * @param vo - Object of TaskVO class.
	 * @return A string of details of a task.
	 */
	public String startTask(TaskVO vo);

	/**
	 * This method is used to submit the task details.
	 * @param vo - Object of TaskVO class.
	 * @return A string of details of a task.
	 */
	public String completeTaskDetails(TaskVO vo);

	/**
	 * This method is canTakeRoutingAction
	 * @param taskVO
	 * @return
	 */
	public void canTaskTakeRoutingAction(TaskVO taskVO);

	/**
	 * This method is used to fetch the tasks by moduleCode and moduleItemKey
	 * @param taskVO - TaskVO
	 * @return String - object of TaskVO
	 */
	public String fetchTasksByParams(TaskVO taskVO);

	/**
	 * This method is used to reassign the task to a new assignee
	 * @param vo - object of TaskVO
	 * @return String - object of TaskVO
	 */
	public String reassignTask(TaskVO vo);

	/**
	 * This method is used for advanced search of tasks.
	 * @param vo - Object of TaskVO class.
	 * @return String - Object of TaskVO class.
	 */
	public String getAdvancedSearchTasks(TaskVO vo);

	/**
	 * This method is used to fetch the details of task history
	 * @param vo
	 * @return object of TaskVO as string
	 */
	public String fetchTaskHistory(TaskVO vo);

	/**
	 * This method is used to cancel the task details.
	 * @param vo - Object of TaskVO class.
	 * @return A string of details of a task.
	 */
	public String cancelTask(TaskVO vo);

	/**
	 * This method is used to load task update, create, assignee and submitted user full name.
	 * @param task- Task.
	 * @param response - void.
	 */
	public void loadTaskUserFullNames(Task task);

	/**
	 * This method is used to fetch the route log details of task by id
	 * @param vo
	 * @return object of TaskVO as string
	 */
	public String loadTaskWorkflowDetailsById(TaskVO vo);

	/**
	 * This method is used for sending notification for task
	 * @param vo
	 * @param notificationTypeId
	 * @param dynamicEmailrecipients
	 * @return taskVO object
	 */
	public TaskVO sendTaskNotification(TaskVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailrecipients);

	/**
	* This method is used to save the task action log details.
	* @param taskId - id of task
	* @param taskActionTypeCode - id of action type
	* @param systemComment - comment of task
	* @param updateUser
	* @return A string details of a taskActionLog.
	*/
	public TaskActionLog saveTaskActionLogDetails(Integer taskId, String taskActionTypeCode, String systemComment, String updateUser);

	/**
	* This method is used to save the task status history details.
	* @param taskId - id of task
	* @param taskActionTypeCode - id of action type
	* @param actionLogId - id of action log
	* @param updateUser
	*/
	public void saveTaskStatusHistory(Integer taskId, String taskActionTypeCode, Integer actionLogId, String updateUser);

	/**
	 * This method is used for the notification for task Assignee
	 * @param vo
	 * @return email success
	 */
	public String taskInvitation(TaskVO taskVO);

	/**
	 * This method is used to send notfication For FinanceAssignee 
	 * @param vo - Object of TaskVO class.
	 * @param notificationTypeCode
	 */
	public void sendNotficationForFinanceAssignee(TaskVO vo, Integer notificationTypeCode);

	/**
	 * This method is used for the fetch task count
	 * @param moduleItemKey
	 * @return task count
	 */
	public Integer fetchTaskCountByModuleItemKey(String moduleItemKey);

	/**
	 * This method is used to create task with list of attachments.
	 * @param taskVO - form data for the attachment.
	 * @return a String of details of task data with list of attachments.
	 */
	public String saveOrUpdateTaskForWAF(TaskVO taskVO);

	/**
	 * This method is used to save or update task comment with list of attachments.
	 * @param taskVO - form data for the attachment.
	 * @return a String of details of task data with list of attachments.
	 */
	public String saveOrUpdateTaskCommentForWAF(TaskVO taskVO);

}
