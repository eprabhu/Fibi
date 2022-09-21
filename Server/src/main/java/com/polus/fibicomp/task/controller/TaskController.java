package com.polus.fibicomp.task.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.task.service.TaskService;
import com.polus.fibicomp.task.vo.TaskVO;

import io.jsonwebtoken.Claims;

@RestController
public class TaskController {

	protected static Logger logger =  LogManager.getLogger(TaskController.class.getName());

	@Autowired
	@Qualifier(value = "taskService")
	private TaskService taskService;

	@Autowired
	private CommonService commonService;

	@GetMapping(value = "/getTaskLookUpData", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAllTaskType(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getTaskLookUpData");
		return taskService.getTaskLookUpData();
	}

	@PostMapping(value = "/saveOrUpdateTask", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateTask(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for saveOrUpdateTask");
		return taskService.saveOrUpdateTask(files, formDataJson, null);
	}

	@PostMapping(value = "/loadTaskDetailsById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<String> loadTaskDetailsById(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadTaskDetailsById");
		logger.info("taskId : {}", vo.getTaskId());
		return taskService.loadTaskDetailsById(vo);
	}

	@PostMapping(value = "/loadTaskWorkflowDetailsById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadTaskWorkflowDetailsById(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadTaskWorkflowDetailsById");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		vo.setPersonId(claims.get(Constants.LOGIN_PERSON_ID).toString());
		vo.setUpdateUser(claims.getSubject());
		vo.setUserName(claims.getSubject());
		logger.info("taskId : {}", vo.getTaskId());
		logger.info("personId : {}", vo.getPersonId());
		logger.info("updateUser : {}", vo.getUpdateUser());
		logger.info("userName : {}", vo.getUserName());
		return taskService.loadTaskWorkflowDetailsById(vo);
	}

	@GetMapping(value = "/downloadTaskAttachment")
	public ResponseEntity<byte[]> downloadTaskAttachment(HttpServletResponse response, @RequestHeader("attachmentId") Integer attachmentId) {
		logger.info("Requesting for downloadTaskAttachment");
		logger.info("attachmentId : {}", attachmentId);
		return taskService.downloadTaskAttachment(attachmentId);
	}

	@PostMapping(value = "/deleteTaskAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteTaskAttachment(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete Task Attachment");
		logger.info("attachmentId : {}", vo.getTaskAttachmentId());
		return taskService.deleteTaskAttachment(vo);
	}

	@PostMapping(value = "/fetchTaskCommentsByTaskId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchTaskCommentsByTaskId(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchTaskCommentsByTaskId");
		logger.info("taskId : {}", vo.getTaskId());
		return taskService.fetchTaskCommentsByTaskId(vo);
	}

	@PostMapping(value = "/saveOrUpdateTaskComment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateTaskComment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for saveOrUpdateTaskComment");
		return taskService.saveOrUpdateTaskComment(files, formDataJson, null);
	}

	@PostMapping(value = "/deleteTaskComment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteTaskComment(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete Task Comment");
		logger.info("taskCommentId : {}", vo.getTaskCommentId());
		return taskService.deleteTaskComment(vo);
	}

	@GetMapping(value = "/downloadTaskCommentAttachment")
	public ResponseEntity<byte[]> downloadTaskCommentAttachment(HttpServletResponse response, @RequestHeader("attachmentId") Integer attachmentId) {
		logger.info("Requesting for downloadTaskCommentAttachment");
		logger.info("attachmentId : {}", attachmentId);
		return taskService.downloadTaskCommentAttachment(attachmentId);
	}

	@PostMapping(value = "/deleteTaskCommentAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteTaskCommentAttachment(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete Task Comment Attachment");
		logger.info("taskId : {}", vo.getTaskId());
		logger.info("attachmentId : {}", vo.getTaskAttachmentId());
		return taskService.deleteTaskCommentAttachment(vo);
	}

	@PostMapping(value = "/startTask", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String startTask(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for startTask");
		return taskService.startTask(vo);
	}

	@PostMapping(value = "/completeTask", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String completeTaskDetails(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for completeTask");
		return taskService.completeTaskDetails(vo);
	}

	@PostMapping(value = "/fetchTasksByParams", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchTasksByParams(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchTasksByParams");
		logger.info("moduleItemId : {}", vo.getModuleItemId());
		logger.info("moduleCode : {}", vo.getModuleCode());
		logger.info("moduleItemKey : {}", vo.getModuleItemKey());
		return taskService.fetchTasksByParams(vo);
	}

	@PostMapping(value = "/reassignTask", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String reassignTask(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for reassignTask");
		return taskService.reassignTask(vo);
	}

	@PostMapping(value = "/advancedSearchForTask", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String advancedSearchForTask(@RequestBody TaskVO vo, HttpServletRequest request) {
		logger.info("Requesting for advancedSearchForTask");
		logger.info("moduleCode : {}", vo.getModuleCode());
		logger.info("moduleItemKey : {}", vo.getModuleItemKey());
		logger.info("assigneePersonId : {}", vo.getAssigneePersonId());
		logger.info("dueDateFrom : {}", vo.getDueDateFrom());
		logger.info("dueDateTo : {}", vo.getDueDateTo());
		return taskService.getAdvancedSearchTasks(vo);
	}

	@PostMapping(value = "/fetchTaskHistory", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchTaskHistory(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchTaskHistory");
		return taskService.fetchTaskHistory(vo);
	}

	@PostMapping(value = "/cancelTask", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String cancelTask(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for cancelTask");
		return taskService.cancelTask(vo);
	}

	@PostMapping(value = "/notifyTaskAssignee", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String grandInvitation(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for notify Task Assignee via email");
		return taskService.taskInvitation(vo);
	}

	@PostMapping(value = "/saveOrUpdateTaskForWAF", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateTaskForWAF(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateTaskForWAF");
		return taskService.saveOrUpdateTaskForWAF(vo);
	}

	@PostMapping(value = "/saveOrUpdateTaskCommentForWAF", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateTaskCommentForWAF(@RequestBody TaskVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateTaskForWAF");
		return taskService.saveOrUpdateTaskCommentForWAF(vo);
	}

}
