package com.polus.fibicomp.task.scheduler;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.task.dao.TaskDao;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.service.TaskService;
import com.polus.fibicomp.task.vo.TaskVO;

@Component
@Transactional
public class AwardTaskScheduler {
	protected static Logger logger = LogManager.getLogger(AwardTaskScheduler.class.getName());

	@Autowired
	private CommonService commonService;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private TaskService taskService;

	@Autowired
	private TaskDao taskDao;

	@Scheduled(cron = "${awardTask.remainder.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void sendRemainderEmailNotificationForTaskAssignee() {
		java.util.Date date = commonDao.getCurrentDate();
		logger.info("Remainder notification for task assignee");
		Timestamp currentTime = new Timestamp(date.getTime());
		List<String> taskStatusCodes = new ArrayList<>();
		taskStatusCodes.add(Constants.TASK_STATUS_CODE_COMPLETED);
		taskStatusCodes.add(Constants.TASK_STATUS_CODE_CANCELLED);
		List<Task> tasks = taskDao.fetchAllTasksByModuleCodeAndTaskStatusCodes(Constants.AWARD_MODULE_CODE, taskStatusCodes);
		if (tasks != null && !tasks.isEmpty()) {
			for (Task taskDetail : tasks) {
				if (taskDetail.getDueDate() != null) {
					long milliseconds = currentTime.getTime() - taskDetail.getDueDate().getTime();
					int seconds = (int) milliseconds / 1000;
					int hours = seconds / 3600;
					if (hours < 24 && hours > 0) {
						Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
						if (taskDetail.getAssigneePersonId() != null) {
							commonService.setNotificationRecipients(taskDetail.getAssigneePersonId(), Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
						}
						TaskVO taskVO = new TaskVO();
						taskVO.setTaskId(taskDetail.getTaskId());
						taskVO.setModuleCode(Constants.AWARD_MODULE_CODE);
						taskVO.setSubModuleCode(Constants.AWARD_TASK_SUBMODULE_CODE);
						taskVO.setModuleItemId(Integer.parseInt(taskDetail.getModuleItemId()));
						taskService.sendTaskNotification(taskVO, Constants.TASK_ASSIGNEE_SCHEDULER_REMAINDER_NOTIFICATION_CODE, dynamicEmailRecipients);
					}
				}
			}
		}
	}

}
