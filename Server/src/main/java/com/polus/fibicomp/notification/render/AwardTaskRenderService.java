package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.task.dao.TaskDao;
import com.polus.fibicomp.task.pojo.Task;
import com.polus.fibicomp.task.pojo.TaskType;

@Transactional
@Service
public class AwardTaskRenderService implements EmailRenderService {

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private TaskDao taskDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardRenderService awardRenderService;

	@Override
	public Map<String, String> getPlaceHolderData(String subModuleItemKey) {
		Task task = taskDao.fetchTaskByTaskId(Integer.parseInt(subModuleItemKey));
		Map<String, String> placeHolder = new HashMap<>();
		if (task.getModuleCode().equals(Constants.MODULE_CODE_AWARD)) {
			Award award = awardDao.fetchAwardByAwardId(task.getModuleItemId());
			Map<String, String> awardPlaceHolder = awardRenderService.getAwardPlaceHolder(award);
			placeHolder.putAll(awardPlaceHolder);
		}
		placeHolder.putAll(getTaskPlaceHolder(task));
		return placeHolder;
	}

	public Map<String, String> getTaskPlaceHolder(Task task) {
		String link = generateAwardTaskLinkToApplication(Integer.parseInt(task.getModuleItemId()), task.getTaskId());
		TaskType taskType = taskDao.fetchTaskTypeByTaskTypeCode(task.getTaskTypeCode());
		String awardTitle = awardDao.fetchAwardByAwardId(task.getModuleItemId()).getTitle();
		String assigneeName = personDao.getPersonFullNameByPersonId(task.getAssigneePersonId());
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{ASSIGNEE_NAME}", (assigneeName != null) ?assigneeName + "" : "");
		placeHolder.put("{TASK_NAME}", (taskType.getDescription() != null) ? taskType.getDescription() + "" : "");
		placeHolder.put("{DUE_DATE}", commonService.convertDateFormatBasedOnTimeZone(task.getDueDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
		placeHolder.put("{TASK_ID}", (task.getTaskId()!=null) ? task.getTaskId() + "": "");
		placeHolder.put("{USER_NAME}", personDao.getUserFullNameByUserName(task.getUpdateUser()));
		placeHolder.put("{AWARD_TITLE}", (awardTitle != null) ? awardTitle + "" : "");
		placeHolder.put("{APPLICATION_URL}", link);
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return String.valueOf(Constants.MODULE_CODE_AWARD);
	}

	@Override
	public String getSubModuleCode() {
		return Constants.AWARD_TASK_SUBMODULE_CODE.toString();
	}

	public String generateAwardTaskLinkToApplication(Integer awardId, Integer taskId) {
		return Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_AWARD_TASK_PATH + awardId + "ANDtaskId=" + taskId + Constants.APPLICATION_URL_END_TAG;
	}

}
